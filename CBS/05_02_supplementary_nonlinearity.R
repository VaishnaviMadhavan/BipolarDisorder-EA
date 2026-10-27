
#
# Script: 05_02_supplementary_nonlinearity.R
# Project: Bipolar Disorder and Education:
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Script for creating plots to check if there are non-linear patterns
#                   in the association between bipolar disorder PGS and academic performance
#
# Author: Vaishnavi K Madhavan
# Start date: 27-07-2026
# Last updated: 27-07-2026
# 


# 1. Set Up ####

# popdata <- fread("./Data/03_Final_Data/20260716_popdata_detailedonly.csv")
# sibdata <- fread("./Data/03_Final_Data/20260716_sibdata_detailedonly.csv")


# 2. Define Variables ####

scores <- c("std_CitoStandaardScore", "CitoZScoreRekenenWiskunde", "CitoZScoreStudievaardigh", "CitoZScoreTaal")

## labels for plotting
label <- c(
  std_CitoStandaardScore = "Age 12 Total Score",
  CitoZScoreRekenenWiskunde = "Age 12 Mathematics",
  CitoZScoreStudievaardigh = "Age 12 Reading",
  CitoZScoreTaal = "Age 12 Language"
)


# 3. Create Plots ####

make_binned_plot <- function(data, score) {
  
  plot_data <- data %>%
    filter(
      !is.na(.data[[score]]),
      !is.na(detailed_bipolar_diag)
   ) %>%
    mutate(
      bin = cut(
        .data[[score]],
      breaks = seq(-4,4,0.25)
    )
  ) %>%
    group_by(bin) %>%
    summarise(
      mean_score = mean(.data[[score]], na.rm = TRUE),
      diagnosis = mean(detailed_bipolar_diag) * 100,
      n = n(),
      .groups = "drop"
    ) %>%
    filter(n>50)
  
  
  ggplot(
    plot_data,
    aes(
      x = mean_score, 
      y = diagnosis
    )
  ) +
    geom_point() + 
    geom_line () + 
    geom_smooth(
      method="loess", 
      se=FALSE
    ) + 
    scale_x_continuous(
      limits = c(-3, 3), 
      breaks = seq(-3, 3, 1)
    ) + 
    scale_y_continuous(
      limits = c(0, 0.2),
      breaks = seq(0, 0.2, 0.05)
    ) + 
    labs(
      title = NULL,
      x = "Academic Performance Score (SD)",
      y = "Bipolar Disorder (%)"
    ) + 
    theme_classic() + 
    theme(
      plot.title = element_text(
        hjust = 0.5, 
        face = "bold"
      ))
}


## make spline plots
make_cox_spline_plot <- function(
    data,
    predictor, 
    time_var = "time", 
    outcome = "bipolar_diag", 
    covariates = NULL, 
    df_spline = 4, 
    x_range = c(-3, 3)
) {
  
  
  # build formula 
  spline_term <- paste0(
    "ns(", predictor, ", df = ", df_spline, ")"
  )
  
  all_terms <- c(spline_term, covariates)
  
  formula_str <- paste0(
    "Surv(", time_var, ", ", outcome, ") ~ ",
    paste(all_terms, collapse = " + ")
  )
  
  model_formula <- as.formula(formula_str)
  
  # complete cases
  vars_needed <- unique(c(
    predictor,
    time_var,
    outcome, 
    covariates
  ))
  
  plot_data <- data %>%
    dplyr::filter(
      complete.cases(
        dplyr::across(dplyr::all_of(vars_needed))
      )
    )
  
  # cox spline model
  model <- survival::coxph(
    model_formula, 
    data = plot_data,
    x = TRUE
  )
  
  # prediction grid
  score_grid <- seq(
    x_range[1],
    x_range[2],
    length.out = 200
  )
  
  newdata <- plot_data[
    rep(1, length(score_grid)),
    ,
    drop = FALSE
  ]
  
  newdata[[predictor]] <- score_grid
  
  # reference = 0 SD
  ref_data <- newdata[1, , drop = FALSE]
  ref_data[[predictor]] <- 0
  
  # model matrices
  model_terms <- delete.response(
    terms(model)
  )
  
  X <- model.matrix(
    model_terms,
    data = newdata
  )
  
  X_ref <- model.matrix(
    model_terms,
    data = ref_data
  )
  
  # make sure reference matrix has only one row
  X_ref <- X_ref[1, , drop = FALSE]
  
  # HR relative to 0 SD
  beta <- coef(model)
  
  V <- vcov(model)
  
  # difference between each score and reference score
  contrast <- sweep(
    X, 
    2, 
    X_ref[1, ],
    "-"
  )
  
  # remove any columns that do not correspond to coefficients
  contrast <- contrast[, names(beta), drop = FALSE]
  
  # log HR
  logHR <- as.vector(
    contrast %*% beta
  )
  
  # standard error 
  se <- sqrt(
    rowSums(
      (contrast %*% V) * contrast
    )
  )
  
  plot_df <- data.frame(
    score = score_grid,
    HR = exp(logHR),
    CI_lower = exp(logHR - 1.96 * se),
    CI_upper = exp(logHR + 1.96 * se)
  )
  
  # plot
  ggplot(
    plot_df,
    aes(
      x = score,
      y = HR
    )
  ) +
    
    geom_ribbon(
      aes(
        ymin = CI_lower,
        ymax = CI_upper
      ),
      alpha = 0.2
    ) + 
    
    geom_line(
      linewidth = 0.9
    ) + 
    
    geom_hline(
      yintercept = 1,
      linetype = "dashed"
    ) + 
    
    geom_vline(
      xintercept = 0, 
      linetype = "dotted"
    ) + 
    
    scale_x_continuous(
      limits = x_range,
      breaks = seq(
        x_range[1],
        x_range[2],
        by = 1
      )
    ) + 
    
    labs(
      x = "Standardised score (SD)", 
      y = "Hazard Ratio (HR)"
    ) + 
    
    theme_classic()
  
}


# 4. Generate All Plots ####

bin_plots <- list()
spline_plots <- list()

for(score in scores) {
  
  name <- paste0("Age12_", score)
  n <- sum(!is.na(popdata[[score]]))
  
  bin_plots[[name]] <-
    make_binned_plot(popdata, score) +
    ggtitle(label = paste0(label[score], " (n = ", n, ")"))
  
  spline_plots[[name]] <-
    make_cox_spline_plot(
      data = popdata,
      predictor = score,
      time_var = "detailed_time", 
      outcome = "detailed_bipolar_diag",
      covariates = c("GBAGESLACHT", "GBAGEBOORTEJAAR", "birthorder"),
      df_spline = 4,
      x_range = c(-3, 3)
    ) + 
    
    labs(
      title = paste0(label[score], " (n = ", n, ")")
    )
}


bin_panel <- wrap_plots(
  bin_plots,
  ncol=4
)

spline_grid <- wrap_plots(
  spline_plots,
  ncol = 4
)


bin_panel
sd_panel
spline_grid


# 5. Save ####
ggsave(
  "./Results/Supplementary_Results/nonlinearity_binned.pdf",
  bin_panel, 
  width = 14,
  height = 4
)

ggsave(
  "./Results/Supplementary_Results/nonlinearity_binned.jpg",
  bin_panel, 
  width = 14,
  height = 4
)

ggsave(
  "./Results/Supplementary_Results/nonlinearity_SplinePlots.pdf",
  spline_grid, 
  width = 14,
  height = 4
)

ggsave(
  "./Results/Supplementary_Results/nonlinearity_SplinePlots.jpg",
  spline_grid, 
  width = 14,
  height = 4
)


# 6. Spline Cox Model ####

## initialize list to store results
all_results <- list()

# define predictor variable 
tests <- c("CitoZScoreTaal", "CitoZScoreRekenenWiskunde", "CitoZScoreStudievaardigh", "std_CitoStandaardScore")

## loop through each grade
for (test in tests) {
  
  test_label <- ifelse(test == "std_CitoStandaardScore", "Total Score",
                       ifelse(test == "CitoZScoreRekenenWiskunde", "Math", 
                              ifelse(test == "CitoZScoreStudievaardigh", "Reading", 
                                     ifelse(test == "CitoZScoreTaal", "Language", NA))))
  
  # define within/between variables
  bw_vars <- c(paste0(test, "_between"), paste0(test, "_within"))
  
  # run population model on popdata
  detailed_pop <- compare_cox_nonlinearity(
    cohort = "CBS",
    grade = "Grade 7",
    test = test_label, 
    data = popdata,
    outcome = "detailed_bipolar_diag",
    predictor = test,
    time_var = "detailed_time",
    covariates = c("GBAGESLACHT", "GBAGEBOORTEJAAR", "birthorder"),
    cluster_var = "FID",
    analysis = "Main Model",
    is_sibdata = FALSE
  )
  
  
  # combine all results for this grade
  all_results[[test]] <- bind_rows(detailed_pop)
}

## combine across all grades
nonlinear_results <- bind_rows(all_results)

nonlinear_results <- nonlinear_results %>%
  mutate(Significance = p_value < 0.05)

significance <- nonlinear_results %>%
  filter(Significance == TRUE)

write.csv(nonlinear_results, "./Results/Supplementary_Results/nonlinearity_splinecox.csv")

