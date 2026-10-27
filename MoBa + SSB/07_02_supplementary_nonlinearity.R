
# 
# Script: 07_02_supplementary_nonlinearity_check.R 
# Project: Bipolar Disorder and Education: 
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Creating plots to check if there are non-linear patterns
#                   in the association between bipolar disorder PGS and academic performance 
#
# Author: Vaishnavi K Madhavan
# Start date: 24-07-2026
# 

# popdata <- fread("./Data/Final Data/academicperformance_diagnosis_popdata.csv")
# sibdata <- fread("./Data/Final Data/academicperformance_diagnosis_sibdata.csv")


# 1. Define Variables ####
scores <- list(
  
  Grade5 = c(
    "std_score_agg5",
    "std_score_NPREG05",
    "std_score_NPLES05",
    "std_score_NPENG05"
  ),
  
  Grade8 = c(
    "std_score_agg8",
    "std_score_NPREG08",
    "std_score_NPLES08",
    "std_score_NPENG08"
  ),
  
  Grade9 = c(
    "std_score_agg9",
    "std_score_NPREG09",
    "std_score_NPLES09"
  )
)


## labels for plotting
label <- c(
  std_score_agg5        = "Age 10 Total Score",
  std_score_NPREG05     = "Age 10 Mathematics",
  std_score_NPLES05     = "Age 10 Reading",
  std_score_NPENG05     = "Age 10 English",
  
  std_score_agg8        = "Age 13 Total Score",
  std_score_NPREG08     = "Age 13 Mathematics",
  std_score_NPLES08     = "Age 13 Reading",
  std_score_NPENG08     = "Age 13 English",
  
  std_score_agg9        = "Age 14 Total Score",
  std_score_NPREG09     = "Age 14 Mathematics",
  std_score_NPLES09     = "Age 14 Reading"
)


# 2. Functions for Plots ####

## GEOSS curve
make_gam_plot <- function(data, score){
  
  ggplot(
    data,
    aes(
      x = .data[[score]],
      y = bipolar_diag
    )
  ) +
    
    geom_smooth(
      method = "gam",
      formula = y ~ s(x, bs = "cs"),
      se = TRUE,
      colour = "#2166AC",
      linewidth = 1.1
    ) +
    
    scale_x_continuous(
      limits = c(-3, 3),
      breaks = seq(-3, 3, 1)
    ) +
    
    scale_y_continuous(
      labels = scales::percent_format(),
      expand = expansion(mult = c(0, 0.05))
    ) +
    
    labs(
      title = labels[score],
      x = "Academic Performance Score (SD)",
      y = "Estimated probability of bipolar disorder"
    ) +
    
    theme_classic() +
    
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      )
    )
  
}


## binned plot
make_binned_plot <- function(data, score){
  
  plot_data <- data %>%
    filter(
      !is.na(.data[[score]]),
      !is.na(bipolar_diag)
    ) %>%
    mutate(
      bin = cut(
        .data[[score]],
        breaks = seq(-4,4,0.25)
      )
    ) %>%
    group_by(bin) %>%
    summarise(
      mean_score = mean(.data[[score]], na.rm=TRUE),
      diagnosis = mean(bipolar_diag)*100,
      n=n(),
      .groups="drop"
    ) %>%
    filter(n>=50)
  
  
  ggplot(
    plot_data,
    aes(
      x=mean_score,
      y=diagnosis
    )
  ) +
    geom_point() +
    geom_line() +
    geom_smooth(
      method="loess",
      se=FALSE
    ) +
    scale_x_continuous(
      limits = c(-3, 3),
      breaks = seq(-3, 3, 1)
    ) +
    scale_y_continuous(
      limits = c(0, 1.5),
      breaks = seq(0, 2, 0.5)
    ) +
    labs(
      title = label[score],
      x = "Academic Performance Score (SD)",
      y = "Bipolar disorder (%)"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      )
    )
  
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
  

  # Build formula
  spline_term <- paste0(
    "ns(", predictor, ", df = ", df_spline, ")"
  )
  
  all_terms <- c(spline_term, covariates)
  
  formula_str <- paste0(
    "Surv(", time_var, ", ", outcome, ") ~ ",
    paste(all_terms, collapse = " + ")
  )
  
  model_formula <- as.formula(formula_str)
  
  
  # Complete cases
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
  
  
  # Cox spline model
  model <- survival::coxph(
    model_formula,
    data = plot_data,
    x = TRUE
  )
  
  
  # Prediction grid
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
  
  
  # Reference = 0 SD
  ref_data <- newdata[1, , drop = FALSE]
  ref_data[[predictor]] <- 0
  
  

  # Model matrices
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
  
  # Make sure reference matrix has only one row
  X_ref <- X_ref[1, , drop = FALSE]
  
  
  # HR relative to 0 SD
  beta <- coef(model)
  
  V <- vcov(model)
  
  # Difference between each score and reference score
  contrast <- sweep(
    X,
    2,
    X_ref[1, ],
    "-"
  )
  
  # Remove any columns that do not correspond to coefficients
  contrast <- contrast[, names(beta), drop = FALSE]
  
  # Log HR
  logHR <- as.vector(
    contrast %*% beta
  )
  
  # Standard error
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
  
    # Plot
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
      y = "Hazard ratio (HR)"
    ) +
    
    theme_classic()
}


# 3. Generate All Plots ####

gam_plots <- list()
bin_plots <- list()
spline_plots <- list()

for(grade in names(scores)){
  
  for(score in scores[[grade]]){
    
    name <- paste0(grade,"_",score)
    
    gam_plots[[name]] <- 
      make_gam_plot(popdata, score) +
      ggtitle(label[score])
    
    bin_plots[[name]] <-
      make_binned_plot(popdata, score) +
      ggtitle(label[score])
    
    spline_plots[[plot_name]] <-
      make_cox_spline_plot(
        data = popdata,
        predictor = score_var,
        time_var = "time",
        outcome = "bipolar_diag",
        covariates = c("sex", "birthyear", "birthorder"),
        df_spline = 4,
        x_range = c(-3, 3)
      ) +
      
      labs(
        title = label[score_var]
      )
    
  }
}

## create panels
gam_panel <- wrap_plots(
  gam_plots,
  ncol=4)

bin_panel <- wrap_plots(
  bin_plots,
  ncol=4)

spline_grid <- wrap_plots(
    spline_plots,
    ncol=4)

gam_panel
bin_panel
spline_grid


# 4. Save Plots ####

ggsave(
  "./Results/sensitivity_checks/nonlinearity_GAM.pdf",
  gam_panel,
  width=14,
  height=10)

ggsave(
  "./Results/sensitivity_checks/nonlinearity_binned.pdf",
  bin_panel,
  width=14,
  height=10)

ggsave(
  "./Results/sensitivity_checks/nonlinearity_SplinePlots.pdf",
  spline_grid,
  width=14,
  height=10)


# 5. Spline Cox Model ####

#* 5.1. Total Score ----

## looping through all grades
grades <- c("5", "8", "9")

## initialize list to store results
all_results <- list()

## loop through each grade
for (grade in grades) {
  
  # define predictor variable 
  score_var <- paste0("std_score_agg", grade)
  
  # define within/between variables
  bw_vars <- c(paste0(score_var, "_between"), paste0(score_var, "_within"))
  
  # run population model on popdata
  main_pop <- compare_cox_nonlinearity(
    cohort = "SSB",
    grade = paste0("Grade ", grade),
    test = "Total Score",
    data = popdata,
    outcome = "bipolar_diag",
    predictor = score_var,
    time_var = "time",
    covariates = c("sex", "birthyear", "birthorder"),
    cluster_var = "parents",
    analysis = "Main Model",
    is_sibdata = FALSE
  )
  
  # combine all results for this grade
  all_results[[grade]] <- bind_rows(main_pop, main_bw)
}

## combine across all grades
total_score <- bind_rows(all_results)


#* 5.2. Individual Test Scores ----

## prepping objects to loop over
## list of test variables grouped by grade
test_by_grade <- list(
  "5" = c("std_score_NPREG05", "std_score_NPLES05", "std_score_NPENG05"),
  "8" = c("std_score_NPREG08", "std_score_NPLES08", "std_score_NPENG08"),
  "9" = c("std_score_NPREG09", "std_score_NPLES09")
)

score_vars <- unlist(test_by_grade, use.names = FALSE)

## initialize results list
all_results <- list()

## loop through each grade
for (grade in names(test_by_grade)) {
  
  ## get the test variables for that grade
  test_vars <- test_by_grade[[grade]]
  
  ## loop through each test variable
  for (score_var in test_vars) {
    
    # extract test and grade from score_var
    matches <- stringr::str_match(score_var, "^std_score_([A-Z]+)(\\d+)$")
    test <- matches[, 2]
    grade <- matches[, 3]
    
    # reconstruct base name
    base_name <- paste0("std_score_", test, grade)
    
    # create between and within variable names
    bw_vars <- c(
      paste0(base_name, "_between"),
      paste0(base_name, "_within")
    )
    
    # extract subject name (e.g., "Math", "Reading", "English") from variable
    test_type <- case_when(
      grepl("NPREG", score_var) ~ "Math",
      grepl("NPLES", score_var) ~ "Reading",
      grepl("NPENG", score_var) ~ "English",
      TRUE ~ "Unknown"
    )
    
    # run population model on popdata
    main_pop <- compare_cox_nonlinearity(
      cohort = "SSB",
      grade = paste0("Grade ", as.numeric(grade)),
      test = test_type,
      data = popdata,
      outcome = "bipolar_diag",
      predictor = score_var,
      time_var = "time",
      covariates = c("sex", "birthyear", "birthorder"),
      cluster_var = "parents",
      analysis = "Main Model",
      is_sibdata = FALSE
    )
    
    # store all models for this score in a list
    key <- paste0(score_var)
    all_results[[key]] <- bind_rows(main_pop, main_bw)
  }
}

## combine results across all test variables and grades
test_scores <- bind_rows(all_results)

## combine all results 
nonlinear_results <- bind_rows(total_score, 
                               test_scores)

nonlinear_results <- nonlinear_results %>%
  mutate(Significance = p_value < 0.05)

signficance <- nonlinear_results %>%
  filter(Significance == TRUE)


write.csv(nonlinear_results, "./Results/sensitivity_checks/nonlinearity_splinecox.csv", row.names = F)

