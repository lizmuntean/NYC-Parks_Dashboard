preprocess_data <- function(df_all) {

  # Clean column names and create year
  df_all <- df_all %>%
    clean_names() %>%
    mutate(
      date = as.Date(date, format = "%m/%d/%Y"),
      year = as.numeric(as.character(year(date)))
    )
  
  df <- df_all %>%
    filter(!grepl("Pre", age_yr, ignore.case = TRUE))
  
  # Standardize Spartina column names if needed
  df <- df %>%
    rename(
      avg_spal_h_cm = any_of("avg_spal_h"),
      avg_spal_d_mm = any_of("avg_spal_d")
    )
  
  # Remove structural zeros from average height/diameter
  # (only if all required columns exist)
  if (all(c("spal_stem_count",
            "avg_spal_h_cm",
            "avg_spal_d_mm") %in% names(df))) {
    
    df <- df %>%
      mutate(
        avg_spal_h_cm = if_else(
          spal_stem_count == 0,
          NA_real_,
          avg_spal_h_cm
        ),
        avg_spal_d_mm = if_else(
          spal_stem_count == 0,
          NA_real_,
          avg_spal_d_mm
        )
      )
  }
  
  # Create marsh if it doesn't already exist
  if (!"marsh" %in% names(df)) {
    
    df <- df %>%
      mutate(
        marsh = case_when(
          grepl("^HM", area, ignore.case = TRUE) ~ "HM",
          grepl("^LM", area, ignore.case = TRUE) ~ "LM",
          TRUE ~ NA_character_
        )
      )
  }
  
  # Create treatments if it doesn't already exist (see get_treatment function below)
  df <- df %>%
    mutate(
      treatment = get_treatment(.)
    )
  
  # Keep only High and Low Marsh observations
  df <- df %>%
    filter(marsh %in% c("HM", "LM"))
  
  # Set Reference as the baseline treatment
  df <- df %>%
    mutate(
      treatment = factor(
        treatment,
        levels = c("Reference", "Restored", "Cluster")
      )
    )
  
  #create unique plot ID column 
  df$plotID <- interaction(df$treatment, df$plot)
  
  # Move commonly used columns to the front
  df <- df %>%
    relocate(marsh, treatment, year, plotID)
  
  return(df)
}



get_treatment <- function(df) {
  
  if ("treatment" %in% names(df) &&
      any(!is.na(df$treatment))) {
    return(as.character(df$treatment))
  }
  
  if ("rest_ref" %in% names(df) &&
      any(!is.na(df$rest_ref))) {
    return(as.character(df$rest_ref))
  }
  
  for (col in c("area_name", "site_name", "area")) {
    
    if (col %in% names(df)) {
      
      x <- df[[col]]
      
      trt <- case_when(
        grepl("Reference", x, ignore.case = TRUE) ~ "Reference",
        grepl("Restored", x, ignore.case = TRUE) ~ "Restored",
        grepl("Cluster", x, ignore.case = TRUE) ~ "Cluster",
        TRUE ~ NA_character_
      )
      
      if (any(!is.na(trt)))
        return(trt)
    }
  }
  
  rep(NA_character_, nrow(df))
}



run_annual_analysis <- function(data, response, groups, marsh_type, season_year){
  
  print("Entering single year analysis")
  
  #filter data
  df <- data %>%
    filter(treatment %in% groups, year == season_year, marsh %in% marsh_type) %>%
    droplevels()
  
  # Check that at least two treatment groups remain
  if (nlevels(df$treatment) < 2) {
    stop("Need at least two treatment groups.")
  }
  
  #Build the model
  form <- as.formula(
    paste(response, "~ treatment")
  )
  model <-lm(form, data = df)
  
  # Statistical results
  anova_tbl <- broom::tidy(anova(model)) %>%
    mutate(
      statistic = round(statistic, 2),
      p.value = ifelse(
        p.value < 0.001,
        "<0.001",
        sprintf("%.3f", p.value)
      ),
      test = "F test"
    )
  
  shapiro_tbl <- shapiro.test(residuals(model))
  
  levene_tbl <- leveneTest(form, data = df)
  
  emm_tbl <- emmeans(model, pairwise ~ treatment)
  
  # Boxplot
  boxplot <- ggplot(
    df,
    aes(
      x = treatment,
      y = .data[[response]],
      fill = treatment
    )
  ) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.6) +
    labs(
      title = paste(response, "-", season_year),
      x = "Treatment",
      y = response
    ) +
    theme_bw() +
    theme(legend.position = "none")
  
  # QQ plot
  qqplot <- ggplot(
    data.frame(sample = residuals(model)),
    aes(sample = sample)
  ) +
    stat_qq() +
    stat_qq_line(colour = "red") +
    labs(title = "Normal Q-Q Plot") +
    theme_bw()
  
  # Residual plot
  residplot <- ggplot(
    data.frame(
      fitted = fitted(model),
      residuals = residuals(model)
    ),
    aes(fitted, residuals)
  ) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = 2) +
    labs(
      title = "Residuals vs Fitted",
      x = "Fitted values",
      y = "Residuals"
    ) +
    theme_bw()
  
  # Return everything
  return(list(
    model = model,
    anova = anova_tbl,
    shapiro = shapiro_tbl,
    levene = levene_tbl,
    emmeans = emm_tbl,
    boxplot = boxplot,
    qqplot = qqplot,
    residplot = residplot
  ))
  
}



run_time_analysis <- function(data, response, marsh_type, groups){
  
  print("Entering time analysis")
  
  #filter data
  df <- data %>%
    filter(treatment %in% groups, marsh %in% marsh_type) %>%
    droplevels()
  
  # Check that at least two treatment groups remain
  if (nlevels(df$treatment) < 2) {
    stop("Need at least two treatment groups.")
  }
  
  df$plotID <- interaction(df$treatment, df$plot)
  df$year <- factor(df$year)
  
  #Build the model
  form <- as.formula(
    paste(response, "~ treatment * year + (1|plotID)")
  )
  model <-lmer(form, data = df)
  
  # Statistical results
  anova_tbl <- anova(model) |>
    as.data.frame() |>
    tibble::rownames_to_column("term") |>
    rename(
      statistic = `F value`,
      p.value = `Pr(>F)`
    ) |>
    mutate(
      statistic = round(statistic, 2),
      p.value = ifelse(
        p.value < 0.001,
        "<0.001",
        sprintf("%.3f", p.value)
      ),
      test = "F test"
    )
  print(anova_tbl)
  
  shapiro_tbl <- shapiro.test(residuals(model))
  
  emm_tbl <- emmeans(model, pairwise ~ treatment | year)
  
  # Boxplot
  boxplot <- ggplot(
    df,
    aes(
      x = treatment,
      y = .data[[response]],
      fill = treatment
    )
  ) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.6) +
    labs(
      title = paste(response, "across years"),
      x = "Treatment",
      y = response
    ) +
    theme_bw() +
    theme(legend.position = "none")
  
  # QQ plot
  qqplot <- ggplot(
    data.frame(sample = residuals(model)),
    aes(sample = sample)
  ) +
    stat_qq() +
    stat_qq_line(colour = "red") +
    labs(title = "Normal Q-Q Plot") +
    theme_bw()
  
  # Residual plot
  residplot <- ggplot(
    data.frame(
      fitted = fitted(model),
      residuals = residuals(model)
    ),
    aes(fitted, residuals)
  ) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = 2) +
    labs(
      title = "Residuals vs Fitted",
      x = "Fitted values",
      y = "Residuals"
    ) +
    theme_bw()
  
  # Return everything
  return(list(
    model = model,
    anova = anova_tbl,
    shapiro = shapiro_tbl,
    emmeans = emm_tbl,
    boxplot = boxplot,
    qqplot = qqplot,
    residplot = residplot
  ))
  
}



compare_transformations <- function(data, response, marsh_type, groups){
  
  # Filter data
  df <- data %>%
    filter(area %in% groups, marsh %in% marsh_type) %>%
    mutate(year = factor(lubridate::year(date)))
  
  # List of transformations
  transforms <- list(
    Original = identity,
    Log = function(x) log(x + 1),
    Sqrt = sqrt
  )
  
  # Store results
  results <- list()
  
  # Set up plotting window
  par(mfrow = c(3, 2))
  
  for(tr in names(transforms)){
    
    cat("\n========================\n")
    cat(response, "-", tr, "\n")
    cat("========================\n")
    
    # Apply transformation
    df$response <- transforms[[tr]](df[[response]])
    
    # Fit model
    model <- lmer(response ~ area * year + (1|plot), data = df)
    
    # Assumption tests
    shap <- shapiro.test(residuals(model))
    
    lev <- leveneTest(
      response ~ interaction(area, year),
      data = df
    )
    
    # Print summary
    cat("\nShapiro-Wilk p =", round(shap$p.value,4), "\n")
    cat("Levene p =", round(lev$`Pr(>F)`[1],4), "\n\n")
    
    # QQ plot
    qqnorm(residuals(model),
           main = paste(response, "-", tr))
    qqline(residuals(model), col = "red")
    
    # Residual plot
    plot(fitted(model),
         residuals(model),
         main = paste(response, "-", tr),
         xlab = "Fitted",
         ylab = "Residuals")
    abline(h = 0, lty = 2)
    
    # Save everything
    results[[tr]] <- list(
      model = model,
      shapiro = shap,
      levene = lev
    )
  }
  
  return(results)
}



run_annual_glm <- function(data, response, groups, marsh_type, season_year){
  
  print("Entering single year GLM")
  
  #filter data
  df <- data %>%
    filter(treatment %in% groups, year == season_year, marsh %in% marsh_type) %>%
    droplevels()
  
  if (nlevels(df$treatment) < 2) {
    stop("Need at least two treatment groups to fit the model.")
  }
  
  nonzero_by_group <- tapply(
    df[[response]] > 0,
    df$treatment,
    sum,
    na.rm = TRUE
  )
  
  if (any(nonzero_by_group < 3)) {
    
    msg <- paste0(
      response,
      ": analysis not run. At least 3 non-zero observations are required per treatment group."
    )
    
    message(msg)   # prints to R console
    
    return(
      list(
        error = msg
      )
    )
  }
  
  #Build the model
  form <- as.formula(
    paste(response, "~ treatment")
  )
  model <- MASS::glm.nb(form, data = df) #can change to poisson if variance is equal to the mean, unlikely in this situation
  
  # Statistical results
  anova_tbl <- broom::tidy(car::Anova(model, type = "III")) %>%
    mutate(
      statistic = round(statistic, 2),
      p.value = ifelse(
        p.value < 0.001,
        "<0.001",
        sprintf("%.3f", p.value)
      ),
      test = "Likelihood ratio"
    )
  
  contrast_tbl <- emmeans(model, pairwise ~ treatment, type = "response")
  
  # Boxplot
  boxplot <- ggplot(
    df,
    aes(
      x = treatment,
      y = .data[[response]],
      fill = treatment
    )
  ) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.6) +
    labs(
      title = paste(response, "-", season_year),
      x = "Treatment",
      y = response
    ) +
    theme_bw() +
    theme(legend.position = "none")
  
  # QQ plot
  qqplot <- ggplot(
    data.frame(sample = residuals(model)),
    aes(sample = sample)
  ) +
    stat_qq() +
    stat_qq_line(colour = "red") +
    labs(title = "Normal Q-Q Plot") +
    theme_bw()
  
  # Residual plot
  residplot <- ggplot(
    data.frame(
      fitted = fitted(model),
      residuals = residuals(model)
    ),
    aes(fitted, residuals)
  ) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = 2) +
    labs(
      title = "Residuals vs Fitted",
      x = "Fitted values",
      y = "Residuals"
    ) +
    theme_bw()
  
  # Return everything
  return(list(
    model = model,
    anova = anova_tbl,
    emmeans = contrast_tbl,
    boxplot = boxplot,
    qqplot = qqplot,
    residplot = residplot
  ))
  
}



run_time_glm <- function(data, response, marsh_type, groups){
  
  library(glmmTMB)
  
  #filter data
  df <- data %>%
    filter(treatment %in% groups, marsh %in% marsh_type) %>%
    droplevels()
  
  if (nlevels(df$treatment) < 2) {
    stop("Need at least two treatment groups to fit the model.")
  }
  
  nonzero_by_group <- tapply(
    df[[response]] > 0,
    df$treatment,
    sum,
    na.rm = TRUE
  )
  
  if (any(nonzero_by_group < 3)) {
    
    msg <- paste0(
      response,
      ": analysis not run. At least 3 non-zero observations are required per treatment group."
    )
    
    message(msg)   # prints to R console
    
    return(
      list(
        error = msg
      )
    )
  }

  df$year <- factor(df$year)
  
  #Build the model
  form <- as.formula(
    paste(response, "~ treatment * year + (1|plotID)")
  )
  model <- glmmTMB(form, data = df, family = nbinom2) #can change to poisson if variance is equal to the mean, unlikely in this situation
  
  # Check for near-zero random effect variance (singular-like fit)
  singular <- FALSE
  
  vc <- VarCorr(model)
  
  if("plotID" %in% names(vc$cond)){
    
    plot_variance <- as.numeric(vc$cond$plotID[1])
    
    if(plot_variance < 1e-8){
      singular <- TRUE
    }
  }
  # Statistical results
  anova_tbl <- broom::tidy(car::Anova(model, type = "III")) %>%
    mutate(
      statistic = round(statistic, 2),
      p.value = ifelse(
        p.value < 0.001,
        "<0.001",
        sprintf("%.3f", p.value)
      ),
      test = "Likelihood ratio"
    )
  
  contrast_tbl <- emmeans(model, pairwise ~ treatment | year, type = "response")
  
  # Boxplot
  boxplot <- ggplot(
    df,
    aes(
      x = treatment,
      y = .data[[response]],
      fill = treatment
    )
  ) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.6) +
    labs(
      title = paste(response, "-", "across years"),
      x = "Treatment",
      y = response
    ) +
    theme_bw() +
    theme(legend.position = "none")
  
  # QQ plot
  qqplot <- ggplot(
    data.frame(sample = residuals(model)),
    aes(sample = sample)
  ) +
    stat_qq() +
    stat_qq_line(colour = "red") +
    labs(title = "Normal Q-Q Plot") +
    theme_bw()
  
  # Residual plot
  residplot <- ggplot(
    data.frame(
      fitted = fitted(model),
      residuals = residuals(model)
    ),
    aes(fitted, residuals)
  ) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = 2) +
    labs(
      title = "Residuals vs Fitted",
      x = "Fitted values",
      y = "Residuals"
    ) +
    theme_bw()
  
  # Return everything
  return(list(
    model = model,
    anova = anova_tbl,
    singular = singular,
    emmeans = contrast_tbl,
    boxplot = boxplot,
    qqplot = qqplot,
    residplot = residplot
  ))
}



plot_average_counts <- function(data, response, show_means = TRUE){
  
  # Create combined marsh-treatment grouping
  data <- data %>%
    mutate(
      marsh_treatment = interaction(
        marsh,
        treatment,
        sep = " - "
      )
    )
  
  # Calculate means and standard errors
  summary_data <- data %>%
    group_by(year, marsh_treatment) %>%
    summarise(
      mean_value = mean(.data[[response]], na.rm = TRUE),
      se = sd(.data[[response]], na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(
      label_y = mean_value + se + (0.01 * max(mean_value, na.rm = TRUE))
    )
  
  
 p <- ggplot(
    summary_data,
    aes(
      x = marsh_treatment,
      y = mean_value,
      fill = factor(year)
    )
  ) +
    
    geom_col(
      position = position_dodge(width = 0.9),
      width = 0.8
    ) +
    
    geom_errorbar(
      aes(
        ymin = mean_value - se,
        ymax = mean_value + se
      ),
      position = position_dodge(width = 0.9),
      width = 0.2
    ) +
    
    labs(
      title = paste("Mean", response, "by Marsh, Treatment, and Year"),
      x = "Marsh Type - Treatment",
      y = paste("Mean", response),
      fill = "Year"
    ) +
    
    theme_bw() +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ) 
    ) 
    
 #add labels if show means is checked
  if(show_means){
      
      p <- p +
        geom_text(
          data = summary_data,
          aes(
            y = label_y,
            label = scales::number(mean_value, accuracy = 0.1)
          ),
          position = position_dodge(width = 0.9),
          vjust = -0.2,
          angle = 45,
          hjust = 0,
          size = 3
        )
}
 
 return(p)
}



plot_response_comparison <- function(data, responses, show_means = TRUE){
  
  data <- data %>%
    dplyr::mutate(
      marsh_treatment = interaction(
        marsh,
        treatment,
        sep = " - "
      )
    )
  
  
  summary_data <- data %>%
    dplyr::select(
      marsh_treatment,
      all_of(responses)
    ) %>%
    tidyr::pivot_longer(
      cols = all_of(responses),
      names_to = "response",
      values_to = "value"
    ) %>%
    dplyr::group_by(
      marsh_treatment,
      response
    ) %>%
    dplyr::summarise(
      mean_value = mean(value, na.rm = TRUE),
      se = sd(value, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      label_y = mean_value + se + (0.01 * max(mean_value, na.rm = TRUE))
    )
  
  
  p <- ggplot(
    summary_data,
    aes(
      x = marsh_treatment,
      y = mean_value,
      fill = response
    )
  ) +
    
    geom_col(
      position = position_dodge(width = 0.9),
      width = 0.8
    ) +
    
    geom_errorbar(
      aes(
        ymin = mean_value - se,
        ymax = mean_value + se
      ),
      position = position_dodge(width = 0.9),
      width = 0.2
    ) +
    
    labs(
      title = paste(
        "Count Indicators by Marsh and Treatment -",
        unique(data$year)
      ),
      x = "Marsh Type - Treatment",
      y = "Mean Count",
      fill = "Indicator"
    ) +
    
    theme_bw() +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      )
    )
  
  #add labels if show means is checked
  if(show_means){
    
    p <- p +
      geom_text(
        data = summary_data,
        aes(
          y = label_y,
          label = scales::number(mean_value, accuracy = 0.1)
        ),
        position = position_dodge(width = 0.9),
        vjust = -0.2,
        angle = 45,
        hjust = 0,
        size = 3
      )
  }
  
  return(p)
}



plot_elevation <- function(data, season_year, line_labels, line_values){
  
  data <- data %>%
    mutate(
      marsh_treatment = interaction(
        marsh,
        treatment,
        sep = " - "
      )
    )
  
  ggplot(
    data,
    aes(
      x = marsh_treatment,
      y = elevation_ft
    )
  ) +
    geom_boxplot(alpha = 0.6) +
    
    geom_hline(
      yintercept = line_values,
      linewidth = 1
    ) +
    
    geom_text(
      data = data.frame(
        label = line_labels,
        value = line_values
      ),
      aes(
        x = Inf,
        y = value,
        label = paste0(label, " ", value, " ft")
      ),
      inherit.aes = FALSE,
      hjust = 1,
      vjust = -0.5
    ) +
    
    labs(
      title = season_year,
      x = "Marsh Type - Treatment",
      y = "Elevation NAVD88 (ft)"
    ) +
    
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(
        angle = 45,
        hjust = 1 
      )
    )
}
