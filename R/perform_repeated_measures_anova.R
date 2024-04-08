perform_repeated_measures_anova <- function(data, dependent_var, identifier_var, independent_var) {
  data[[dependent_var]] <- as.numeric(data[[dependent_var]])
  data[[independent_var]] <- as.factor(data[[independent_var]])

  # Convert dependent variable to integer or factor
  data[[dependent_var]] <- as.integer(data[[dependent_var]])

  res.aov <- rstatix::anova_test(data = data,
                                 dv = data[[dependent_var]],
                                 wid = data[[identifier_var]],
                                 within = data[[independent_var]])

  rstatix::get_anova_table(res.aov)
}

