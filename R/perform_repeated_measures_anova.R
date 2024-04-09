perform_repeated_measures_anova <- function(data, dependent_var, identifier_var, independent_var) {
  data[[dependent_var]] <- as.integer(data[[dependent_var]])
  data[[independent_var]] <- as.factor(data[[independent_var]])


  independent_var_unique <- unique(data[[independent_var]])
  message(independent_var)

  res.aov <- rstatix::anova_test(data = data,
                                 dv = dependent_var,
                                 wid = identifier_var,
                                 within = independent_var)

  rstatix::get_anova_table(res.aov)
}
