#' Perform Friedman's ANOVA II Test
#'
#' Performs the Friedman's ANOVA II test to compare multiple related groups.
#'
#' @param dependent_var The name of the dependent variable.
#' @param independent_var The name of the independent variable.
#' @param data The data frame containing the variables.
#'
#' @return The result of the Friedman's ANOVA II test.
#'
#' @export
#'
perform_friedman_test <- function(dependent_var, independent_var, data) {
  # Create the formula for the test
  formula <- as.formula(paste(dependent_var, "~", independent_var, "| Studentnummer"))

  # Perform the Friedman's ANOVA II test
  result <- friedman.test(formula, data)

  # Return the test result
  return(result)
}
