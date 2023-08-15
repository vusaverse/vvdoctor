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
  formula <- stats::as.formula(paste(dependent_var, "~", independent_var, "| Studentnummer"))

  # Perform the Friedman's ANOVA II test
  result <- stats::friedman.test(formula, data)

  # Return the test result
  return(result)
}
