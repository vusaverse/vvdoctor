#' Perform Friedman's ANOVA II Test
#'
#' Performs the Friedman's ANOVA II test to compare multiple related groups.
#'
#' @param dependent_var The name of the dependent variable.
#' @param independent_var The name of the independent variable.
#' @param identifier_var The name of the identifier variable.
#' @param data The data frame containing the variables.
#'
#' @return The result of the Friedman's ANOVA II test.
#'
#' @export
#'
perform_friedman_test <- function(dependent_var, independent_var, identifier_var, data) {
  # Create the formula for the test using paste to construct the formula string
  message(dependent_var)

  # Construct the formula directly without using as.formula
  # formula <- reformulate(independent_var, dependent_var, groups = identifier_var)

  # tryCatch(
  #   {
  #     # Perform the Friedman's ANOVA II test
  #     result <- stats::friedman.test(formula, data = data)
  #     return(result)
  #   },
  #   error = function(e) {
  #     print(paste0("Caught an error while performing Friedman's ANOVA II (paired): ", e))
  #     return(NULL)
  #   }
  # )
}




perform_friedman_test_now <- function(data, dependent_var, independent_var, identifier_var) {
  # Check if the required variables exist in the data frame
  if (!all(c(dependent_var, independent_var, identifier_var) %in% names(data))) {
    stop("One or more of the specified variables are not present in the data frame.")
  }

  # Construct the formula
  formula <- stats::as.formula(paste(dependent_var, "~", independent_var, "|", identifier_var))
  message(formula)

  # Perform the Friedman test
  friedman_result <- stats::friedman.test(formula, data = data)

  return(friedman_result)
}
