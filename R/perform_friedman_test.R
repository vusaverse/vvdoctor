# Function to perform Friedman's ANOVA II test
#' Title
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
