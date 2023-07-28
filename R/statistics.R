#' Get Variable Class Information
#'
#' This function determines the class of the selected variable and additional information.
#'
#' @param variable The selected variable data.
#' @return A character vector containing the variable class and additional information.
#' @export
get_variable_class_info <- function(variable) {
  if (is.numeric(variable)) {
    # Perform Shapiro-Wilk test to check for normality
    sw_test <- stats::shapiro.test(variable)
    is_normal <- sw_test$p.value >= 0.05

    # Create a character vector to store the variable class information
    var_class_info <- character(0)

    if (is_normal) {
      var_class_info <- c(var_class_info, "Independent Variable Class: Continuous (Normally Distributed)")
    } else {
      var_class_info <- c(var_class_info, "Independent Variable Class: Continuous (Not Normally Distributed)")
      # Include Shapiro-Wilk test result with p-value
      shapiro_result <- paste("Shapiro-Wilk Test: p-value =", format(sw_test$p.value, digits = 4))
      var_class_info <- c(var_class_info, shapiro_result)
    }

    return(var_class_info)
  } else {
    # Check if the categorical variable is binary or nominal
    unique_values <- unique(variable)
    is_binary <- length(unique_values) == 2

    if (is_binary) {
      var_class_info <- "Independent Variable Class: Categorical (Binary)"
    } else {
      var_class_info <- "Independent Variable Class: Categorical (Nominal)"
    }

    return(var_class_info)
  }
}
