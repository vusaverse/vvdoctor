#' Display Data Table
#'
#' Helper function to display a data table in a Shiny app.
#'
#' @param data The data frame to display.
#' @return A Shiny output object representing the data table
#'
#' @export
#'
display_data_table <- function(data) {
  DT::datatable(data)
}


#' Determine the Type of Dependent Variable
#'
#' This function determines the type of the dependent variable based on its data type and distribution.
#'
#' @param dependent_var A vector representing the dependent variable.
#'
#' @return A character string indicating the type of the dependent variable.
#'
#' @export
determine_dependent_variable <- function(dependent_var) {
  # Check if the selected dependent variable is numeric/float
  if (is.numeric(dependent_var)) {
    # Perform Shapiro-Wilk test on the dependent variable
    shapiro_test <- stats::shapiro.test(dependent_var)

    # Check if the p-value is less than 0.05
    if (shapiro_test$p.value < 0.05) {
      "Assumption of Normality violated by the dependent variable."
    } else {
      "Assumption of Normality met by the dependent variable."
    }
  } else if (is.character(dependent_var)) {
    # Check if the selected dependent variable has 2 unique values (binary) or more than 2 (nominal/ordinal)
    unique_values <- length(unique(dependent_var))
    if (unique_values == 2) {
      "The selected dependent variable is binary."
    } else if (unique_values > 2) {
      "The selected dependent variable is nominal/ordinal."
    } else {
      "The selected dependent variable has invalid data."
    }
  } else {
    "The selected dependent variable is not numeric or character."
  }
}

#' Determine the Type of Independent Variable
#'
#' This function determines the type of the independent variable based on its data type and the number of unique values.
#'
#' @param independent_var A vector representing the independent variable.
#'
#' @return A character string indicating the type of the independent variable.
#'
#' @export
determine_independent_variable <- function(independent_var) {
  # Check if the selected independent variable is numeric/float
  if (is.numeric(independent_var)) {
    "The selected independent variable is numeric."
  } else if (is.character(independent_var)) {
    # Check if the selected independent variable has 2 unique values or more than 2
    unique_values <- length(unique(independent_var))
    if (unique_values == 2) {
      "The selected independent variable has 2 unique values."
    } else if (unique_values > 2) {
      "The selected independent variable has more than 2 unique values."
    } else {
      "The selected independent variable has invalid data."
    }
  } else {
    "The selected independent variable is not numeric or character."
  }
}

#' Create a Histogram Plot for the Dependent Variable
#'
#' This function creates a histogram plot for a numeric dependent variable.
#'
#' @param dependent_var A numeric vector representing the dependent variable.
#'
#' @return No return value, the function creates a histogram plot.
#'
#' @export
create_dependent_variable_histogram <- function(dependent_var) {
  if (is.numeric(dependent_var)) {
    graphics::hist(dependent_var, main = "Histogram of Dependent Variable", xlab = "Values")
  }
}

#' Choose Statistical Test
#'
#' Determines the appropriate statistical test based on the characteristics of the dependent and independent variables.
#'
#' @param dependent_var The dependent variable.
#' @param independent_var The independent variable.
#'
#' @return The name of the chosen statistical test.
#'
#' @export
#'
choose_statistical_test <- function(dependent_var, independent_var) {
  # Determine the characteristics of the dependent variable
  if (is.numeric(dependent_var)) {
    shapiro_test <- stats::shapiro.test(dependent_var)
    if (shapiro_test$p.value < 0.05) {
      dependent_var_characteristics <- "Assumption of Normality violated"
    } else {
      dependent_var_characteristics <- "Assumption of Normality met"
    }
  } else if (is.character(dependent_var)) {
    unique_values <- length(unique(dependent_var))
    if (unique_values == 2) {
      dependent_var_characteristics <- "binary"
    } else if (unique_values > 2) {
      dependent_var_characteristics <- "nominal/ordinal"
    }
  } else {
    return("The dependent variable type is not supported.")
  }


  # Determine the characteristics of the independent variable
  if (is.numeric(independent_var)) {
    independent_var_characteristics <- "continuous"
  } else if (is.character(independent_var)) {
    unique_independent <- length(unique(independent_var))
    if (unique_independent == 1) {
      independent_var_characteristics <- "1 group"
    } else if (unique_independent == 2) {
      independent_var_characteristics <- "2 groups"
    } else if (unique_independent > 2) {
      independent_var_characteristics <- "2+ groups"
    } else {
      ## Voor het geval de categorische onafhankelijke alleen 1 level heeft
      return("The independent variable type is not supported.")
    }
  }



  # Use the characteristics to choose the statistical test
  if (dependent_var_characteristics == "Assumption of Normality violated") {
    if (independent_var_characteristics == "continuous") {
      return("Spearman Correlation")
    } else if (independent_var_characteristics == "1 group") {
      return("SignTest I")
    } else if (independent_var_characteristics == "2 groups") {
      return(c(
        "Wilcoxon Signed-Rank Test I / SignTest II (paired)",
        "Mann-Whitney U Test I / Mood's Median Test (unpaired)"
      ))
    } else if (independent_var_characteristics == "2+ groups") {
      return(c(
        # "Friedman's ANOVA I (paired)",
        "Kruskal-Wallis Test I (unpaired)"
      ))
    }
  } else if (dependent_var_characteristics == "Assumption of Normality met") {
    if (independent_var_characteristics == "continuous") {
      return("Pearson Correlation")
    } else if (independent_var_characteristics == "1 group") {
      return("One sample t-test")
    } else if (independent_var_characteristics == "2 groups") {
      return(c("Independent samples t-test (unpaired)", "Paired t-test (paired)"))
    } else if (independent_var_characteristics == "2+ groups") {
      return(c("One-way ANOVA (unpaired)"
               # "Repeated measures ANOVA (paired)"
      ))
    }
  } else if (dependent_var_characteristics == "binary") {
    if (independent_var_characteristics == "1 group") {
      return("Chi-Square Goodness-of-Fit Test and Binomial Test")
    } else if (independent_var_characteristics == "2 groups") {
      return(c("McNemar's Test (paired)", "Chi-Square Test of Independence en Fisher's Exact Test (unpaired)"))
    } else if (independent_var_characteristics == "2 groups & unpaired") {
      return("Chi-Square Test of Independence and Fisher's Exact Test (unpaired)")
    } else if (independent_var_characteristics == "2+ groups") {
      return(c(
        "Chi-Square Test of Independence and Fisher-Freeman-Halton Exact Test I (unpaired)"
        # "Cochran's Q test (paired)"
      ))
    }
  } else if (dependent_var_characteristics == "nominal/ordinal") {
    if (independent_var_characteristics == "1 group") {
      return("Chi-square goodness-of-fit test and Multinomial Test")
    } else if (independent_var_characteristics == "2 groups") {
      return(c(
        "Chi-Square Test of Independence and Fisher-Freeman-Halton Exact Test I (unpaired)",
        "Mann-Whitney U Test II (unpaired)",
        "Bhapkar's Test",
        "Wilcoxon Signed-Rank Test II (paired)"
      ))
    } else if (independent_var_characteristics == "2+ groups") {
      return(c(
        # "Multilevel Logistic Regression (paired)",
        # "Friedman's ANOVA II (paired)",
        "Chi-Square Test of Independence en Fisher-Freeman-Halton Exact Test I (unpaired)",
        "Kruskal-Wallis Test II (unpaired)"
      ))
    }
  }

  # No appropriate statistical test found
  return("No appropriate statistical test found for the given combination of dependent and independent variables.")
}

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



#' Perform Statistical Test
#'
#' This function performs the appropriate statistical test based on the user's input.
#'
#' @param data A data frame containing the variables.
#' @param input A list containing the user's input, including the selected statistical test, dependent variable, independent variable, identifier variable, and input mean.
#'
#' @return The result of the statistical test.
#'
#' @export
perform_statistical_test <- function(data, input) {
  test_name <- input$statistical_test
  dependent_var <- input$dependent_var
  independent_var <- input$independent_var
  identifier_var <- input$identifier_var
  input_mean <- input$input_mean

  # Input validation
  # Check for missing data
  if (any(is.na(data[[dependent_var]])) || any(is.na(data[[independent_var]]))) {
    shiny::showNotification(
      "Missing values detected in selected variables. Please remove or impute missing data before analysis.",
      type = "error",
      duration = 8000
    )
    return(NULL)
  }

  # Check for minimum sample size (e.g., at least 3 for most tests)
  if (length(data[[dependent_var]]) < 3 || length(data[[independent_var]]) < 3) {
    shiny::showNotification(
      "Sample size too small. Please select variables with at least 3 observations.",
      type = "error",
      duration = 8000
    )
    return(NULL)
  }

  # Check for appropriate variable types
  if (!is.numeric(data[[dependent_var]]) && !is.character(data[[dependent_var]])) {
    shiny::showNotification(
      "Dependent variable must be numeric or categorical.",
      type = "error",
      duration = 8000
    )
    return(NULL)
  }
  if (!is.numeric(data[[independent_var]]) && !is.character(data[[independent_var]])) {
    shiny::showNotification(
      "Independent variable must be numeric or categorical.",
      type = "error",
      duration = 8000
    )
    return(NULL)
  }

  # Determine the value of mu based on the user's selection
  mu <- if (independent_var == "reference value") input_mean else mean(data[[dependent_var]], na.rm = TRUE)

  # Perform the selected statistical test
  result <- tryCatch(
    {
      switch(test_name,
             "SignTest I" = DescTools::SignTest(x = data[[dependent_var]], mu = mu, alternative = "two.sided"),
             "Wilcoxon Signed-Rank Test I / SignTest II (paired)" = stats::wilcox.test(data[[dependent_var]] ~ data[[independent_var]], data, paired = TRUE, alternative = "two.sided"),
             "Mann-Whitney U Test I / Mood's Median Test (unpaired)" = {
               unique_values <- unique(data[[independent_var]])
               group1 <- data[data[[independent_var]] == unique_values[1], dependent_var]
               group2 <- data[data[[independent_var]] == unique_values[2], dependent_var]
               stats::wilcox.test(group1, group2, paired = FALSE, alternative = "two.sided", conf.int = TRUE)
             },
             "Kruskal-Wallis Test I (unpaired)" = stats::kruskal.test(data[[dependent_var]] ~ data[[independent_var]], data),
             "One sample t-test" = stats::t.test(data[[dependent_var]], mu = mu, alternative = "two.sided"),
             "Paired t-test (paired)" = stats::t.test(data[[dependent_var]] ~ data[[independent_var]], data, paired = TRUE, alternative = "two.sided", var.equal = FALSE),
             "Independent samples t-test (unpaired)" = {
               data[[independent_var]] <- as.factor(data[[independent_var]])
               if (length(levels(data[[independent_var]])) != 2) stop("The independent variable must have exactly two levels for an independent t-test.")
               part_one <- stats::t.test(data[[dependent_var]] ~ data[[independent_var]], data, paired = FALSE, alternative = "two.sided", var.equal = FALSE)

               formula <- stats::as.formula(paste(substitute(dependent_var), "~", substitute(independent_var))) ## formula working example!
               part_two <- rstatix::cohens_d(data, formula,
                                             var.equal = TRUE)

               c(part_one, part_two)

             },
             # "Repeated measures ANOVA (paired)" = perform_repeated_measures_anova(data, dependent_var, identifier_var, independent_var),
             "One-way ANOVA (unpaired)" = {
               data[[independent_var]] <- as.factor(data[[independent_var]])
               res.aov <- stats::aov(data[[dependent_var]] ~ data[[independent_var]])
               summary(res.aov)
             },
             "Chi-Square Goodness-of-Fit Test and Binomial Test" = {
               table_var <- table(data[[dependent_var]], useNA = "no")
               reference_value <- if (independent_var == "reference value") input_mean else max(table_var) / sum(table_var)
               stats::chisq.test(table_var, p = c(1 - reference_value, reference_value))
             },
             "McNemar's Test (paired)" = { ## Error: Error in exact2x2::exact2x2(group_matrix, paired = TRUE, midp = TRUE): 'x' must have at least 2 rows and columns
               unique_values <- unique(data[[independent_var]])
               group1 <- data[data[[independent_var]] == unique_values[1], dependent_var]
               group2 <- data[data[[independent_var]] == unique_values[2], dependent_var]
               group_matrix <- table(group1, group2)
               exact2x2::exact2x2(group_matrix, paired = TRUE, midp = TRUE)
             },
             "Chi-Square Test of Independence and Fisher's Exact Test (unpaired)" = stats::chisq.test(data[[dependent_var]], data[[independent_var]]),
             "Bhapkar's Test" = {
               unique_values <- unique(data[[independent_var]])
               group1 <- data[data[[independent_var]] == unique_values[1], dependent_var]
               group2 <- data[data[[independent_var]] == unique_values[2], dependent_var]
               irr::bhapkar(cbind(group1, group2))
             },
             "Wilcoxon Signed-Rank Test II (paired)" = {
               data[[dependent_var]] <- as.numeric(as.factor(data[[dependent_var]]))
               stats::wilcox.test(data[[dependent_var]], paired = TRUE)
             },
             "Pearson Correlation" = stats::cor(data[[dependent_var]], data[[independent_var]], method = "pearson"),
             "Spearman Correlation" = stats::cor(data[[dependent_var]], data[[independent_var]], method = "spearman"),
             # "Cochran's Q Test (paired)" = {
             # data[[dependent_var]] <- as.factor(data[[dependent_var]])
             # data[[independent_var]] <- as.factor(data[[independent_var]])
             # rstatix::cochran_qtest(data[[dependent_var]] ~ data[[independent_var]] | data[[identifier_var]], data)
             # },
             "Fisher's Exact Test (unpaired)" = {
               data[[dependent_var]] <- as.factor(data[[dependent_var]])
               data[[independent_var]] <- as.factor(data[[independent_var]])
               stats::fisher.test(data[[dependent_var]], data[[independent_var]])
             },
             # "Friedman's ANOVA I (paired)" = {
             # data[[dependent_var]] <- as.numeric(data[[dependent_var]])
             # data[[independent_var]] <- as.factor(data[[independent_var]])
             # message(identifier_var)
             # formula <- stats::as.formula(paste(substitute(dependent_var), "~", substitute(independent_var), "|", substitute(identifier_var)))
             # stats::friedman.test(substitute(dependent_var) ~ substitute(independent_var) | substitute(identifier_var), data)
             # stats::friedman.test(formula, data)
             # },
             # "Friedman's ANOVA II (paired)" = {
             #
             #
             #   message(dependent_var)
             #
             #   message(deparse(dependent_var))
             #   message(substitute(dependent_var))
             #   message(deparse(substitute(dependent_var)))
             #
             #   perform_friedman_test_now(data, substitute(dependent_var), substitute(independent_var), substitute(identifier_var))
             #   # data[[dependent_var]] <- as.numeric(data[[dependent_var]])
             #   # data[[independent_var]] <- as.factor(data[[independent_var]])
             #   # stats::friedman.test(data[[dependent_var]] ~ data[[independent_var]] | data[[identifier_var]], data)
             # },
             # "Multilevel Logistic Regression (paired)" = {
             #   data[[dependent_var]] <- as.factor(data[[dependent_var]])
             #   data[[independent_var]] <- as.factor(data[[independent_var]])
             #   message(identifier_var)
             #   lme4::glmer(data[[dependent_var]] ~ data[[independent_var]] + (1 | data[[identifier_var]]), data, family = binomial)
             # },
             stop(paste0("No appropriate statistical test found for the given combination of dependent and independent variables: ", dependent_var, " and ", independent_var))
      )
    },
    error = function(e) {
      shiny::showNotification(
        paste0(
          "An error occurred: ",
          if (!is.null(e$message)) e$message else as.character(e),
          "\nPlease check your input and try again."
        ),
        type = "error",
        duration = 8000
      )
      NULL
    }
  )

  return(result)
}

