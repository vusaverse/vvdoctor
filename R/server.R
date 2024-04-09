#' App Server
#'
#' @param input The input object from the Shiny app.
#' @param output The output object from the Shiny app.
#' @param session The session object from the Shiny app.
#'
#' @export
app_server <- function(input, output, session) {
  data <- handle_file_upload(input, output, session)

  steps <- shiny::reactive({
    data.frame(
      element = c(NA, "#sep", "#header", "#dependent_var_dropdown", "#independent_var_dropdown", "#input_mean", "#identifier_dropdown", "#statistical_test_dropdown"),
      intro = c(
        "Welcome to the Statistical Test App. Let's start by uploading a file.",
        "Select the separator used in your file.",
        "Check this if your file has a header.",
        "Choose the dependent variable from this dropdown. The dependent variable is the outcome or response variable that is being studied and measured.",
        "Choose the independent variable from this dropdown. The independent is like the 'cause' or the 'input' that is being tested to see its effect on something else.",
        "Enter the mean here.",
        "Choose the identifier from this dropdown.",
        "Choose the statistical test from this dropdown. When should you use a paired or unpaired test? A paired test is used when the participants in the data are in both groups. For instance, if you want to research the effect of two different programs, and all the participants took both programs, you should use a paired test. If only part of the participants took one program and the other part of the participants took the other program, you should use an unpaired test. Other examples of unpaired tests would be differences between gender or A/B testing."
      )
    )
  })

  # Listen for the click event of the "About this app" action button
  shiny::observeEvent(input$about_app, {
    # Start the intro.js tour when the button is clicked
    rintrojs::introjs(session, options = list(steps = steps()))
  })

  # Display the uploaded data as a datatable
  output$dataTable <- DT::renderDataTable({
    shiny::req(data())
    DT::datatable(data())
  })

  # Dropdown for choosing the dependent variable
  output$dependent_var_dropdown <- shiny::renderUI({
    shiny::req(data())
    shinyWidgets::pickerInput("dependent_var", "Choose dependent variable", choices = colnames(data()))
  })

  # Dropdown for choosing the independent variable
  output$independent_var_dropdown <- shiny::renderUI({
    shiny::req(data())
    choices <- c("reference value", colnames(data()))
    shinyWidgets::pickerInput(
      "independent_var",
      "Choose independent variable or reference value",
      choices = choices
    )
  })

  output$input_mean <- shiny::renderUI({
    shiny::req(data())
    shiny::req(input$dependent_var)

    ## Retrieve the data from input
    data <- data()

    ## Calculate the mean of the data, if it's a valid value
    if (is.numeric(data[[input$dependent_var]])) {
      reference_value <- mean(data[[input$dependent_var]], na.rm = TRUE)
    } else {
      reference_value <- NULL
    }

    ## Only show input field if reference value is selected
    if (input$independent_var == "reference value") {
      shiny::numericInput("input_mean", "Set reference value", value = reference_value)
    }
  })

  # Dropdown for choosing the dependent variable
  output$identifier_dropdown <- shiny::renderUI({
    shiny::req(data())
    shinyWidgets::pickerInput("identifier_var", "Choose identifier variable", choices = colnames(data()))
  })

  # Text below the dropdowns
  output$dependent_var_text <- shiny::renderText({
    shiny::req(input$dependent_var)
    determine_dependent_variable(data()[, input$dependent_var])
  })

  # Additional text for independent variable
  output$independent_var_text <- shiny::renderText({
    shiny::req(input$independent_var)

    if (input$independent_var %in% colnames(data())) {
      determine_independent_variable(data()[, input$independent_var])
    }
  })

  # New dropdown for selecting statistical test
  output$statistical_test_dropdown <- shiny::renderUI({
    shiny::req(input$dependent_var, input$independent_var)

    if (input$independent_var == "reference value") {
      independent_var <- "reference value"
    } else {
      independent_var <- data()[, input$independent_var]
    }
    test <- choose_statistical_test(data()[, input$dependent_var], independent_var)
    test_options <- c(test)
    shinyWidgets::pickerInput("statistical_test", "Choose statistical test", choices = test_options)
  })

  # Histogram of the dependent variable
  output$dependent_var_histogram <- shiny::renderPlot({
    shiny::req(data(), input$dependent_var)
    create_dependent_variable_histogram(data()[, input$dependent_var])
  })

  ## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ## X. Statistical tests ####
  ## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  output$test_report <- renderPrint({
    result <- perform_statistical_test(data(), input)
    if (!is.null(result)) result
  })
}


perform_statistical_test <- function(data, input) {
  test_name <- input$statistical_test
  dependent_var <- input$dependent_var
  independent_var <- input$independent_var
  identifier_var <- input$identifier_var
  input_mean <- input$input_mean

  # Determine the value of mu based on the user's selection
  mu <- if (independent_var == "reference value") input_mean else mean(data[[dependent_var]], na.rm = TRUE)

  # Perform the selected statistical test
  result <- tryCatch(
    {
      switch(test_name,
        "SignTest I" = DescTools::SignTest(x = data[[dependent_var]], mu = mu, alternative = "two.sided"),
        "Wilcoxon signed rank toets I / SignTest II (paired)" = stats::wilcox.test(data[[dependent_var]] ~ data[[independent_var]], data, paired = TRUE, alternative = "two.sided"),
        "Mann-Whitney U toets I / Mood's mediaan toets (unpaired)" = {
          unique_values <- unique(data[[independent_var]])
          group1 <- data[data[[independent_var]] == unique_values[1], dependent_var]
          group2 <- data[data[[independent_var]] == unique_values[2], dependent_var]
          stats::wilcox.test(group1, group2, paired = FALSE, alternative = "two.sided", conf.int = TRUE)
        },
        "Kruskal Wallis toets I (unpaired)" = stats::kruskal.test(data[[dependent_var]] ~ data[[independent_var]], data),
        "One sample t-test" = stats::t.test(data[[dependent_var]], mu = mu, alternative = "two.sided"),
        "Paired t-test (paired)" = stats::t.test(data[[dependent_var]] ~ data[[independent_var]], data, paired = TRUE, alternative = "two.sided", var.equal = FALSE),
        "Independent samples t-test (unpaired)" = {
          data[[independent_var]] <- as.factor(data[[independent_var]])
          if (length(levels(data[[independent_var]])) != 2) stop("The independent variable must have exactly two levels for an independent t-test.")
          part_one <- stats::t.test(data[[dependent_var]] ~ data[[independent_var]], data, paired = FALSE, alternative = "two.sided", var.equal = FALSE)

          formula <- as.formula(paste(substitute(dependent_var), "~", substitute(independent_var))) ## formula working example!
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
        "Chi-kwadraat toets voor goodness of fit en binomiaaltoets" = {
          table_var <- table(data[[dependent_var]], useNA = "no")
          reference_value <- if (independent_var == "reference value") input_mean else max(table_var) / sum(table_var)
          stats::chisq.test(table_var, p = c(1 - reference_value, reference_value))
        },
        "McNemar toets (paired)" = { ## Error: Error in exact2x2::exact2x2(group_matrix, paired = TRUE, midp = TRUE): 'x' must have at least 2 rows and columns
          unique_values <- unique(data[[independent_var]])
          group1 <- data[data[[independent_var]] == unique_values[1], dependent_var]
          group2 <- data[data[[independent_var]] == unique_values[2], dependent_var]
          group_matrix <- table(group1, group2)
          exact2x2::exact2x2(group_matrix, paired = TRUE, midp = TRUE)
        },
        "Chi-kwadraat toets voor onafhankelijkheid en Fisher's exacte toets (unpaired)" = stats::chisq.test(data[[dependent_var]], data[[independent_var]]),
        "Bhapkar toets" = {
          unique_values <- unique(data[[independent_var]])
          group1 <- data[data[[independent_var]] == unique_values[1], dependent_var]
          group2 <- data[data[[independent_var]] == unique_values[2], dependent_var]
          irr::bhapkar(cbind(group1, group2))
        },
        "Wilcoxon signed rank toets II (paired)" = {
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
          # formula <- as.formula(paste(substitute(dependent_var), "~", substitute(independent_var), "|", substitute(identifier_var)))
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
      cat(paste0("Error: ", e))
      NULL
    }
  )

  return(result)
}



perform_friedman_test_now <- function(data, dependent_var, independent_var, identifier_var) {
  # Check if the required variables exist in the data frame
  if (!all(c(dependent_var, independent_var, identifier_var) %in% names(data))) {
    stop("One or more of the specified variables are not present in the data frame.")
  }

  # Construct the formula
  formula <- as.formula(paste(dependent_var, "~", independent_var, "|", identifier_var))
  message(formula)

  # Perform the Friedman test
  friedman_result <- friedman.test(formula, data = data)

  return(friedman_result)
}
