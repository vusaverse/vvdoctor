#' App Server
#'
#' @param input The input object from the Shiny app.
#' @param output The output object from the Shiny app.
#' @param session The session object from the Shiny app.
#'
#' @export
app_server <- function(input, output, session) {
  data <- handle_file_upload(input, output, session)

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
    shiny::req(input$independent_var)

    ## Only show input field if reference value is selected
    if (input$independent_var == "reference value") {
      shiny::numericInput("input_mean", "Set reference value", value = mean(data()[, input$dependent_var], na.rm = TRUE))
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

  output$order <- shiny::renderPrint({input$foo})

  # New dropdown for selecting statistical test
  output$statistical_test_dropdown <- shiny::renderUI({

    shiny::req(input$dependent_var, input$independent_var)

    if (input$independent_var == "reference value") {
      independent_var <- "reference value"
    } else {
      independent_var <- data()[, input$independent_var]
    }
    test <- choose_statistical_test(data()[, input$dependent_var], independent_var, paired = input$paired_unpaired == "Paired")
    test_options <- c(test)
    shinyWidgets::pickerInput("statistical_test", "Choose statistical test", choices = test_options)
  })

  # Histogram of the dependent variable
  output$dependent_var_histogram <- shiny::renderPlot({
    shiny::req(data(), input$dependent_var)
    create_dependent_variable_histogram(data()[, input$dependent_var])
  })


  # Determine the type of dependent variable
  determine_dependent_variable <- function(dependent_var) {

    # Check if the selected dependent variable is numeric/float
    if (is.numeric(dependent_var)) {
      # Perform Shapiro-Wilk test on the dependent variable
      shapiro_test <- stats::shapiro.test(dependent_var)

      # Check if the p-value is less than 0.05
      if (shapiro_test$p.value < 0.05) {
        "The selected dependent variable is not normally distributed."
      } else {
        "The selected dependent variable is normally distributed."
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

  # Determine the type of independent variable
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

  # Create the histogram plot for the dependent variable
  create_dependent_variable_histogram <- function(dependent_var) {
    if (is.numeric(dependent_var)) {
      graphics::hist(dependent_var, main = "Histogram of Dependent Variable", xlab = "Values")
    }
  }

  # Perform the statistical test using the selected variables
  # Inside the app_server function
  shiny::observeEvent(input$statistical_test, {
    shiny::req(input$dependent_var, input$independent_var, data())

    if (input$independent_var == "reference value") {
      mu <- input$input_mean
    }

    if (input$statistical_test == "Tekentoets I") {
      # Perform the Tekentoets I test

      result <- DescTools::SignTest(x = data()[, input$dependent_var], mu = mu, alternative = "two.sided")

      # Display the test report
      output$test_report <- shiny::renderPrint({
        result
      })
    } else if (input$statistical_test == "Wilcoxon signed rank toets I / Tekentoets II") {
      # Perform the Wilcoxon signed rank toets I / Tekentoets II test
      result <- stats::wilcox.test(input$dependent_var ~ input$independent_var, data(),
                  paired = TRUE,
                  alternative = "two.sided")

      # Display the test report
      output$test_report <- shiny::renderPrint({
        result
      })
    } else if (input$statistical_test == "Mann-Whitney U toets I / Mood's mediaan toets") {
      ## Retrieve the data from input
      data <- data()

      ## Retrieve the input values for dependent_var and independent_var
      dependent_var <- input$dependent_var
      independent_var <- input$independent_var

      ## Get the unique values of independent_var
      unique_values <- unique(data[[independent_var]])

      ## Create objects for the first and second groups
      group1 <- data[data[[independent_var]] == unique_values[1], dependent_var]
      group2 <- data[data[[independent_var]] == unique_values[2], dependent_var]

      ## Perform Wilcoxon test
      result <- stats::wilcox.test(group1,
                                   group2,
                                   paired = FALSE,
                                   alternative = "two.sided",
                                   conf.int = TRUE)
      # Perform the Mann-Whitney U toets I / Mood's mediaan toets
      # Display the test report
      output$test_report <- shiny::renderPrint({
        result
      })
    } else if (input$statistical_test == "Kruskal Wallis toets I") {
      ## erronuous: Error in kruskal.test.default: all observations are in the same group
      # print("here")
      # result <- stats::kruskal.test(input$dependent_var ~ input$independent_var, data())
      #
      # output$test_report <- shiny::renderPrint({
      #   result
      # })


    } else if (input$statistical_test == "One sample t-test") {
      # Perform the One sample t-test
      result <- stats::t.test(data(), mu = mu, alternative = "two.sided")

      # Display the test report
      output$test_report <- shiny::renderPrint({
        result
      })
    } else if (input$statistical_test == "Paired t-test") {
      # Perform the Paired t-test
      result <- stats::t.test(input$dependent_var ~ input$independent_var, data(),
                       paired = TRUE,
                       alternative = "two.sided")

      # Display the test report
      output$test_report <- shiny::renderPrint({
        result
      })
    } else if (input$statistical_test == "Independent samples t-test") {
      # Perform the Independent samples t-test
      # Perform the Paired t-test
      result <- stats::t.test(input$dependent_var ~ input$independent_var, data(),
                       paired = FALSE,
                       alternative = "two.sided",
                       var.equal = FALSE)

      # Display the test report
      output$test_report <- shiny::renderPrint({
        result
      })
    } else if (input$statistical_test == "Repeated measures ANOVA") {
      # Perform the Repeated measures ANOVA
      ## hardcoded wid
      result <- ez::ezANOVA(data(), dv = input$dependent_var, wid = input$identifier_var,
              within = input$independent_var)
      # ...
      output$test_report <- shiny::renderPrint({
        result
      })
    } else if (input$statistical_test == "One-way ANOVA") {
      res.aov <- stats::aov(input$dependent_var ~ input$independent_var, data = input$data)
      result <- summary(res.aov)
      # Perform the One-way ANOVA
      # ...
      # Display the test report
      output$test_report <- shiny::renderPrint({
        result
      })
    } else if (input$statistical_test == "Chi-kwadraat toets voor goodness of fit en binomiaaltoets") {
      ## Retrieve the data from input
      data <- data()

      ## Retrieve the input value for dependent_var
      dependent_var <- input$dependent_var

      ## Create a table with the counts of students with and without hbo vooropleiding
      table_var <- table(data[[dependent_var]])

      ## Calculate the expected frequencies
      ## hardcoded mu is mean(data()), but not for this case
      result <- stats::chisq.test(table_var, p = c(0.1113, 1 - 0.1113))
      #
      # Perform the Chi-kwadraat toets voor goodness of fit en binomiaaltoets
      output$test_report <- shiny::renderPrint({
        result
      })
    } else if (input$statistical_test == "McNemar toets") {
      # Perform the McNemar toets
      ## Retrieve the data from input
      data <- data()

      ## Retrieve the input values for dependent_var and independent_var
      dependent_var <- input$dependent_var
      independent_var <- input$independent_var

      ## Get the unique values of independent_var
      unique_values <- unique(data[[independent_var]])

      ## Create objects for the first and second groups
      group1 <- data[data[[independent_var]] == unique_values[1], dependent_var]
      group2 <- data[data[[independent_var]] == unique_values[2], dependent_var]

      ## Create a frequency matrix
      group_matrix <- table(group1, group2)

      # Perform McNemar's test
      result <- exact2x2::exact2x2(group_matrix,
                         paired = TRUE,
                         midp = TRUE)

      # Voer McNemar toets uit

      output$test_report <- shiny::renderPrint({
        result
      })
    } else if (input$statistical_test == "Chi-kwadraat toets voor onafhankelijkheid en Fisher's exacte toets") {
      ## Retrieve the data from input
      data <- data()

      ## Retrieve the input values for dependent_var and independent_var
      dependent_var <- input$dependent_var
      independent_var <- input$independent_var

      ## Perform the chi-squared test
      result <- stats::chisq.test(data[[dependent_var]], data[[independent_var]])

      output$test_report <- shiny::renderPrint({
        result
      })

      # Perform the Chi-kwadraat toets voor onafhankelijkheid en Fisher's exacte toets
      # ...
    } else if (input$statistical_test == "Cochran's Q toets") {
      print("here")
      # Perform the Cochran's Q toets
      # ## hardcoded
      ## erronunous
      # result <- rstatix::cochran_qtest(data(), input$dependent_var ~ input$independent_var | input$identier_var)
      # # result <- car::CochranQTest(input$dependent_var ~ input$independent_var | Studentnummer,
      # #              data = data())
      # # # ...
      # output$test_report <- shiny::renderPrint({
      #   result
      # })

    } else if (input$statistical_test == "Chi-kwadraat toets voor onafhankelijkheid en Fisher-Freeman-Halton exacte toets I") {
      # Perform the Chi-kwadraat toets voor onafhankelijkheid en Fisher-Freeman-Halton exacte toets I
      # ...
    } else if (input$statistical_test == "Chi-square goodness-of-fit test en multinomiaaltoets") {
      # Perform the Chi-square goodness-of-fit test en multinomiaaltoets
      # ...
    } else if (input$statistical_test == "Bhapkar toets") {
      ## Retrieve the data from input
      data <- data()

      ## Retrieve the input values for dependent_var and independent_var
      dependent_var <- input$dependent_var
      independent_var <- input$independent_var

      ## Get the unique values of independent_var
      unique_values <- unique(data[[independent_var]])

      ## Create objects for the first and second groups
      group1 <- data[data[[independent_var]] == unique_values[1], dependent_var]
      group2 <- data[data[[independent_var]] == unique_values[2], dependent_var]

      ## Perform Bhapkar's test
      result <- irr::bhapkar(cbind(group1, group2))
      # Perform the Bhapkar toets

      output$test_report <- shiny::renderPrint({
        result
      })

    } else if (input$statistical_test == "Wilcoxon signed rank toets II") {
      # Perform the Wilcoxon signed rank toets II
      # ...
    } else if (input$statistical_test == "Mann-Whitney U toets II") {
      # Perform the Mann-Whitney U toets II
      # ...
    } else if (input$statistical_test == "Multilevel multinomiale logistische regressie") {
      # Perform the Multilevel multinomiale logistische regressie
      # ...
    } else if (input$statistical_test == "Friedman's ANOVA II") {
      # Perform the Friedman's ANOVA II test
      result <- perform_friedman_test(input$dependent_var, input$independent_var, data())

      # Display the test report
      output$test_report <- shiny::renderPrint({
        result
      })
    } else if (input$statistical_test == "Kruskal Wallis toets II") {
      # Perform the Kruskal Wallis toets II
      # ...
    }




  })

}
