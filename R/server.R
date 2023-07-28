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
    req(data())
    DT::datatable(data())
  })

  # Dropdown for choosing the dependent variable
  output$dependent_var_dropdown <- renderUI({
    req(data())
    shiny::selectInput("dependent_var", "Choose dependent variable", choices = colnames(data()))
  })

  # Dropdown for choosing the independent variable
  output$independent_var_dropdown <- renderUI({
    req(data())
    shiny::selectInput("independent_var", "Choose independent variable", choices = c(NA, colnames(data())), selectize = FALSE)
  })

  # Text below the dropdowns
  output$dependent_var_text <- renderText({
    req(input$dependent_var)
    determine_dependent_variable(data()[, input$dependent_var])
  })

  # Additional text for independent variable
  output$independent_var_text <- renderText({
    req(input$independent_var)
    determine_independent_variable(data()[, input$independent_var])
  })


  # Observe the 'mean' input field for changes; overwrite independent
  observeEvent(input$input_mean, {
    if (!is.null(input$input_mean) && input$input_mean != "") {
      # If 'mean' input field is filled, update the independent variable value
      updateSelectInput(session, "independent_var", selected = input$input_mean)
    }
  })

  ## FAILED TEST: Hide when independent variable is chosen
  # observe({
  #   req(input$independent_var)
  #   print(input$independent_var)
  #   if (is.na(input$independent_var)) {
  #     output$input_mean <- renderUI({
  #       shiny::textInput("input_mean", "Input mean:", value = "")
  #     })
  #   } else {
  #     output$input_mean <- renderUI({
  #       shiny::tags$div()
  #     })
  #   }
  # })


  # New dropdown for selecting statistical test
  output$statistical_test_dropdown <- renderUI({
    req(input$dependent_var, input$independent_var)
    test <- choose_statistical_test(data()[, input$dependent_var], data()[, input$independent_var], paired = input$paired_unpaired == "Paired")
    test_options <- c("None", test)
    shiny::selectInput("statistical_test", "Choose statistical test", choices = test_options)
  })

  # Histogram of the dependent variable
  output$dependent_var_histogram <- renderPlot({
    req(data(), input$dependent_var)
    create_dependent_variable_histogram(data()[, input$dependent_var])
  })


  # Determine the type of dependent variable
  determine_dependent_variable <- function(dependent_var) {
    # Check if the selected dependent variable is numeric/float
    if (is.numeric(dependent_var)) {
      # Perform Shapiro-Wilk test on the dependent variable
      shapiro_test <- shapiro.test(dependent_var)

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
      hist(dependent_var, main = "Histogram of Dependent Variable", xlab = "Values")
    }
  }

  # Perform the statistical test using the selected variables
  # Inside the app_server function
  observeEvent(input$statistical_test, {
    req(input$dependent_var, input$independent_var, data())

    if (input$statistical_test == "Tekentoets I") {
      # Perform the Tekentoets I test

      result <- DescTools::SignTest(x = input$dependent_var, mu = input$independent_var, alternative = "two.sided")

      # Display the test report
      output$test_report <- renderPrint({
        result
      })
      # ...
    } else if (input$statistical_test == "Wilcoxon signed rank toets I / Tekentoets II") {
      result <- wilcox.test(input$dependent_var ~ input$independent_var, data(),
                  paired = TRUE,
                  alternative = "two.sided")
      # Perform the Wilcoxon signed rank toets I / Tekentoets II test
      # ...
      # Display the test report
      output$test_report <- renderPrint({
        result
      })
    } else if (input$statistical_test == "Mann-Whitney U toets I / Mood's mediaan toets") {
      # Perform the Mann-Whitney U toets I / Mood's mediaan toets
      # ...
    } else if (input$statistical_test == "One sample t-test") {
      # Perform the One sample t-test
      # ...
    } else if (input$statistical_test == "Paired t-test") {
      # Perform the Paired t-test
      result <- t.test(input$dependent_var ~ input$independent_var, data(),
                       paired = TRUE,
                       alternative = "two.sided")

      # Display the test report
      output$test_report <- renderPrint({
        result
      })
    } else if (input$statistical_test == "Independent samples t-test") {
      # Perform the Independent samples t-test
      # Perform the Paired t-test
      result <- t.test(input$dependent_var ~ input$independent_var, data(),
                       paired = FALSE,
                       alternative = "two.sided",
                       var.equal = FALSE)

      # Display the test report
      output$test_report <- renderPrint({
        result
      })
      # ...
    } else if (input$statistical_test == "Repeated measures ANOVA") {
      # Perform the Repeated measures ANOVA
      ## hardcoded
      ez::ezANOVA(data(), dv = input$dependent_var, wid = Studentnummer,
              within = input$independent_var)
      # ...
    } else if (input$statistical_test == "One-way ANOVA") {
      res.aov <- aov(input$dependent_var ~ input$independent_var, data = input$data)
      result <- summary(res.aov)
      # Perform the One-way ANOVA
      # ...
      # Display the test report
      output$test_report <- renderPrint({
        result
      })
    } else if (input$statistical_test == "Chi-kwadraat toets voor goodness of fit en binomiaaltoets") {
      # Perform the Chi-kwadraat toets voor goodness of fit en binomiaaltoets
      # ...
    } else if (input$statistical_test == "McNemar toets") {
      # Perform the McNemar toets
      # ...
    } else if (input$statistical_test == "Chi-kwadraat toets voor onafhankelijkheid en Fisher's exacte toets") {
      # Perform the Chi-kwadraat toets voor onafhankelijkheid en Fisher's exacte toets
      # ...
    } else if (input$statistical_test == "Cochran's Q toets") {
      # Perform the Cochran's Q toets
      ## hardcoded
      result <- CochranQTest(input$dependent_var ~ input$independent_var | Studentnummer,
                   data = data())
      # ...
      output$test_report <- renderPrint({
        result
      })

    } else if (input$statistical_test == "Chi-kwadraat toets voor onafhankelijkheid en Fisher-Freeman-Halton exacte toets I") {
      # Perform the Chi-kwadraat toets voor onafhankelijkheid en Fisher-Freeman-Halton exacte toets I
      # ...
    } else if (input$statistical_test == "Chi-square goodness-of-fit test en multinomiaaltoets") {
      # Perform the Chi-square goodness-of-fit test en multinomiaaltoets
      # ...
    } else if (input$statistical_test == "Bhapkar toets") {
      # Perform the Bhapkar toets
      # ...
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

      print(result)
      # Display the test report
      output$test_report <- renderPrint({
        result
      })
    } else if (input$statistical_test == "Kruskal Wallis toets II") {
      # Perform the Kruskal Wallis toets II
      # ...
    }




  })

}
