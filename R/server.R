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


  # Perform the statistical test using the selected variables
  # Inside the app_server function
  shiny::observeEvent(input$statistical_test, {
    shiny::req(input$dependent_var, input$independent_var, input$input_mean, data())
    message(input$statistical_test)
    # message(input$identifier_var)

    output$test_report <- shiny::renderPrint({
      cat(paste0("Welcome: "))
    })

    if (input$independent_var == "reference value") {
      mu <- input$input_mean
    } else {
      mu <- mean(data()[, input$dependent_var], na.rm = TRUE)
    }

    if (input$statistical_test == "Tekentoets I") {
      observeEvent(c(input$independent_var, input$input_mean, input$statistical_test), {
        # Determine the value of mu based on the user's selection
        if (input$independent_var == "reference value") {
          mu <- input$input_mean
        } else {
          mu <- mean(data()[, input$dependent_var], na.rm = TRUE)
        }
        tryCatch(
          {
            result <- DescTools::SignTest(x = data()[, input$dependent_var], mu = mu, alternative = "two.sided")
            output$test_report <- shiny::renderPrint({
              result
            })
          },
          error = function(e) {
            print(paste0("Caught an error while performing Tekentoets I: ", e))
            output$test_report <- shiny::renderPrint({
              cat(paste0("Error: ", e))
            })
          }
        )
      })
    } else if (input$statistical_test == "Wilcoxon signed rank toets I / Tekentoets II (paired)") {
      tryCatch(
        {
          result <- stats::wilcox.test(input$dependent_var ~ input$independent_var, data(),
            paired = TRUE,
            alternative = "two.sided"
          )
          output$test_report <- shiny::renderPrint({
            result
          })
        },
        error = function(e) {
          print(paste0("Caught an error while performing Wilcoxon signed rank toets I / Tekentoets II: ", e))
          output$test_report <- shiny::renderPrint({
            cat(paste0("Error: ", e))
          })
        }
      )
    } else if (input$statistical_test == "Mann-Whitney U toets I / Mood's mediaan toets (unpaired)") {
      tryCatch(
        {
          data <- data()
          dependent_var <- input$dependent_var
          independent_var <- input$independent_var
          unique_values <- unique(data[[independent_var]])
          group1 <- data[data[[independent_var]] == unique_values[1], dependent_var]
          group2 <- data[data[[independent_var]] == unique_values[2], dependent_var]
          result <- stats::wilcox.test(group1,
            group2,
            paired = FALSE,
            alternative = "two.sided",
            conf.int = TRUE
          )
          output$test_report <- shiny::renderPrint({
            result
          })
        },
        error = function(e) {
          print(paste0("Caught an error while performing Mann-Whitney U toets I / Mood's mediaan toets: ", e))
          output$test_report <- shiny::renderPrint({
            cat(paste0("Error: ", e))
          })
        }
      )
    } else if (input$statistical_test == "Kruskal Wallis toets I (unpaired)") {
      tryCatch(
        {
          result <- stats::kruskal.test(input$dependent_var ~ input$independent_var, data())
          output$test_report <- shiny::renderPrint({
            result
          })
        },
        error = function(e) {
          print(paste0("Caught an error while performing Kruskal Wallis toets I: ", e))
          output$test_report <- shiny::renderPrint({
            cat(paste0("Error: ", e))
          })
        }
      )
    } else if (input$statistical_test == "One sample t-test") {
      observeEvent(c(input$independent_var, input$input_mean), {
        # Determine the value of mu based on the user's selection
        if (input$independent_var == "reference value") {
          mu <- input$input_mean
        } else {
          mu <- mean(data()[, input$dependent_var], na.rm = TRUE)
        }

        tryCatch(
          {
            result <- stats::t.test(data(), mu = mu, alternative = "two.sided")
            output$test_report <- shiny::renderPrint({
              result
            })
          },
          error = function(e) {
            print(paste0("Caught an error while performing One sample t-test: ", e))
            output$test_report <- shiny::renderPrint({
              cat(paste0("Error: ", e))
            })
          }
        )
      })
    } else if (input$statistical_test == "Paired t-test (paired)") {
      tryCatch(
        {
          # Ensure data is a reactive expression that returns a data frame
          # Assuming data() is defined elsewhere in your Shiny app
          data_frame <- data()

          # Convert the independent variable to a factor
          data_frame[[input$independent_var]] <- as.factor(data_frame[[input$independent_var]])

          # Ensure the independent variable has exactly two levels
          if (length(levels(data_frame[[input$independent_var]])) != 2) {
            message("The independent variable must have exactly two levels for an independent t-test.")
          }

          # Perform the independent t-test
          result <- stats::t.test(data_frame[[input$dependent_var]] ~ data_frame[[input$independent_var]],
            paired = TRUE,
            alternative = "two.sided",
            var.equal = FALSE
          )
          output$test_report <- shiny::renderPrint({
            result
          })
        },
        error = function(e) {
          print(paste0("Caught an error while performing Independent samples t-test: ", e))
          output$test_report <- shiny::renderPrint({
            cat(paste0("Error: ", e))
          })
        }
      )
    } else if (input$statistical_test == "Independent samples t-test (unpaired)") {
      tryCatch(
        {
          # Ensure data is a reactive expression that returns a data frame
          # Assuming data() is defined elsewhere in your Shiny app
          data_frame <- data()

          # Convert the independent variable to a factor
          data_frame[[input$independent_var]] <- as.factor(data_frame[[input$independent_var]])

          # Ensure the independent variable has exactly two levels
          if (length(levels(data_frame[[input$independent_var]])) != 2) {
            stop("The independent variable must have exactly two levels for an independent t-test.")
          }

          # Perform the independent t-test
          result <- stats::t.test(data_frame[[input$dependent_var]] ~ data_frame[[input$independent_var]],
            paired = FALSE,
            alternative = "two.sided",
            var.equal = FALSE
          )
          output$test_report <- shiny::renderPrint({
            result
          })
        },
        error = function(e) {
          print(paste0("Caught an error while performing Independent samples t-test: ", e))
          output$test_report <- shiny::renderPrint({
            cat(paste0("Error: ", e))
          })
        }
      )
    } else if (input$statistical_test == "Repeated measures ANOVA (paired)") {
      result <- perform_repeated_measures_anova(
        data(),
        input$dependent_var,
        input$identifier_var,
        input$independent_var
      )

      if (!is.null(result)) {
        output$test_report <- shiny::renderPrint({
          result
        })
      } else {
        output$test_report <- shiny::renderPrint({
          "Error occurred while performing Repeated measures ANOVA."
        })
      }
    } else if (input$statistical_test == "One-way ANOVA (unpaired)") {
      tryCatch(
        {
          # Ensure data is a reactive expression that returns a data frame
          # Assuming data() is defined elsewhere in your Shiny app
          data_frame <- data()

          # Convert the independent variable to a factor
          data_frame[[input$independent_var]] <- as.factor(data_frame[[input$independent_var]])

          # Perform the ANOVA
          res.aov <- stats::aov(data_frame[[input$dependent_var]] ~ data_frame[[input$independent_var]])
          result <- summary(res.aov)
          output$test_report <- shiny::renderPrint({
            result
          })
        },
        error = function(e) {
          print(paste0("Caught an error while performing One-way ANOVA: ", e))
          output$test_report <- shiny::renderPrint({
            cat(paste0("Error: ", e))
          })
        }
      )
    } else if (input$statistical_test == "Chi-kwadraat toets voor goodness of fit en binomiaaltoets") {
      observeEvent(c(input$independent_var, input$input_mean, input$statistical_test, input$dependent_var), {
        ## Retrieve the data from input
        data <- data()

        ## Retrieve the input value for dependent_var
        dependent_var <- input$dependent_var

        ## Create a table with the counts of the dependent variable
        table_var <- table(data[[dependent_var]])

        ## Determine the reference value based on the user's selection
        if (input$independent_var == "reference value") {
          reference_value <- input$input_mean
        } else {
          # Calculate the proportion of the most frequent value in the dependent variable
          if (is.numeric(data[[dependent_var]])) {
            reference_value <- mean(data[[dependent_var]], na.rm = TRUE)
          } else {
            reference_value <- max(table_var) / sum(table_var)
          }
        }

        tryCatch(
          {
            # Check if there are any missing values in the data
            if (any(is.na(data[[dependent_var]]))) {
              # Handle missing values by excluding them from the analysis
              table_var <- table(data[[dependent_var]][!is.na(data[[dependent_var]])])
            }

            result <- stats::chisq.test(table_var, p = c(1 - reference_value, reference_value))
            output$test_report <- shiny::renderPrint({
              result
            })
          },
          error = function(e) {
            print(paste0("Caught an error while performing Chi-kwadraat toets voor goodness of fit en binomiaaltoets: ", e))
            output$test_report <- shiny::renderPrint({
              cat(paste0("Error: ", e))
            })
          }
        )
      })
    } else if (input$statistical_test == "McNemar toets (paired)") {
      tryCatch(
        {
          # Ensure data is a reactive expression that returns a data frame
          # Assuming data() is defined elsewhere in your Shiny app
          data_frame <- data()

          # Retrieve the input values for dependent_var and independent_var
          dependent_var <- input$dependent_var
          independent_var <- input$independent_var

          # Ensure the data is correctly structured for McNemar's test
          # This step might involve checking that each subject has exactly one observation in each group
          # For demonstration, let's assume 'data_frame' is correctly structured

          # Get the unique values of independent_var
          unique_values <- unique(data_frame[[independent_var]])

          # Create objects for the first and second groups
          group1 <- data_frame[data_frame[[independent_var]] == unique_values[1], dependent_var]
          group2 <- data_frame[data_frame[[independent_var]] == unique_values[2], dependent_var]

          # Create a frequency matrix
          group_matrix <- table(group1, group2)

          # Perform McNemar's test
          result <- exact2x2::exact2x2(group_matrix,
            paired = TRUE,
            midp = TRUE
          )

          # Display the test report
          output$test_report <- shiny::renderPrint({
            print(result)
          })
        },
        error = function(e) {
          output$test_report <- shiny::renderPrint({
            cat(paste0("Error occurred while performing McNemar toets (paired): ", e))
          })
        }
      )
    } else if (input$statistical_test == "Chi-kwadraat toets voor onafhankelijkheid en Fisher's exacte toets (unpaired)") {
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
    } else if (input$statistical_test == "Cochran's Q toets (paired)") {
      output$test_report <- shiny::renderPrint({
        "NOTE: The test chosen is yet to be implemented."
      })
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
    } else if (input$statistical_test == "Chi-kwadraat toets voor onafhankelijkheid en Fisher-Freeman-Halton exacte toets I (unpaired)") {
      # Perform the Chi-kwadraat toets voor onafhankelijkheid en Fisher-Freeman-Halton exacte toets I
      result <- stats::chisq.test(data()[[input$dependent_var]], data()[[input$independent_var]])
      # Display the test report
      output$test_report <- shiny::renderPrint({
        result
      })
      # ...
    } else if (input$statistical_test == "Chi-square goodness-of-fit test en multinomiaaltoets") {
      # Perform the Chi-square goodness-of-fit test en multinomiaaltoets
      # ...
      output$test_report <- shiny::renderPrint({
        "NOTE: The test chosen is yet to be implemented."
      })
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
    } else if (input$statistical_test == "Wilcoxon signed rank toets II (paired)") {
      # Perform the Wilcoxon signed rank toets II
      tryCatch(
        {
          # Ensure data is a reactive expression that returns a data frame
          # Assuming data() is defined elsewhere in your Shiny app
          data_frame <- data()

          # Convert the dependent variable to a factor and then to numeric
          # This step is necessary because the Wilcoxon signed-rank test requires numeric data
          data_frame[[input$dependent_var]] <- as.numeric(as.factor(data_frame[[input$dependent_var]]))

          # Perform the Wilcoxon signed-rank test
          # Assuming input$dependent_var is the variable representing the measurements before and after
          # and input$identifier_var is the variable representing the subjects
          result <- wilcox.test(data_frame[[input$dependent_var]], paired = TRUE)

          # Display the test report
          output$test_report <- shiny::renderPrint({
            result
          })
        },
        error = function(e) {
          print(paste0("Caught an error while performing Wilcoxon signed rank toets II (paired): ", e))
          output$test_report <- shiny::renderPrint({
            cat(paste0("Error: ", e))
          })
        }
      )
    } else if (input$statistical_test == "Mann-Whitney U toets II (unpaired)") {
      # Perform the Mann-Whitney U toets II
      output$test_report <- shiny::renderPrint({
        "NOTE: The test chosen is yet to be implemented."
      })
      # ...
    } else if (input$statistical_test == "Multilevel multinomiale logistische regressie (paired)") {
      output$test_report <- shiny::renderPrint({
        "NOTE: The test chosen is yet to be implemented."
      })
      # Perform the Multilevel multinomiale logistische regressie
      # ...
    } else if (input$statistical_test == "Friedman's ANOVA I (paired)") {
      output$test_report <- shiny::renderPrint({
        "NOTE: The test chosen is yet to be implemented."
      })
      # Perform the Multilevel multinomiale logistische regressie
      # ...
    } else if (input$statistical_test == "Friedman's ANOVA II (paired)") {
      tryCatch(
        {
          # Ensure data is a reactive expression that returns a data frame
          # Assuming data() is defined elsewhere in your Shiny app
          data_frame <- data()

          # Convert the independent variable to a factor
          data_frame[[input$independent_var]] <- as.factor(data_frame[[input$independent_var]])

          # Perform the Friedman's ANOVA II test
          # Assuming perform_friedman_test is a custom function you've defined elsewhere
          result <- perform_friedman_test(input$dependent_var, input$independent_var, input$identifier_var, data_frame)
          # result <- perform_friedman_test(data_frame[[input$dependent_var]], data_frame[[input$independent_var]], data_frame)

          # Display the test report
          output$test_report <- shiny::renderPrint({
            result
          })
        },
        error = function(e) {
          print(paste0("Caught an error while performing Friedman's ANOVA II (paired): ", e))
          output$test_report <- shiny::renderPrint({
            cat(paste0("Error: ", e))
          })
        }
      )
    } else if (input$statistical_test == "Kruskal Wallis toets II (unpaired)") {
      # Perform the Kruskal Wallis toets II
      # ...
      output$test_report <- shiny::renderPrint({
        "NOTE: The test chosen is yet to be implemented."
      })
    } else if (input$statistical_test == "Pearson Correlation") {
      tryCatch(
        {
          result <- stats::cor(data()[[input$dependent_var]], data()[[input$independent_var]], method = "pearson")
          output$test_report <- shiny::renderPrint({
            result
          })
        },
        error = function(e) {
          print(paste0("Caught an error while performing Pearson Correlation: ", e))
          output$test_report <- shiny::renderPrint({
            cat(paste0("Error: ", e))
          })
        }
      )
    } else if (input$statistical_test == "Spearman Correlation") {
      tryCatch(
        {
          result <- stats::cor(data()[[input$dependent_var]], data()[[input$independent_var]], method = "spearman")
          output$test_report <- shiny::renderPrint({
            result
          })
        },
        error = function(e) {
          print(paste0("Caught an error while performing Spearman Correlation: ", e))
          output$test_report <- shiny::renderPrint({
            cat(paste0("Error: ", e))
          })
        }
      )
    } else if (input$statistical_test == "No appropriate statistical test found for the given combination of dependent and independent variables.") {
      output$test_report <- shiny::renderPrint({
        "No appropriate statistical test found for the given combination of dependent and independent variables."
      })
    }
  })
}
