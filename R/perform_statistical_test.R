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
      cat(paste0("Error: ", e))
      NULL
    }
  )

  return(result)
}
