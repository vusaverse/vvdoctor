#' Choose Statistical Test
#'
#' Determines the appropriate statistical test based on the characteristics of the dependent and independent variables.
#'
#' @param dependent_var The dependent variable.
#' @param independent_var The independent variable.
#' @param paired Logical indicating whether the independent variable is paired or unpaired. Default is FALSE.
#'
#' @return The name of the chosen statistical test.
#'
#' @export
#'
choose_statistical_test <- function(dependent_var, independent_var, paired = FALSE) {
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

    if (independent_var_characteristics == "continuous") {
      return("Pearson Correlation")
      # } else if (independent_var_characteristics == "1 group") {
      #   return("One sample t-test")
      # } else if (independent_var_characteristics == "2 groups & paired") {
      #   return("Paired t-test (paired)")
    } else if (independent_var_characteristics == "2 groups & unpaired") {
      return("Independent samples t-test (unpaired)")
      # } else if (independent_var_characteristics == "2+ groups & paired") {
      #   return("Repeated measures ANOVA (paired)")
    } else if (independent_var_characteristics == "2+ groups & unpaired") {
      return("One-way ANOVA (unpaired)")
    }

  } else if (is.character(independent_var)) {
    unique_independent <- length(unique(independent_var))
    if (unique_independent == 2) {
      independent_var_characteristics <- "2 groups & unpaired"
    } else if (unique_independent >= 2) {
      independent_var_characteristics <- "2+ groups & unpaired"
    } else {
      ## Voor het geval de categorische onafhankelijke alleen 1 level heeft
      return("The independent variable type is not supported.")
    }


    # Use the characteristics to choose the statistical test
    if (dependent_var_characteristics == "Assumption of Normality violated") {
       if (independent_var_characteristics == "continuous") {
         return("Spearman Correlation")
      } else if (independent_var_characteristics == "2 groups & paired") {
        return("Wilcoxon signed rank toets I / Tekentoets II (paired)")
      # } else if (independent_var_characteristics == "2+ groups & paired") {
      #   return("Friedman's ANOVA I (paired)")
      } else if (independent_var_characteristics == "2 groups & unpaired") {
        return("Mann-Whitney U toets I / Mood's mediaan toets (unpaired)")
      # } else if (independent_var_characteristics == "2+ groups & unpaired") {
      #   return("Kruskal Wallis toets I (unpaired)")
      }

    } else if (dependent_var_characteristics == "Assumption of Normality met") {

      if (independent_var_characteristics == "continuous") {
        return("Pearson Correlation")
      # } else if (independent_var_characteristics == "1 group") {
      #   return("One sample t-test")
      # } else if (independent_var_characteristics == "2 groups & paired") {
      #   return("Paired t-test (paired)")
      } else if (independent_var_characteristics == "2 groups & unpaired") {
        return("Independent samples t-test (unpaired)")
      # } else if (independent_var_characteristics == "2+ groups & paired") {
      #   return("Repeated measures ANOVA (paired)")
      } else if (independent_var_characteristics == "2+ groups & unpaired") {
        return("One-way ANOVA (unpaired)")
      }
    } else if (dependent_var_characteristics == "binary") {
      # if (independent_var_characteristics == "1 group") {
      #   return("Chi-kwadraat toets voor goodness of fit en binomiaaltoets")
      # } else if (independent_var_characteristics == "2 groups & paired") {
      #   return("McNemar toets (paired)")
      } else if (independent_var_characteristics == "2 groups & unpaired") {
        return("Chi-kwadraat toets voor onafhankelijkheid en Fisher's exacte toets (unpaired)")
      # } else if (independent_var_characteristics == "2+ groups & paired") {
      #   return("Cochran's Q toets (paired)")
      } else if (independent_var_characteristics == "2+ groups & unpaired") {
        return("Chi-kwadraat toets voor onafhankelijkheid en Fisher-Freeman-Halton exacte toets I (unpaired)")
      }
    } else if (dependent_var_characteristics == "nominal/ordinal") {
      if (independent_var_characteristics == "1 group") {
        return("Chi-square goodness-of-fit test en multinomiaaltoets")
      # } else if (independent_var_characteristics == "2 groups & paired") {
      #   return(c("Bhapkar toets", "Wilcoxon signed rank toets II (paired)"))
      } else if (independent_var_characteristics == "2 groups & unpaired") {
        return(c("Chi-kwadraat toets voor onafhankelijkheid en Fisher-Freeman-Halton exacte toets I (unpaired)", "Mann-Whitney U toets II (unpaired)"))
      } else if (independent_var_characteristics == "2+ groups & paired") {
        return(c("Multilevel multinomiale logistische regressie (paired)", "Friedman's ANOVA II (paired)"))
      } else if (independent_var_characteristics == "2+ groups & unpaired") {
        return(c("Chi-kwadraat toets voor onafhankelijkheid en Fisher-Freeman-Halton exacte toets I (unpaired)", "Kruskal Wallis toets II (unpaired)"))
      }
    }

    # No appropriate statistical test found
    return("No appropriate statistical test found for the given combination of dependent and independent variables.")
}

