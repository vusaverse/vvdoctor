#' Title
#'
#' @export
choose_statistical_test <- function(dependent_var, independent_var, paired = NULL) {
  # Determine the characteristics of the dependent variable
  if (is.numeric(dependent_var)) {
    shapiro_test <- shapiro.test(dependent_var)
    if (shapiro_test$p.value < 0.05) {
      dependent_var_characteristics <- "not normally distributed"
    } else {
      dependent_var_characteristics <- "normally distributed"
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
  unique_groups <- length(unique(independent_var))
  if (unique_groups == 1) {
    independent_var_characteristics <- "1 group"
  } else if (unique_groups == 2) {
    independent_var_characteristics <- "2 groups"
    if (paired) {
      independent_var_characteristics <- "2 groups & paired"
    } else {
      independent_var_characteristics <- "2 groups & unpaired"
    }
  } else {
    independent_var_characteristics <- "more than 2 groups"
    if (paired) {
      independent_var_characteristics <- "2+ groups & paired"
    } else {
      independent_var_characteristics <- "2+ groups & unpaired"
    }
  }


    # Use the characteristics to choose the statistical test
    if (dependent_var_characteristics == "not normally distributed") {
      if (independent_var_characteristics == "1 group") {
        return("Tekentoets I")
      } else if (independent_var_characteristics == "2 groups & paired" || independent_var_characteristics == "2+ groups & paired") {
        return("Wilcoxon signed rank toets I / Tekentoets II")
      } else if (independent_var_characteristics == "2 groups & unpaired" || independent_var_characteristics == "2+ groups & unpaired") {
        return("Mann-Whitney U toets I / Mood's mediaan toets")
      }
    } else if (dependent_var_characteristics == "normally distributed") {
      if (independent_var_characteristics == "1 group") {
        return("One sample t-test")
      } else if (independent_var_characteristics == "2 groups & paired") {
        return("Paired t-test")
      } else if (independent_var_characteristics == "2 groups & unpaired") {
        return("Independent samples t-test")
      } else if (independent_var_characteristics == "2+ groups & paired") {
        return("Repeated measures ANOVA")
      } else if (independent_var_characteristics == "2+ groups & unpaired") {
        return("One-way ANOVA")
      }
    } else if (dependent_var_characteristics == "binary") {
      if (independent_var_characteristics == "1 group") {
        return("Chi-kwadraat toets voor goodness of fit en binomiaaltoets")
      } else if (independent_var_characteristics == "2 groups & paired") {
        return("McNemar toets")
      } else if (independent_var_characteristics == "2 groups & unpaired") {
        return("Chi-kwadraat toets voor onafhankelijkheid en Fisher's exacte toets")
      } else if (independent_var_characteristics == "2+ groups & paired") {
        return("Cochran's Q toets")
      } else if (independent_var_characteristics == "2+ groups & unpaired") {
        return("Chi-kwadraat toets voor onafhankelijkheid en Fisher-Freeman-Halton exacte toets I")
      }
    } else if (dependent_var_characteristics == "nominal/ordinal") {
      if (independent_var_characteristics == "1 group") {
        return("Chi-square goodness-of-fit test en multinomiaaltoets")
      } else if (independent_var_characteristics == "2 groups & paired") {
        return(c("Bhapkar toets", "Wilcoxon signed rank toets II"))
      } else if (independent_var_characteristics == "2 groups & unpaired") {
        return(c("Chi-kwadraat toets voor onafhankelijkheid en Fisher-Freeman-Halton exacte toets I", "Mann-Whitney U toets II"))
      } else if (independent_var_characteristics == "2+ groups & paired") {
        return(c("Multilevel multinomiale logistische regressie", "Friedman's ANOVA II"))
      } else if (independent_var_characteristics == "2+ groups & unpaired") {
        return(c("Chi-kwadraat toets voor onafhankelijkheid en Fisher-Freeman-Halton exacte toets I", "Kruskal Wallis toets II"))
      }
    }

    # # Additional options for nominal variables
    # if (dependent_var_characteristics == "nominal") {
    #   if (independent_var_characteristics == "1 group") {
    #     return("Chi-kwadraat toets voor goodness of fit en multinomiaaltoets")
    #   } else if (independent_var_characteristics == "2 groups & paired") {
    #     return("Wilcoxon signed rank toets II")
    #   } else if (independent_var_characteristics == "2 groups & unpaired") {
    #     return("Mann-Whitney U toets II")
    #   } else if (independent_var_characteristics == "2+ groups & paired") {
    #     return("Friedman's ANOVA II")
    #   } else if (independent_var_characteristics == "2+ groups & unpaired") {
    #     return("Kruskal Wallis toets II")
    #   }
    # }

    # No appropriate statistical test found
    return("No appropriate statistical test found for the given combination of dependent and independent variables.")
}

