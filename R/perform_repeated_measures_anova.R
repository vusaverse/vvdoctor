perform_repeated_measures_anova <- function(data, dependent_var, identifier_var, independent_var) {
  print(names(data))
  print(dependent_var)
  print(independent_var)
  print(identifier_var)


  tryCatch(
    {
      # Use eval() and substitute() to pass the variables to ezANOVA()
      result <- eval(
        substitute(
          ez::ezANOVA(
            data = data,
            wid = identifier_var,
            dv = dependent_var,
            within = independent_var
          ),
          list(
            dependent_var = noquote(dependent_var),
            identifier_var = noquote(identifier_var),
            independent_var = noquote(independent_var)
          )
        )
      )

      return(result)
    },
    error = function(e) {
      print(paste0("Caught an error while performing Repeated measures ANOVA: ", e))
      return(NULL)
    }
  )
}
