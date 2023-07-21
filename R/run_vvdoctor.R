#' Run the Statistical Test App
#'
#' This function starts the Shiny app, which allows users to upload data and perform
#' statistical tests on the dataset.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(vvdoctor)
#' run_statistical_test_app()
#' }
run_vvdoctor <- function() {
  shiny::shinyApp(ui = app_ui(), server = app_server)
}
