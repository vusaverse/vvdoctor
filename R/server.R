#' Shiny App Server
#'
#' This file contains the server logic for the Shiny app.
#'
#' @export
app_server <- function(input, output, session) {
  output$distPlot <- shiny::renderPlot({
    req(input$file)

    # Read the uploaded file
    data <- read.csv(input$file$datapath)

    # Display the dataframe in the app
    View(data)

    # # Draw the histogram
    # hist(data, col = 'darkgray', border = 'white',
    #      xlab = 'Waiting time to next eruption (in mins)',
    #      main = 'Histogram of waiting times')
  })
}
