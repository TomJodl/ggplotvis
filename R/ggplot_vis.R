#' Creating a ggplot based on a dataset
#'
#' @param data a dataset
#'
#' @return A ggplot based on the selected Variables. Either a Scatter- or Lineplot
#'
#' @examples
#' gg_vis_point(your_data)
#' gg_vis_line(mtcars)
#'
#' @import shiny
#' @import ggplot2
#'
#' @export
gg_vis_point <- function(data) {

  ui <- fluidPage(
    headerPanel("ggplot Visualizer for Scatterplots"),
    sidebarPanel(
      width = 3,
      selectInput(inputId = "VarX",
                  label = "Select X-axis Variable:",
                  choices = colnames(data)),
      selectInput(inputId = "VarY",
                  label = "Select Y-axis Variable:",
                  choices = colnames(data))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )

  server <- function(input, output, session) {
    output$plot <- renderPlot({
      ggplot(data, aes_string(x = input$VarX, y = input$VarY)) +
        geom_point() +
        labs(x = input$VarX, y = input$VarY)
    })
  }

  shinyApp(ui, server)
}

#' @export

gg_vis_line <- function(data) {

  ui <- fluidPage(
    headerPanel("ggplot Visualizer for Line Plots"),
    sidebarPanel(
      width = 3,
      selectInput(inputId = "VarX",
                  label = "Select X-axis Variable:",
                  choices = colnames(data)),
      selectInput(inputId = "VarY",
                  label = "Select Y-axis Variable:",
                  choices = colnames(data))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )

  server <- function(input, output, session) {
    output$plot <- renderPlot({
      ggplot(data, aes_string(x = input$VarX, y = input$VarY)) +
        geom_line() +
        labs(x = input$VarX, y = input$VarY)
    })
  }

  shinyApp(ui, server)
}

