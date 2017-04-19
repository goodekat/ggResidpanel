library(shiny)
require(ggplot2)
require(dplyr)
library(plotly)
library(ggResidpanel)


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Residual Plots"),

  # Sidebar with a slider input for the number of bins
  sidebarPanel(
    selectInput("color", label = "Color of Points",
                choices = list("Blue" = "blue",
                               "Response" = "green"),
                selected = 1)
  ),

  # Show a plot of the generated distribution
  mainPanel(
    plotlyOutput("scatter"),
    plotlyOutput("resid"),
    plotlyOutput("qq")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$scatter <- renderPlotly({
    s <- ggplot(trees, aes(x = Girth, y = Volume)) +
      geom_point(aes(color = Volume)) +
      labs(x = "Girth", y = "Volume", title = "Scatter Plot of Volume versus Girth") +
      theme_bw(base_size = 10)
    ggplotly(s)
  })

  output$resid <- renderPlotly({
    model <- lm(Volume ~ Girth, data = trees)
    r <- resid_plot(model$residuals, model$fitted.values, response = trees$Volume)
    ggplotly(r)
  })

  output$qq <- renderPlotly({
    model <- lm(Volume ~ Girth, data = trees)
    q <- qq_plot(model$residuals)
    ggplotly(q)
  })
}

# Bind ui and server together
shinyApp(ui, server)
