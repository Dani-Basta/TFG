#####
####Still in development
#####
#Load to Enviroment "knn_optim", "knn_past" and "knn_elements". Also load packages "plotly" and "shiny"

x <- sunspot.month
res <- knn_optim(x = x, k = 1:50, d = 1:30)
EucPro <- knn_past(x = x, k = res$k, d = res$d, init = 2224, weight = "proximity", threads = 3)
EucTre <- knn_past(x = x, k = res$k, d = res$d, init = 2224, weight = "trend", threads = 3)
EucSame <- knn_past(x = x, k = res$k, d = res$d, init = 2224, weight = "same", threads = 3)

subX <- x[2225:3177]

library(plotly)
library(shiny)

p1 <- plot_ly(x = 1:953, type = "scatter", y = subX[1:953], name = "Original", mode = "lines")
p1 <- add_trace(p1, y = EucPro[1:953], name = "Proximity", mode = "lines") 
p1 <- add_trace(p1, y = EucTre[1:953], name = "Trend", mode = "lines") 
p1 <- add_trace(p1, y = EucSame[1:953], name = "Same", mode = "lines")
p1 <- layout(p1, title = "Prediccion normal", xaxis = list(rangeslider = list(type = "date")))

p2 <- plot_ly(x = 1:952, type = "scatter", y = subX[1:952], name = "Original", mode = "lines")
p2 <- add_trace(p2, y = EucPro[2:953], name = "Proximity", mode = "lines") 
p2 <- add_trace(p2, y = EucTre[2:953], name = "Trend", mode = "lines") 
p2 <- add_trace(p2, y = EucSame[2:953], name = "Same", mode = "lines")
p2 <- layout(p2, title = "Prediccion desplazada")

pO <- plot_ly(z = res$errors, type = "contour", autocontour = TRUE, contours = list(showlabels = TRUE, coloring = "heatmap") )
pO <- layout(pO, title = "MAE Optimization", xaxis = list(title = "D"), yaxis = list(title = "K") )


ui <- navbarPage("",
                 tabPanel("Sunspot",
                          titlePanel("Monthly sunspots since 1749"),
                          # Sidebar with a slider input for number of bins
                          sidebarLayout(
                              sidebarPanel(
                                  sliderInput("k",
                                              "number of neighbors",
                                              min = 1,
                                              max = 70,
                                              value = 4
                                  ),
                                  sliderInput("d",
                                              "number values to characterize elements",
                                              min = 1,
                                              max = 50,
                                              value = 3
                                        )
                                ),
                              mainPanel(
                                  # Display Knn results)
                                  fluidRow(                           
                                      column(4, selectInput(inputId = "selected", 
                                                            label = h3("Graficas a mostrar"), 
                                                            choices = list("Original" = "original", 
                                                                           "Desplazado" = "desplazado" ),
                                                            selected = "original")
                                        )
                                    ),
                                  fluidRow( column(4, plotlyOutput("serie")) )
                                )
                            )
                    ),
                 tabPanel("Optimization",
                          fluidRow( 
                              column(10, plotlyOutput("optimization"))
                          )
                 )
    )


server <- function(input, output) { 
    output$serie <- renderPlotly({ p1 })
    output$optimization <- renderPlotly({ pO }) } 

# Now run the following: 
# shinyApp(server = server, ui = ui)
