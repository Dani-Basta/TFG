#####
####Still in development
#####
#Load to Enviroment "knn_optim", "knn_past" and "knn_elements". Also load packages "plotly" and "shiny"

x <- sunspot.month
init <- floor(NROW(x)*0.7)
distance <- "euclidean"
error_metric <-  "MAE"
weight <- "proximity"
nThreads <- 3

res <- knn_optim(x = x, k = 1:50, d = 1:30, init  = init, distance_metric = distance, error_metric = error_metric, weight = weight)

EucPro <- knn_past(x = x, k = res$k, d = res$d, init = init, distance_metric = distance, weight = "proximity", threads = nThreads)
EucTre <- knn_past(x = x, k = res$k, d = res$d, init = init, distance_metric = distance, weight = "trend", threads = nThreads)
EucSame <- knn_past(x = x, k = res$k, d = res$d, init = init, distance_metric = distance, weight = "same", threads = nThreads)

subX <- x[(init+1):NROW(x)]

library(plotly)
library(shiny)

plotMode <- "lines"

p1 <- plot_ly(x = 1:NROW(subX), type = "scatter", y = subX, name = "Real", mode = plotMode)
p1 <- add_trace(p1, y = EucPro, name = "Proximity", mode = plotMode) 
p1 <- add_trace(p1, y = EucTre, name = "Trend", mode = plotMode) 
p1 <- add_trace(p1, y = EucSame, name = "Same", mode = plotMode)
p1 <- layout(p1, title = "Prediccion normal", xaxis = list(rangeslider = list(type = "date")))

p2 <- plot_ly(x = 1:(NROW(subX)-1), type = "scatter", y = subX[1:(NROW(subX)-1)], name = "Real", mode = plotMode)
p2 <- add_trace(p2, y = EucPro[2:NROW(EucPro)], name = "Proximity", mode = plotMode) 
p2 <- add_trace(p2, y = EucTre[2:NROW(EucTre)], name = "Trend", mode = plotMode) 
p2 <- add_trace(p2, y = EucSame[2:NROW(EucSame)], name = "Same", mode = plotMode)
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
