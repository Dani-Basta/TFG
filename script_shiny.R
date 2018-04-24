server <- function(input, output) { 
    output$serie <- renderPlotly({ combPlot })
    output$optimization <- renderPlotly({ pO }) } 

ui <- navbarPage("",
                   tabPanel("Graphics",
                            titlePanel("Monthly sunspots since 1749"),
                            sidebarLayout(
                                sidebarPanel(
                                    sliderInput("k",
                                                "number of neighbors",
                                                min = 1,
                                                max = 100,
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
                            
                                # Display KNN results)
                                fluidRow(
                                    plotlyOutput("serie")
                                )
                            )
                            
                        )
                   ),
                   
                   
                   tabPanel("Optimization",
                            fluidRow( 
                                column(10, plotlyOutput("optimization"))
                            )
                            
                   )
            )

# Now run the following: 
# shinyApp(server = server, ui = ui)
