server <- function(input, output) { 
    output$serie <- renderPlotly({ combPlot })
    output$optimization <- renderPlotly({ pOpt }) } 

ui <- navbarPage("",
                   tabPanel("Graphics",
                            titlePanel("Monthly sunspots since 1749"),
                            mainPanel(
                                        # Display KNN results)

                                            plotlyOutput("serie")
                                        
                            )
                            
                        
                   ),
                   
                   
                   tabPanel("Optimization",
                            fluidRow(
                                plotlyOutput("optimization")
                            )
                            
                   )
            )

# Now run the following: 
# shinyApp(server = server, ui = ui)
