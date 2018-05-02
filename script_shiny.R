server <- function(input, output, session) { 
    
    updatePlotMain <- function(new_ts, name) {
      pMain <<- add_trace(pMain, x = sub_dates, y = new_ts, name = name)
      pBarsMain <<- add_trace(pBarsMain, x = sub_dates, y = x_err - new_ts, name = name)
      combPlotMain <<- subplot(pMain, pBarsMain, nrows = 2, shareX = TRUE)
      combPlotMain
    }
  
    output$optimization <- renderPlotly({ pContour })
    output$optPlot <- renderPlotly({ combPlotOpt })
      
    observe({
      if (input$browse == 0) return()
      updateTextInput(session, "path",  value = file.choose())
    })
    
    output$mainPlot <- renderPlotly({ 
      if (input$upload == 0) {
        combPlotMain
      }
      else {
        isolate({
          new_ts <- readRDS(input$path)
          name <- basename(input$path)
          updatePlotMain(new_ts, name)
        })
      }
    })
    

    output$table <- renderDataTable(
      if (input$upload == 0) {
        DT::datatable(data.frame(
          Name = names_col, trainME = round(errors_matrix[, 1], digits = 2), trainRMSE = round(errors_matrix[, 2], 2), 
          trainMAE = round(errors_matrix[, 3], digits = 2), testME = round(errors_matrix[, 8], digits = 2), testRMSE = round(errors_matrix[, 9], 2), 
                           testMAE = round(errors_matrix[, 10], digits = 2)
        ), colnames = c("Name", "train ME", "train RMSE", "train MAE", "test ME", "test RMSE", "test MAE"))
      }
      else {
        isolate({
          new_ts <- readRDS(input$path)
          name <- basename(input$path)
        })
        names_col <<- c(names_col, name)
        train_error <- accuracy(ts(new_ts[1:length(x_train_err)]), x_train_err)
        test_error <- accuracy(ts(new_ts[(length(x_train_err) + 1):length(new_ts)]), x_test_err)
        error <<- c(train_error, test_error)
        errors_matrix <<- rbind(errors_matrix, error)
        DT::datatable(data.frame(
          Name = names_col, trainME = round(errors_matrix[, 1], digits = 2), trainRMSE = round(errors_matrix[, 2], 2), 
          trainMAE = round(errors_matrix[, 3], digits = 2), testME = round(errors_matrix[, 8], digits = 2), testRMSE = round(errors_matrix[, 9], 2), 
          testMAE = round(errors_matrix[, 10], digits = 2)
        ), colnames = c("Name", "train ME", "train RMSE", "train MAE", "test ME", "test RMSE", "test MAE"))
      }

    )
      
} 

ui <- navbarPage("",
                   tabPanel("Graphics",
                            titlePanel("Monthly sunspots since 1749"),
                            mainPanel(
                                # Display KNN results)
                                plotlyOutput("mainPlot")   
                            ),
                            
                            headerPanel("Load data"),
                            sidebarPanel(
                              textInput("path", "File:"),
                              actionButton("browse", "Browse"),
                              tags$br(),
                              actionButton("upload", "Upload Data")
                            ),
                            
                            headerPanel("Table"),
                            sidebarPanel(
                              DT::dataTableOutput("table")
                            )
                            
                        
                   ),
                   
                   
                   tabPanel("Optimization",
                            mainPanel(
                                plotlyOutput("optimization")
                            ),
                            
                            headerPanel("Monthly sunspots since 1749"),
                            mainPanel(
                              plotlyOutput("optPlot") 
                            )
                   )
            )

# Now run the following: 
#shinyApp(server = server, ui = ui)
