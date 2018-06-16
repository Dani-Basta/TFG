server <- function(input, output, session) {
    
    updatePlotMain <- function(new_ts, name) {
        pMain <<- add_trace(pMain, x = sub_dates, y = new_ts, name = name)
        pErrMain <<- add_trace(pErrMain, x = sub_dates, y = x_err - new_ts, name = name)
        combPlotMain <<- subplot(pMain, pErrMain, nrows = 2, shareX = TRUE)
        combPlotMain
    }
    
    output$optimization <- renderPlotly({
        #pContour 
        click <- event_data("plotly_click")
            
                print( length(click) ) ####
                print(click) ####
                if ( !is.null(click) ) { ####
                    print( typeof(click[[2]][[1]]) )
                    print( length(click[[2]][[1]]) )
                    print( NROW(click[[2]][[1]]) )
                    print( NCOL(click[[2]][[1]]) )
                    print(click[[2]][[1]] )
                    #click[2] ####
                }
        if (is.null(click) || length( click[[2]][[1]] ) == 1) 
            return(pContour)
        
        
        k = click[[3]]
        d = click[[4]]
        # Selected points that are related to minimuns can't be taken out
        for (i in 1:5) {
            if( x_minims[i] == k && y_minims[i] == d) {
                return(pContour)
            }
        }
        selected_points[k, d] <<- !selected_points[k, d]
        if (selected_points[k,d] ) { #it wasn't selected, so just have to add that dot
            pContour <<- add_trace(pContour, type = "scatter", mode = "markers", x = k, y = d, 
                                   text = as.character(res$errors[k, d]), marker = list(color = "red"), 
                                   hoverinfo = "x+y+text", showlegend = FALSE)
            return(pContour)
        }
        #it was selected, so we have to add all the dots again
        pContour <<- pContourBase
        for (i in 1:NROW(selected_points)) {
            for (j in 1:NCOL(selected_points)) {
                if (selected_points[i, j])
                    pContour <<- add_trace(pContour, type = "scatter", mode = "markers", x = i, y = j, 
                                           text = as.character(res$errors[i, j]), marker = list(color = "red"), 
                                           hoverinfo = "x+y+text", showlegend = FALSE)
            }
        }
        pContour
    })
    
    output$optPlot <- renderPlotly({ 
        #combPlotOpt 
        click <- event_data("plotly_click")
        if (is.null(click) || length( click[[2]][[1]] ) == 1) 
            return(combPlotOpt)
        
        k = click[[3]]
        d = click[[4]]
        
        #the best combination is always plotted
        if ( x_minims[1] == k && y_minims[1] == d)
            return( combPlotOpt )
        
        if (selected_points[k,d] ) { #it wasn't selected, so just have to add that line
            #auxPredtrain <- knn_past(x = x_train, k = k, d = d, init = train_init, 
            #                         distance_metric = distance, weight = "proximity", threads = nThreads)
            #auxPredtest <- knn_past(x = x, k = k, d = d, init = test_init, 
            #                        distance_metric = distance, weight = "proximity", threads = nThreads)
            #auxPred <- c(auxPredtrain, auxPredtest)
            auxPred <<- knn_past(y = x, k = k, d = d, init = train_init, 
                                     distance_metric = distance, weight = "proximity", threads = nThreads)
            pOpt <<- add_trace(pOpt, x = sub_dates, y = auxPred, name = paste("K", k, "D", d), legendgroup = paste("K", k, "D", d))
            pBarsOpt <<- add_trace(pBarsOpt, x = sub_dates, y = abs(x_err - auxPred), name = paste("K" , k, "D" , d, "error"), legendgroup = paste("K", k, "D", d))
            combPlotOpt <<- subplot(pOpt, pBarsOpt, nrows = 2, shareX = TRUE)
            #subplot(add_trace(pOpt, x = sub_dates, y = auxPred, name = paste("K" , click[[3]], "D" , click[[4]]) ), 
            #      add_trace(pBarsOpt, x = sub_dates, y = abs(x_err - auxPred), name = paste("K" , k, "D" , d, "error") ) , 
            #      nrows = 2, shareX = TRUE)
            return( combPlotOpt )
        }
        
        #erased one point, so we have to replot everything
        pOpt <<- pOptBase
        pBarsOpt <<- pBarsOptBase
        for (i in 1:NROW(selected_points)) {
            for (j in 1:NCOL(selected_points)) {
                if (selected_points[i, j])
                    #auxPredtrain <- knn_past(x = x_train, k = k, d = d, init = train_init, 
                    #                         distance_metric = distance, weight = "proximity", threads = nThreads)
                    #auxPredtest <- knn_past(x = x, k = k, d = d, init = test_init, 
                    #                        distance_metric = distance, weight = "proximity", threads = nThreads)
                    #auxPred <- c(auxPredtrain, auxPredtest)
                    auxPred <<- knn_past(y = x, k = k, d = d, init = train_init, 
                                        distance_metric = distance, weight = "proximity", threads = nThreads)
                    
                    pOpt <<- add_trace(pOpt, x = sub_dates, y = auxPred, name = paste("K" , i, "D" , j), legendgroup = paste("K" , i, "D" , j))
                    pBarsOpt <<- add_trace(pBarsOpt, x = sub_dates, y = abs(x_err - auxPred), name = paste("K", i, "D", j, "error"), legendgroup = paste("K" , i, "D" , j))
                    combPlotOpt <<- subplot(pOpt, pBarsOpt, nrows = 2, shareX = TRUE)
            }
        }
        combPlotOpt <<- subplot(pOpt, pBarsOpt, nrows = 2, shareX = TRUE)
        combPlotOpt
        
    })
    
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
# shinyApp(server = server, ui = ui)