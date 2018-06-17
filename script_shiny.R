server <- function(input, output, session) {
    
    output$optimization <- renderPlotly({
        #pContour 
        click <- event_data("plotly_click")
        if (is.null(click) ) 
            return(pContour)
        
        k = click[[3]]
        d = click[[4]]
        # Selected points that are related to minimuns can't be taken out
        for (i in 1:5) {
            if(x_minims[i] == k && y_minims[i] == d) {
                return(pContour)
            }
        }
        selected_points[k, d] <<- !selected_points[k, d]
        if (selected_points[k,d]) { #it wasn't selected, so just have to add that dot
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
        if (is.null(click) )
            return(combPlotOpt)

        k = click[[3]]
        d = click[[4]]

        # The best combination is always plotted
        if (x_minims[1] == k && y_minims[1] == d)
            return(combPlotOpt)


        # It wasn't selected, so just have to add that line
        if (selected_points[k,d]) {
            auxPred <- knn_past(y = y, k = k, d = d, init = train_init,
                                     distance_metric = distance, weight = "proximity", threads = n_threads)
            pOpt <<- add_trace(pOpt, x = sub_dates, y = auxPred, name = paste("k =", k, "d =", d), legendgroup = paste("k", k, "d", d))
            pBarsOpt <<- add_trace(pBarsOpt, x = sub_dates, y = abs(y_err - auxPred), name = paste("k =" , k, "d =" , d, "error"), legendgroup = paste("k", k, "d", d))
            combPlotOpt <<- subplot(pOpt, pBarsOpt, nrows = 2, shareX = TRUE)
            return( combPlotOpt )
        }

        # Erased one point, so we have to replot everything
        pOpt <<- pOptBase
        pBarsOpt <<- pBarsOptBase
        for (i in 1:NROW(selected_points)) {
            for (j in 1:NCOL(selected_points)) {
                if (selected_points[i, j] && i != res$k %% j != res$d) {
                    preds <- knn_past(y = y, k = i, d = j, init = train_init, distance_metric = distance, weight = weight, threads = n_threads)
                    pOpt <<- add_trace(pOpt, x = sub_dates, y = preds, name = paste("k" , i, "d" , j), legendgroup = paste("k", i, "d", j))
                    pBarsOpt <<- add_trace(pBarsOpt, x = sub_dates, y = abs(y_err - preds), name = paste("k", i, "d", j, "error"), legendgroup = paste("k", i, "d", j))
                }
            }
        }
        combPlotOpt <<- subplot(pOpt, pBarsOpt, nrows = 2, shareX = TRUE)
        combPlotOpt

    })

    output$table_tab2 <- renderDataTable({
      names_col_local <- c(names_col[1])
      errors_matrix_local <- matrix(errors_matrix_tab2[1, ], nrow = 1)
      names_col_local <- c(names_col_local, names_col[2])
      errors_matrix_local <- rbind(errors_matrix_local, errors_matrix_tab2[2, ])

      for (i in 1:NROW(selected_points)) {
        for (j in 1:NCOL(selected_points)) {
          if (selected_points[i, j]){
            names_col_local <- c(names_col_local, paste("k" , i, "d" , j))
            preds <- knn_past(y = y, k = i, d = j, init = train_init, distance_metric = distance, weight = weight, threads = n_threads)
            residuals_aux <- y_err - preds
            train_error <- accuracy(ts(residuals_aux[1:length(y_train_err)]), y_train_err)
            test_error <- accuracy(ts(residuals_aux[(length(y_train_err) + 1):length(preds)]), y_test_err)
            errors_aux <- c(train_error, test_error)
            errors_matrix_local <- rbind(errors_matrix_local, errors_aux)
          }
        }
      }

      DT::datatable(data.frame(
        Name = names_col_local, trainME = round(errors_matrix_local[, 1], digits = 2), trainRMSE = round(errors_matrix_local[, 2], 2),
        trainMAE = round(errors_matrix_local[, 3], digits = 2), testME = round(errors_matrix_local[, 8], digits = 2),
        testRMSE = round(errors_matrix_local[, 9], 2), testMAE = round(errors_matrix_local[, 10], digits = 2)
      ), colnames = c("Name", "ME (train)", "RMSE (train)", "MAE (train)", "ME (test)", "RMSE (test)", "MAE (test)"))

    })
    
    output$mainPlot <- renderPlotly({
      # This traces are always in the graphic
      pMain <- plot_ly(x = dates, y = y, type = "scatter",  name = "Real", mode = "lines", legendgroup = "real", hoverinfo = "x+y")
      pMain <- add_trace(pMain, x = sub_dates, y = euc_prox, name = paste("Optimal k =", res$k, "d =", res$d), legendgroup = "optim")
      pMain <- add_segments(pMain, x = dates[train_init], xend = dates[train_init], y = min_x - 0.10 * (max_x - min_x), 
                            yend = max_x + 0.10 * (max_x - min_x), name = "Train", showlegend = FALSE, text = "Train",  hoverinfo = "text", line = list(color = "gray", width = 1.5, dash = "dash"))
      pMain <- add_segments(pMain, x = dates[test_init], xend = dates[test_init], y = min_x - 0.10 * (max_x - min_x), 
                            yend = max_x + 0.10 * (max_x - min_x), name = "Test", showlegend = FALSE, text = "Test",  hoverinfo = "text", line = list(color = "gray", width = 1.5, dash = "dash"))
      pMain <- layout(pMain, xaxis = list(rangeslider = list(type = "date")))
      
      if (input$chbabs == 1) {
        pErrMain <- plot_ly(x = sub_dates, y = abs(residuals_matrix[1, ]), name = "Optimal error", type = "scatter", mode = "markers", legendgroup = "optim", hoverinfo = "x+y")
      }
      else {
        pErrMain <- plot_ly(x = sub_dates, y = residuals_matrix[1, ], name = "Optimal error", type = "scatter", mode = "markers", legendgroup = "optim", hoverinfo = "x+y")
      }
      # Naive activated with checkbox
      if (input$chbnaive == 1) {
        pMain <- add_trace(pMain, x = sub_dates, y = naive, name = "Naive", legendgroup = "naive")
        if (input$chbabs == 1) {
          pErrMain <- add_trace(pErrMain, x = sub_dates, y = abs(residuals_matrix[2, ]), name = "Naive error", legendgroup = "naive") 
        }
        else {
          pErrMain <- add_trace(pErrMain, x = sub_dates, y = residuals_matrix[2, ], name = "Naive error", legendgroup = "naive") 
        }
      }
      
      if (input$chbsnaive == 1) {
        isolate({
          snaive <- ts(y[(train_init - as.numeric(input$s) + 1):(n - as.numeric(input$s))])
        })
        residuals_matrix[3, ] <- y_err - snaive
        pMain <- add_trace(pMain, x = sub_dates, y = snaive, name = "S. Naive", legendgroup = "snaive")
        if (input$chbabs == 1) {
          pErrMain <- add_trace(pErrMain, x = sub_dates, y = abs(residuals_matrix[3, ]), name = "S. Naive error", legendgroup = "snaive") 
        }
        else {
          pErrMain <- add_trace(pErrMain, x = sub_dates, y = residuals_matrix[3, ], name = "S. Naive error", legendgroup = "snaive") 
        }
      }
   
      # Load data activated with checkbox
      if (input$chbsload == 1) {
        isolate({
          new_ts <- readRDS(input$path)
          name <- basename(input$path)
        })
        pMain <- add_trace(pMain, x = sub_dates, y = new_ts, name = name, legendgroup = name)
        residuals_matrix[5, ] <- y_err - new_ts
        if (input$chbabs == 1) {
          pErrMain <- add_trace(pErrMain, x = sub_dates, y = abs(residuals_matrix[5, ]), name = paste(name, "error"), legendgroup = name)
        }
        else {
          pErrMain <- add_trace(pErrMain, x = sub_dates, y = residuals_matrix[5, ], name = paste(name, "error"), legendgroup = name)
          
        }
      }
      combPlotMain <- subplot(pMain, pErrMain, nrows = 2, shareX = TRUE)
      combPlotMain
    })
    
    
    output$table_tab1 <- renderDataTable({
      names_col_local <- c(names_col[1])
      errors_matrix_local <- matrix(errors_matrix_tab1[1, ], nrow = 1)
      
      # Naive activated with checkbox
      if (input$chbnaive == 1) {
        names_col_local <- c(names_col_local, names_col[2])
        errors_matrix_local <- rbind(errors_matrix_local, errors_matrix_tab1[2, ])
      }
      
      if (input$chbsnaive == 1) {
        names_col_local <- c(names_col_local, names_col[3])
        isolate({
          snaive <- ts(y[(train_init - as.numeric(input$s) + 1):(n - as.numeric(input$s))])
        })
        train_error <- accuracy(snaive[1:length(y_train_err)], y_train_err)
        test_error <- accuracy(snaive[(length(y_train_err) + 1):length(snaive)], y_test_err)
        errors_matrix_tab1[3, ] <- c(train_error, test_error)
        errors_matrix_local <- rbind(errors_matrix_local, errors_matrix_tab1[3, ])
      }
      
      if (input$chbsload == 1) {
        isolate({
          new_ts <- readRDS(input$path)
          name <- basename(input$path)
        })
        names_col_local <- c(names_col_local, name)
        train_error <- accuracy(ts(new_ts[1:length(y_train_err)]), y_train_err)
        test_error <- accuracy(ts(new_ts[(length(y_train_err) + 1):length(new_ts)]), y_test_err)
        errors_matrix_tab1[5, ] <- c(train_error, test_error)
        errors_matrix_local <- rbind(errors_matrix_local, errors_matrix_tab1[5, ])
      }
      
      DT::datatable(data.frame(
        Name = names_col_local, trainME = round(errors_matrix_local[, 1], digits = 2), trainRMSE = round(errors_matrix_local[, 2], 2), 
        trainMAE = round(errors_matrix_local[, 3], digits = 2), testME = round(errors_matrix_local[, 8], digits = 2), 
        testRMSE = round(errors_matrix_local[, 9], 2), testMAE = round(errors_matrix_local[, 10], digits = 2)
      ), colnames = c("Name", "ME (train)", "RMSE (train)", "MAE (train)", "ME (test)", "RMSE (test)", "MAE (test)"))

    })
    
} 

ui <- navbarPage("",
                 tabPanel("Graphics",
                          titlePanel("Monthly sunspots since 1749"),
                          mainPanel(
                              # Display KNN results)
                              plotlyOutput("mainPlot")   
                          ),
                          
                          sidebarPanel(
                            checkboxInput("chbnaive", label = "Naive", value = TRUE),
                            tags$hr(),
                            checkboxInput("chbsnaive", label = "Seasonal Naive", value = FALSE), 
                            textInput("s", "Seasonality"),
                            tags$hr(),
                            checkboxInput("chbsload", label = "Load data", value = FALSE),
                            textInput("path", "File:"),
                            actionButton("browse", "Browse"),
                            tags$hr(),
                            checkboxInput("chbabs", label = "Absolute error", value = FALSE)
                          ),
                          
                          headerPanel("Table"),
                          sidebarPanel(
                              DT::dataTableOutput("table_tab1")
                          )
                          
                          
                 ),
                 
                 
                 tabPanel("Optimization",
                          mainPanel(
                              plotlyOutput("optimization")
                          ),
                          
                          headerPanel("Monthly sunspots since 1749"),
                          mainPanel(
                              plotlyOutput("optPlot") 
                          ),
                          sidebarPanel(
                            checkboxInput("chbabs_tab2", label = "Absolute error", value = FALSE)
                          ),
                          headerPanel("Table"),
                          sidebarPanel(
                            DT::dataTableOutput("table_tab2")
                          )
                 )
)

# Now run the following: 
# shinyApp(server = server, ui = ui)