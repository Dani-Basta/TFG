library(plotly)
library(shiny)
library(shinyWidgets)
library(DT)

# distColIndex <- function(size, row) { 
#     end <- ((size - 1)*size/2) - ((row - 2)*(row - 1)/2 ) 
#     return((end - (row - 2)):end)
# }

selected_points <- matrix(rep(FALSE, NROW(res$errors) * NCOL(res$errors)), nrow = NROW(res$errors), ncol = NCOL(res$errors))
selected_points_aux <<- selected_points

server <- function(input, output, session) {
    
    output$elemsPlot <- renderPlotly({
        pMain <<- pMainBase
    
        if ( (input$selKtabDist == res$opt_k || input$selKtabDist == "" ) && 
             (input$selDtabDist == res$opt_d || input$selDtabDist == "") ) {
            
            # Parámetros de combinación óptima
            pMain <<- add_trace(pMain, x = sub_dates, y = optimal$fitted, line = list(color = colPalette[2]), 
                               name = paste0("Optimal (k = ", res$opt_k, ", d = ", res$opt_d, ")"), legendgroup = "optim")
            
            errors <- residuals_matrix[1, ]
            
            actK <- res$opt_k
            actD <- res$opt_d
            
            y_low  <- min_y - 0.05 * (max_y - min_y)
            y_high <- max_y + 0.05 * (max_y - min_y)
            
            if (input$chbabsDist == TRUE) {
                pError <<- plot_ly(x = sub_dates, y = abs(errors), legendgroup = "optim", hoverinfo = "x+y", 
                                   name = "Optimal error", type = "bar", marker = list(color = colPalette[2]) ) 
                err_low  <- min(abs(errors)) - 0.05 * (max(abs(errors)) - min(abs(errors)))
                err_high <- max(abs(errors)) + 0.05 * (max(abs(errors)) - min(abs(errors)))
            }
            else {
                pError <<- plot_ly(x = sub_dates, y = errors, legendgroup = "optim", hoverinfo = "x+y", 
                                   name = "Optimal error", type = "bar", marker = list(color = colPalette[2]) )
                err_low  <- min(errors) - 0.05 * (max(errors) - min(errors))
                err_high <- max(errors) + 0.05 * (max(errors) - min(errors))
            }
        }
        else {
            actK <- ifelse(input$selKtabDist == "", res$opt_k, as.numeric(input$selKtabDist))
            actD <- ifelse(input$selDtabDist == "", res$opt_d, as.numeric(input$selDtabDist))
            
            preds <- knn_past(y = y, k = actK, d = actD, initial = train_init, distance = distance, 
                              weight = weight, threads = n_threads)$mean
            
            # Gráfico de predicciones 
            pMain <<- add_trace(pMain, x = sub_dates, y = preds, name = paste0("k = " , actK, ", d = " , actD, " prediction"), 
                               legendgroup = paste("k", actK, "d", actD), line = list(color = colPalette[2]))
            
            errors <- y_err - preds
            
            y_low  <- min(preds) - 0.05 * (max(preds) - min(preds))
            y_high <- max(preds) + 0.05 * (max(preds) - min(preds))
            
            if (input$chbabsDist == TRUE) {
                pError <<- plot_ly(x = sub_dates, y = abs(errors), legendgroup = paste("k", actK, "d", actD), hoverinfo = "x+y",
                                   name = paste0("k = " , actK, ", d = " , actD, " error"), type = "bar", marker = list(color = colPalette[2]) ) 
                err_low  <- min(abs(errors)) - 0.05 * (max(abs(errors)) - min(abs(errors)))
                err_high <- max(abs(errors)) + 0.05 * (max(abs(errors)) - min(abs(errors)))
            }
            else {
                pError <<- plot_ly(x = sub_dates, y = errors, legendgroup = paste("k", actK, "d", actD), hoverinfo = "x+y",
                                   name = paste0("k = " , actK, ", d = " , actD, " error"), type = "bar", marker = list(color = colPalette[2]) ) 
                err_low  <- min(errors) - 0.05 * (max(errors) - min(errors))
                err_high <- max(errors) + 0.05 * (max(errors) - min(errors))
            }
        }

        # Clicked on main plot
        click <- event_data("plotly_click", source = "main")
        if (!is.null(click)) {
            # print("Procesando click en main")
            # print(click)
            x_click <- match(as.Date(click[[3]]), dates)
            
            # print(paste0("Se ha encontrado en ", x_click, " que corresponde a ", dates[x_click] ))
            # y_click <- click[[4]]
            
            pMain <<- add_segments(pMain, x = click[[3]], xend = click[[3]], y = y_low, yend = y_high, 
                                  name = "Knn", showlegend = FALSE, hoverinfo = "x", # text = "Knn",  
                                  legendgroup = "knn", line = list(color = "blue", width = 1.5, dash = "dash"))
            
            if ( x_click < n ) {
                pError <<- add_segments(pError, x = full_dates[x_click+1], xend = full_dates[x_click+1], y = err_low, yend = err_high, 
                                   name = "Knn", showlegend = FALSE, hoverinfo = "text", type = "line", mode = "line",  
                                   legendgroup = "knn", line = list(color = "blue", width = 1.5, dash = "dash"), text = "Prediction error \n for selected")
            }
            
            ind <- 1
            # shapes <- pMainShapes
            
            # pMain <<- layout(pMain, xaxis = list(rangeslider = list(type = "date")), shapes = pMainShapes)
            shapesInd <- length(pMain[["x"]][["layoutAttrs"]][[ pMain[["x"]][["cur_data"]] ]][["shapes"]]) + 1
            
            
            # distances <- parDist( knn_elements(matrix(y, ncol = NCOL(y)), d = res$opt_d), method = distance, threads = n_threads)
            # # print(paste0("El tamaño de distancias es ", attr(distances, "Size"), " y se va a solicitar el ", x_click + 1 - res$opt_d))
            # distances <- rev(distances[distColIndex(attr(distances, "Size"), x_click + 1 - res$opt_d)])
            # print(paste0("Se van a comparar ", length(distances), " distancias en el grafico") )
            neighbors <- knn_forecast(y = head(y, x_click), k = actK, d = actD, distance = distance, 
                                 weight = weight, threads = n_threads)$neighbors
            # distances <- pred$distances 
            
            for (i in neighbors)  {
                pMain <<- add_segments(pMain, x = dates[i], xend = dates[i], y = y_low, yend = y_high, name = "Knn",
                                       showlegend = FALSE, text = paste0(ind, "-nearest"), hoverinfo = "x+text",
                                       legendgroup = "knn", line = list(color = "red", width = 1.5, dash = "dash"))
                
                # shapes[[length(shapes)+1]] <- list(type = "rect",
                pMain[["x"]][["layoutAttrs"]][[ pMain[["x"]][["cur_data"]] ]][["shapes"]][[shapesInd]] <<- list(type = "rect",    
                                    fillcolor = "red", line = list(color = "red"), opacity = 0.2,
                                    x0 = dates[(i+1-actD)], x1 = dates[i], xref = "x", yref = "y",
                                    # y0 = min_y - 0.05 * (max_y - min_y), y1 = max_y + 0.05 * (max_y - min_y)),
                                    y0 = 0.95 * min(y[(i+1-actD):i]), y1 = 1.05 * max(y[(i+1-actD):i])) 
                
                ind <- ind + 1
                shapesInd <- shapesInd + 1
            }
            # print(shapes)
            # pMain <<- layout(pMain, xaxis = list(rangeslider = list(type = "date")), shapes = shapes)
            
        } 
        # else {
        #     pMain <<- layout(pMain, xaxis = list(rangeslider = list(type = "date")), shapes = pMainShapes)
        # }
        
        # pMain
        #combPlotMain
         
        s <- subplot(pMain, pError, nrows = 2, shareX = TRUE)
        s$x$source <- "main"
        s
    })
    
    
    output$neighborsPlot <- renderPlotly({
        click <- event_data("plotly_click", source = "main")
        if (!is.null(click)) {
            # print("Procesando click en main")
            # print(click)
            x_click <- match(as.Date(click[[3]]), dates)

            # print(paste0("Se ha encontrado en ", x_click, " que corresponde a ", dates[x_click] ))
            # y_click <- click[[4]]
            
            actK <- ifelse(input$selKtabDist == "", res$opt_k, as.numeric(input$selKtabDist))
            actD <- ifelse(input$selDtabDist == "", res$opt_d, as.numeric(input$selDtabDist))
            
            # pKNN <- plot_ly(type = "scatter",  mode = "lines", showlegend = FALSE) %>% 
            # add_trace(x = dates[(x_click + 1 - res$opt_d):x_click], y = y[(x_click + 1 - res$opt_d):x_click], line = list(color = colPalette[1]))
            
            pKNN <- plot_ly(type = "scatter",  mode = "lines+markers", showlegend = TRUE) %>% 
                add_trace(x = (-(actD - 1)):0, y = y[(x_click + 1 - actD):x_click], line = list(color = colPalette[1], width = 5), 
                          name = paste0("Observed (", dates[x_click], ")"), marker = list(color = colPalette[1], size = 7), hoverinfo = "text+y", legendgroup = "Observed",
                          text = paste0("Observed, \n", format(dates[(x_click + 1 - actD):x_click], format = "%B %Y")) ) %>%
                layout(title = list(text = paste0(actK, "-nearest neighboors"), font = list(size = 15) ), 
                       xaxis = list(zerolinecolor = "blue", zerolinewidth = 2) )
            
            if ( x_click < n ) {
                pKNN <- add_trace(pKNN, x = 0:1, y = y[x_click:(x_click + 1)], line = list(color = colPalette[1], width = 5, dash = "dash"), 
                          name = "Observed", marker = list(color = colPalette[1], size = 7), hoverinfo = "text+y", legendgroup = "Observed",
                          text = paste0("Observed, \n", format(dates[x_click:(x_click + 1)], format = "%B %Y")), showlegend = FALSE)
                          # text = paste0("Observed, \n", dates[x_click:(x_click+1)]), showlegend = FALSE) 
            }
            
            pKNN <- add_trace(pKNN, x = 1, y = knn_forecast(y = head(y, x_click), k = actK, d = actD, 
                                                            distance = distance, weight = weight, threads = n_threads)$mean, 
                        name = paste0("Prediction (", full_dates[(x_click + 1)], ")"), legendgroup = "Prediction", mode = "marker", hoverinfo = "text+y",
                        marker = list(color = colPalette[2], size = 8), text = paste0("Prediction for ", full_dates[(x_click + 1)]) )
            
            
            # distances <- parDist( knn_elements(matrix(y, ncol = NCOL(y)), d = res$opt_d), method = distance, threads = n_threads)
            # # print(paste0("El tamaño de distancias es ", attr(distances, "Size"), " y se va a solicitar el ", x_click + 1 - res$opt_d))
            # distances <- distances[distColIndex(attr(distances, "Size"), x_click + 1 - res$opt_d)]
            
            ind <- 1
            
            redDecr   <- (250 - 150) / (actK - 1)
            transDecr <- (0.9 - 0.2) / (actK - 1)
            
            if (actK == res$opt_k && actD == res$opt_d && x_click+1 >= train_init && x_click < n) {
                print(x_click-train_init)
                print(dates[x_click])
                for (i in optimal$neighbors[,(x_click+1-train_init)] ) {
                    pKNN <- add_trace(pKNN, x = (-(actD - 1)):0, y = y[(i + 1 - actD):i], name = paste0(ind,"-NN (", dates[(i)], ")" ), legendgroup = paste0(ind,"-NN"),
                                      line = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), width = ifelse(ind>actK/2, 3, 4)),
                                      marker = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), size = ifelse(ind>actK/2, 5, 6)),
                                      text = paste0(ind,"-nearest, \n", format(dates[(i + 1 - actD):i], format = "%B %Y")), hoverinfo = "text+y") %>%
                        add_trace(pKNN, x = 0:1, y = y[i:(i+1)], showlegend = FALSE, name = paste0(ind,"-NN (", dates[(i)], ")" ),legendgroup = paste0(ind,"-NN"),
                                  line = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), width = ifelse(ind>actK/2, 3, 4), dash = "dash"),
                                  marker = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), size = ifelse(ind>actK/2, 5, 6)),
                                  text = paste0(ind,"-nearest, \n", format(dates[(i):(i + 1)], format = "%B %Y")), hoverinfo = "text+y")
                    
                    ind <- ind + 1
                }
            }
            else {
                # distances <- knn_elements(matrix( y[1:(x_click - 1)], ncol = NCOL(y)), d = actD)
                # distances <- rev( cdist( matrix(y[ (x_click + 1 - actD):x_click], nrow = 1), distances) )
                
                neighbors <- knn_forecast(y = head(y, x_click), k = actK, d = actD, distance = distance, 
                                          weight = weight, threads = n_threads)$neighbors
                
                for (i in neighbors ) {
                    # pKNN <- add_trace(pKNN, x = dates[(i + 1 - res$opt_d):i], y = y[(i + 1 - res$opt_d):i], line = list(color = "random") )
                    pKNN <- add_trace(pKNN, x = (-(actD - 1)):0, y = y[(i + 1 - actD):i], name = paste0(ind,"-NN (", dates[(i)], ")" ), legendgroup = paste0(ind,"-NN"),
                                      line = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), width = ifelse(ind>actK/2, 3, 4)),
                                      marker = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), size = ifelse(ind>actK/2, 5, 6)),
                                      text = paste0(ind,"-nearest, \n", format(dates[(i + 1 - actD):i], format = "%B %Y")), hoverinfo = "text+y") %>%
                        add_trace(pKNN, x = 0:1, y = y[(i):(i + 1)], showlegend = FALSE, name = paste0(ind,"-NN (", dates[(i)], ")" ),legendgroup = paste0(ind,"-NN"),
                                  line = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), width = ifelse(ind>actK/2, 3, 4), dash = "dash"),
                                  marker = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), size = ifelse(ind>actK/2, 5, 6)),
                                  text = paste0(ind,"-nearest, \n", format(dates[(i):(i+1)], format = "%B %Y")), hoverinfo = "text+y")
                    
                    ind <- ind + 1
                }
            }
            
            
            pKNN
            # pDists
            # subplot(pKNN, pDists, nrows = 1)
        } 
        else {
            NULL
        }
    })
    
    
    output$distsPlot <- renderPlotly({
        click <- event_data("plotly_click", source = "main")
        if (!is.null(click)) {
            # print("Procesando click en main")
            # print(click)
            x_click <- match(as.Date(click[[3]]), dates)
            # print(paste0("Se ha encontrado en ", x_click, " que corresponde a ", dates[x_click] ))
            # y_click <- click[[4]]
            
            actK <- ifelse(input$selKtabDist == "", res$opt_k, as.numeric(input$selKtabDist))
            actD <- ifelse(input$selDtabDist == "", res$opt_d, as.numeric(input$selDtabDist))
            
            # distances <- parDist( knn_elements(matrix(y, ncol = NCOL(y)), d = res$opt_d), method = distance, threads = n_threads)
            # # print(paste0("El tamaño de distancias es ", attr(distances, "Size"), " y se va a solicitar el ", x_click + 1 - res$opt_d))
            # distances <- distances[distColIndex(attr(distances, "Size"), x_click + 1 - res$opt_d)]
            
            # distances <- knn_elements(matrix( y[1:(x_click - 1)], ncol = NCOL(y)), d = actD)
            
            # pKNN <- plot_ly(type = "scatter",  mode = "lines", showlegend = FALSE) %>% 
            # add_trace(x = dates[(x_click + 1 - res$opt_d):x_click], y = y[(x_click + 1 - res$opt_d):x_click], line = list(color = colPalette[1]))
            
            # pKNN <- plot_ly(type = "scatter",  mode = "lines", showlegend = FALSE) %>% 
            #     add_trace(x = (-(actD - 1)):0, y = y[(x_click + 1 - actD):x_click], line = list(color = colPalette[1], width = 5), 
            #                text = paste0("Observed, \n", format(dates[(x_click + 1 - actD):x_click], format = "%B %Y")), hoverinfo = "text+y")
            
            # distances <- rev( cdist( matrix(y[ (x_click + 1 - actD):x_click], nrow = 1), distances) )
            
            # colores <- rep("royalblue", length(distances))
            #colores[ head( sort.int(distances, index.return = TRUE)$ix , res$opt_k) ] <- "red"
            
            # ind <- 1
            # for (i in head( sort.int(distances, index.return = TRUE)$ix , actK) ) {
            #     # colores[i] <- "red"
            #     # pKNN <- add_trace(pKNN, x = dates[(i + 1 - res$opt_d):i], y = y[(i + 1 - res$opt_d):i], line = list(color = "random") )
            #     pKNN <- add_trace(pKNN, x = (-(actD - 1)):0, y = y[i:(i - 1 + actD)], line = list(color = "random", width = 3) , showlegend = FALSE,
            #                       text = paste0(ind,"-nearest, \n", format(dates[i:(i - 1 + actD)], format = "%B %Y")), hoverinfo = "text+y")
            #     ind <- ind + 1
            # }
            
            # distColors <- c("darkcyan", "lightskyblue", "lightcyan")
            
            # distances <- (max(distances)*1.1) - distances
            
            # distances <- 1 - ((distances-(min(distances))) / (max(distances) - min(distances)))
            
            # distances <-  min(distances) / distances
            
            
            distances <- knn_forecast(y = head(y, x_click), k = actK, d = actD, distance = distance, 
                                      weight = weight, threads = n_threads)$distances

            distances <- min(distances) / distances

            distColors <- rev(c("darkcyan", "lightskyblue", "lightcyan"))
            # x = tail(head(dates, length(distances) + actD - 1), length(distances))
            pDists <<- plot_ly(x = window(dates, start = actD, end = x_click-1), y = distances, name  = "Knn distances", 
                              showlegend = FALSE, hoverinfo = "x+y", type = "bar", color = distances, colors = distColors) %>%
                    layout(xaxis = list( range = list(dates[ (actD)], dates[(actD + length(distances) - 1)]), 
                                        rangeslider = list( range = list(dates[ (actD)], dates[(actD + length(distances) - 1)]) ) ) )
            
            # pKNN
            pDists
            # subplot(pKNN, pDists, nrows = 1)
        } 
        else {
            NULL
        }
    })

    
    
    
    output$errorsPlot <- renderPlotly({
        # This traces are always in the graphic
        # pMain <- plot_ly(x = dates, y = y, type = "scatter",  name = "Real Time Series", mode = "lines", source = "main", 
        #                  legendgroup = "real", hoverinfo = "x+y" ) #, line = list(color = "blue"))
        # pMain <- add_trace(pMain, x = sub_dates, y = optimal, legendgroup = "optim", 
        #                    name = paste0("Optimal (k = ", res$opt_k, ", d = ", res$opt_d, ")"))
        # # pMain <- add_trace(pMain, x = sub_dates, y = naive, name = "Naive", legendgroup = "naive")
        # # Separation lines for train and test
        # pMain <- add_segments(pMain, x = dates[train_init], xend = dates[train_init], y = min_y - 0.05 * (max_y - min_y), 
        #                       yend = max_y + 0.05 * (max_y - min_y), name = "Train", showlegend = FALSE, text = "Train", 
        #                       hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
        # pMain <- add_segments(pMain, x = dates[test_init], xend = dates[test_init], y = min_y - 0.05 * (max_y - min_y), 
        #                       yend = max_y + 0.05 * (max_y - min_y), name = "Test", showlegend = FALSE, text = "Test", 
        #                       hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
        # pMain <- layout(pMain, xaxis = list(rangeslider = list(type = "date")))
        
        ####
        pMain  <- pMainBase
        ####
        
        if (input$chbabs == 1) {
            pErrMain <- plot_ly(x = sub_dates, y = abs(residuals_matrix[1, ]), name = "Optimal Error",
                                type = "scatter", mode = "markers", legendgroup = "optim", hoverinfo = "x+y")
        }
        else {
            pErrMain <- plot_ly(x = sub_dates, y = residuals_matrix[1, ], name = "Optimal Error",
                                type = "scatter", mode = "markers", legendgroup = "optim", hoverinfo = "x+y")
        }
        
        # Naive activated with checkbox
        if (input$chbnaive == 1) {
            pMain <- add_trace(pMain, x = sub_dates, y = naive, name = "Naive", legendgroup = "naive")
            if (input$chbabs == 1) {
                pErrMain <- add_trace(pErrMain, x = sub_dates, y = abs(residuals_matrix[2, ]),
                                      name = "Naive Error", legendgroup = "naive")
            }
            else {
                pErrMain <- add_trace(pErrMain, x = sub_dates, y = residuals_matrix[2, ],
                                      name = "Naive Error", legendgroup = "naive")
            }
        }
        
        if (input$chbsnaive == 1) {
            isolate({
                snaive <- ts(y[(train_init - as.numeric(input$s) + 1):(n - as.numeric(input$s))])
            })
            residuals_matrix[3, ] <- y_err - snaive
            pMain <- add_trace(pMain, x = sub_dates, y = snaive, name = "S. Naive", legendgroup = "snaive")
            if (input$chbabs == 1) {
                pErrMain <- add_trace(pErrMain, x = sub_dates, y = abs(residuals_matrix[3, ]),
                                      name = "S. Naive Error", legendgroup = "snaive")
            }
            else {
                pErrMain <- add_trace(pErrMain, x = sub_dates, y = residuals_matrix[3, ],
                                      name = "S. Naive Error", legendgroup = "snaive")
            }
        }
        
        # # Load data activated with checkbox
        if (input$chbload == 1) {
            isolate({
                new_ts <- readRDS(input$path)
                name <- basename(input$path)
            })
            pMain <- add_trace(pMain, x = sub_dates, y = new_ts, name = name, legendgroup = name)
            residuals_matrix[5, ] <- y_err - new_ts
            if (input$chbabs == 1) {
                pErrMain <- add_trace(pErrMain, x = sub_dates, y = abs(residuals_matrix[5, ]),
                                      name = paste(name, "Error"), legendgroup = name)
            }
            else {
                pErrMain <- add_trace(pErrMain, x = sub_dates, y = residuals_matrix[5, ],
                                      name = paste(name, "Error"), legendgroup = name)
                
            }
        }
        
        combPlotMain <- subplot(pMain, pErrMain, nrows = 2, shareX = TRUE)
        
        #combPlotMain
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
        
        if (input$chbload == 1) {
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
    
    
    
    
    output$contourPlot <- renderPlotly({
        #pContour 
        
        click <- event_data("plotly_click", source = "contour")
        # click <- event_data("plotly_click")
        
        if (!is.null(click)) {
            #print("Procesando click")
            # print(click)
            k <- click[[3]]
            d <- click[[4]]
            
            # same_click <- TRUE
            # print("Llega antes del if")
            if ( exists("last_click") ) {
                # print("Se mete en if")
                same_click <- k == last_click[1]
                # print("Procesada la K")
                same_click <- ifelse(d == last_click[2], same_click, FALSE)
                # print("Procesada la D")
                same_click <- ifelse( is.null(click[["z"]]) == last_click[3], same_click, FALSE)
                
                same_click <- ifelse( click[["curveNumber"]] == last_click[4], same_click, FALSE)
                # same_click <- ifelse( click[["pointNumber"]] == last_click[5], same_click, FALSE)
            }
            else {
                # print("Se mete en else")
                last_click <<- NULL
                same_click <- FALSE 
            }
            # print("Sale de if-else")
            if ( !same_click && (x_minims[1] != k || y_minims[1] != d) ) {
                # print("-------------------")
                # print("-------------------")
                # print("Cambiando seleccion")
                # print(click)
                # print("-------------------")
                # print("-------------------")
                selected_points[k, d] <<- !selected_points[k, d]
                

            }
            
            last_click[1] <<- k
            last_click[2] <<- d
            last_click[3] <<- is.null(click[["z"]])
            
            last_click[4] <<- click[["curveNumber"]]
            # last_click[5] <<- click[["pointNumber"]]
        }
        
        #if (selected_points[k,d]) { #it wasn't selected, so just have to add that dot
        #pContour <<- add_trace(pContour, type = "scatter", mode = "markers", x = k, y = d, 
        #                       text = as.character(res$errors[k, d]), marker = list(color = "red"), 
        #                       hoverinfo = "x+y+text", showlegend = FALSE)
        #return(pContour)
        #}
        
        #it was selected, so we have to add all the dots again
        # if (input$contourType != previous_countour) {
        #     # selected_points <<- selected_points_aux
        # }
        
        if (input$contourType == "trim") {
            pContour <<- pContourTrim
            previous_countour <<- "trim"
            # selected_points_aux <<- selected_points
        }
        else if (input$contourType == "naive") {
            pContour <<- pContourNaive
            previous_countour <<- "naive"
            # selected_points_aux <<- selected_points
        }
        else {
            pContour <<- pContourBase
            previous_countour <<- "default"
            # selected_points_aux <<- selected_points
        }
        
        nDots <- ifelse(input$contourMinims == "", 5, as.numeric(input$contourMinims))

        if (nDots > 1 ) {
            texts <- "2nd best \n"
            if (nDots > 2 ) {
                texts <- c(texts, "3rd best \n")
                if (nDots > 3 ) {
                    texts <- c(texts, paste0( 4:nDots, rep("th best \n", nDots - 3) ))
                }
            }
            
            colors <- paste0( "rgba(40," , 230 - (0:(nDots-1)) * ((230 - 128) / (nDots - 1)), " ,40, 0.95)" )
            
            pContour <<- add_trace(pContour, type = "scatter", mode = "markers", x = x_minims[2:nDots], y = y_minims[2:nDots],
                                   text = paste0(texts, as.character( round(res$errors[x_minims[2:nDots], y_minims[2:nDots] ][1,], digits = 8) ) ),
                                   # marker = list(color = 0:(nDots-2), colorscale = c("seagreen", "chartreuse")), size = 8,
                                   marker = list(color = colors, size = 8),
                                   hoverinfo = "x+y+text", showlegend = FALSE, opacity = 1)
        }
        
        if (any( selected_points == TRUE ) ) {
            for (i in 1:NROW(selected_points)) {
                for (j in 1:NCOL(selected_points)) {
                    if (selected_points[i, j])
                        pContour <<- add_trace(pContour, type = "scatter", mode = "markers", x = ks[i], y = ds[j], 
                                               text = as.character(round(res$errors[i, j], digits = 8)), marker = list(color = "red"), 
                                               hoverinfo = "x+y+text", showlegend = FALSE)
                }
            }
        }
        pContour
    })
    
    
    output$optPlot <- renderPlotly({
        click <- event_data("plotly_click", source = "contour")
        
        if ( !is.null(click) ) {
            #print("Procesando click")
            #print(click)
            k <- click[[3]]
            d <- click[[4]]
            
            # selected_points <<- selected_points_aux
            # print( paste0("k=", k, " d=", d, " esta a ", selected_points[k, d], " en grafica") )
            
            # same_click <- TRUE
            if ( exists("last_click") ) {
                same_click <- k == last_click[1]
                same_click <- ifelse(d == last_click[2], same_click, FALSE)
                same_click <- ifelse( is.null(click[["z"]]) == last_click[3], same_click, FALSE)
                same_click <- ifelse( click[["curveNumber"]] == last_click[4], same_click, FALSE)
                # same_click <- ifelse( click[["pointNumber"]] == last_click[5], same_click, FALSE)
            }
            else {
                last_click <<- NULL
                same_click <- FALSE 
            }
            
            if ( !same_click && (x_minims[1] != k || y_minims[1] != d) ) {
                # print("-------------------")
                # print("-------------------")
                # print("Cambiando seleccion")
                # print(click)
                # print("-------------------")
                # print("-------------------")
                selected_points[k, d] <<- !selected_points[k, d]
                
            }
            
            last_click[1] <<- k
            last_click[2] <<- d
            last_click[3] <<- is.null(click[["z"]])
            
            last_click[4] <<- click[["curveNumber"]]
            # last_click[5] <<- click[["pointNumber"]]
        }
        
        # if (input$contourType != previous_countour) {
        #     # selected_points <<- selected_points_aux
        # }
        
        if ( all( selected_points == FALSE ) && input$chbNaiveOpt == 0 && input$chbSeasNaiveOpt == 0) {
            pErrorsOpt <<- pBarsOptBase
            if (input$chbabs_tab2 == 1) { 
                pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = abs(residuals_matrix[1, ]), showlegend = FALSE,
                                         name = "Optimal Error", legendgroup = "optim", marker = list(color = colPalette[2]))
                mini <- min( abs(residuals_matrix[1, ]) )
                maxi <- max( abs(residuals_matrix[1, ]) )
            }
            else {
                pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = residuals_matrix[1, ], showlegend = FALSE,
                                         name = "Optimal Error", legendgroup = "optim", marker = list(color = colPalette[2]))
                mini <- min( residuals_matrix[1, ])
                maxi <- max( residuals_matrix[1, ])
            }
            
            pErrorsOpt <- add_segments(pErrorsOpt, x = dates[train_init], xend = dates[train_init], y = mini - 0.05 * (maxi - mini), 
                                       yend = maxi + 0.05 * (maxi - mini), name = "Train", showlegend = FALSE, text = "Train", 
                                       hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            pErrorsOpt <- add_segments(pErrorsOpt, x = dates[test_init], xend = dates[test_init], y = mini - 0.05 * (maxi - mini), 
                                       yend = maxi + 0.05 * (maxi - mini), name = "Test", showlegend = FALSE, text = "Test", 
                                       hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            
            combPlotOpt <<- subplot(pOptBase, pErrorsOpt, nrows = 2, shareX = TRUE)
            # return(subplot(pOptBase, pErrorsOpt, nrows = 2, shareX = TRUE))
        }
        else {
            # inicializamos gráficas
            pOpt <<- pOptBase
            pErrorsOpt <<- pLinesBaseOpt
            
            # la gráfica de error depende del selector de mostrar en valor absoluto
            if (input$chbabs_tab2 == 1) { 
                pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = abs(residuals_matrix[1, ]), showlegend = FALSE,
                                         name = "Optimal Error", legendgroup = "optim", line = list(color = colPalette[2]))
                mini <- min( abs(residuals_matrix[1, ]) )
                maxi <- max( abs(residuals_matrix[1, ]) )
            }
            else {
                pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = residuals_matrix[1, ], showlegend = FALSE,
                                         name = "Optimal Error", legendgroup = "optim", line = list(color = colPalette[2]))
                mini <- min( residuals_matrix[1, ])
                maxi <- max( residuals_matrix[1, ])
            }
            
            # calculamos cuántas series se van a pintar en total
            totalSeries <- sum(selected_points)
            if (input$chbNaiveOpt == 1) {
                totalSeries <- totalSeries + 1
            }
            if (input$chbSeasNaiveOpt == 1) {
                totalSeries <- totalSeries + 1
            }
            
            # En función del número de series la gráfica de comparación con Optimo será de barras o lineas
            if (totalSeries > 1) {
                pComparOptim <<- plot_ly(type = "scatter", mode = "lines" , hoverinfo = "x+y" ) # %>% layout(title = "Errors comparison")
            }
            else {
                pComparOptim <<- plot_ly(type = "bar", hoverinfo = "x+y" ) # %>% layout(title = "Errors comparison")
            }
            
            # para pintar correctamente las barras y líneas
            min_compar <- Inf
            max_compar <- -Inf
            colorIndex <- 5
            
            # Naive activated with checkbox
            if (input$chbNaiveOpt == 1) {
                pOpt <<- add_trace(pOpt, x = sub_dates, y = naive, name = "Naive", legendgroup = "naive", line = list(color = colPalette[3]))
                if (input$chbabs_tab2 == 1) {
                    pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = abs(residuals_matrix[2, ]), line = list(color = colPalette[3]),
                                            name = "Naive error", legendgroup = "naive", showlegend = FALSE)
                }
                else {
                    pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = residuals_matrix[2, ], line = list(color = colPalette[3]),
                                            name = "Naive error", legendgroup = "naive", showlegend = FALSE)
                }
                
                dif <- abs(residuals_matrix[1, ]) - abs(residuals_matrix[2, ])
                if (totalSeries > 1) {
                    pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, line = list(color = colPalette[3]),
                                               name = "Naive comparison", legendgroup = "naive", showlegend = FALSE)
                }
                else {
                    pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, marker = list(color = colPalette[3]), 
                                               name = "Naive comparison", legendgroup = "naive", showlegend = FALSE)
                }
                
                min_compar <- min( min_compar, dif)
                max_compar <- max( max_compar, dif)
                # colorIndex <- colorIndex + 1
            }
            
            # Seasonal naive activated with checkbox
            if (input$chbSeasNaiveOpt == 1) {
                seasLag <- as.numeric(input$seasNaivOptLag)
                isolate({
                    snaive <- ts(y[(train_init - seasLag + 1):(n - seasLag)])
                })
                residuals_matrix[3, ] <- y_err - snaive
                pOpt <<- add_trace(pOpt, x = sub_dates, y = snaive, name = paste0("S. Naive (", seasLag, ")"), legendgroup = "snaive", line = list(color = colPalette[4]))
                if (input$chbabs_tab2 == 1) {
                    pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = abs(residuals_matrix[3, ]), line = list(color = colPalette[4]),
                                            name = "S. Naive error", legendgroup = "snaive", showlegend = FALSE)
                }
                else {
                    pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = residuals_matrix[3, ], line = list(color = colPalette[4]),
                                            name = "S. Naive error", legendgroup = "snaive", showlegend = FALSE)
                }
                
                dif <- abs(residuals_matrix[1, ]) - abs(residuals_matrix[3, ])
                if (totalSeries > 1) {
                    pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, line = list(color = colPalette[4]),
                                               name = "S. Naive comparison", legendgroup = "snaive", showlegend = FALSE)
                }
                else {
                    pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, marker = list(color = colPalette[4]), 
                                               name = "S. Naive comparison", legendgroup = "snaive", showlegend = FALSE)
                }
                
                min_compar <- min( min_compar, dif)
                max_compar <- max( max_compar, dif)
                # colorIndex <- colorIndex + 1
            }
            
            if (any( selected_points == TRUE ) ) {
                for (i_ind in 1:NROW(selected_points)) {
                    for (j_ind in 1:NCOL(selected_points)) {
                        if (selected_points[i_ind, j_ind]) {
                            i <- ks[i_ind]
                            j <- ds[j_ind]
                            # print(paste0("Se va a pintar la grafica para k=", i, " d=", j))
                            preds <- knn_past(y = y, k = i, d = j, initial = train_init, distance = distance, 
                                              weight = weight, threads = n_threads)
                            preds <- as.vector(preds)
                            
                            # Gráfico de predicciones 
                            pOpt <<- add_trace(pOpt, x = sub_dates, y = preds, name = paste0("k = " , i, ", d = " , j, " prediction"), 
                                               legendgroup = paste("k", i, "d", j), line = list(color = colPalette[colorIndex]))
                            
                            #Gráfico de errores 
                            error <- y_err - preds
                            if (input$chbabs_tab2 == 1) { 
                                pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = abs(error), line = list(color = colPalette[colorIndex]), showlegend = FALSE,
                                                       name = paste0("k = " , i, ", d = " , j, "", " error"), legendgroup = paste("k", i, "d", j))
                                mini <- min( mini, abs(error))
                                maxi <- max( maxi, abs(error))
                            }
                            else {
                                pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = error, line = list(color = colPalette[colorIndex]), showlegend = FALSE,
                                                       name = paste0("k = " , i, ", d = " , j, "", " error"), legendgroup = paste("k", i, "d", j))
                                mini <- min( mini, error)
                                maxi <- max( maxi, error)
                            }
                            
                            #Gráfico de comparación de errores respecto al óptimo 
                            dif <- abs(residuals_matrix[1, ]) - abs(error)
                            if (totalSeries > 1) {
                                pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, line = list(color = colPalette[colorIndex]), showlegend = FALSE,
                                                           name = paste0("k = " , i, ", d = " , j, "", " comparison"), legendgroup = paste("k", i, "d", j))
                            }
                            else {
                                pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, marker = list(color = colPalette[colorIndex]), showlegend = FALSE,
                                                       name = paste0("k = " , i, ", d = " , j, "", " comparison"), legendgroup = paste("k", i, "d", j))
                            }
                            
                            min_compar <- min( min_compar, dif)
                            max_compar <- max( max_compar, dif)
                            colorIndex <- colorIndex + 1
                        }
                    }
                }
            }
            
            pErrorsOpt <<- add_segments(pErrorsOpt, x = dates[train_init], xend = dates[train_init], y = mini - 0.05 * (maxi - mini), 
                                       yend = maxi + 0.05 * (maxi - mini), name = "Train", showlegend = FALSE, text = "Train", 
                                       hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            pErrorsOpt <<- add_segments(pErrorsOpt, x = dates[test_init], xend = dates[test_init], y = mini - 0.05 * (maxi - mini), 
                                       yend = maxi + 0.05 * (maxi - mini), name = "Test", showlegend = FALSE, text = "Test", 
                                       hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))

            pComparOptim <<- add_segments(pComparOptim, x = dates[train_init], xend = dates[train_init], y = min_compar - 0.05 * (max_compar - min_compar), 
                                       yend = max_compar + 0.05 * (max_compar - min_compar), name = "Train", showlegend = FALSE, text = "Train", 
                                       hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            pComparOptim <<- add_segments(pComparOptim, x = dates[test_init], xend = dates[test_init], y = min_compar - 0.05 * (max_compar - min_compar), 
                                       yend = max_compar + 0.05 * (max_compar - min_compar), name = "Test", showlegend = FALSE, text = "Test", 
                                       hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            
            combPlotOpt <<- subplot(pOpt, pErrorsOpt, pComparOptim, nrows = 3, shareX = TRUE )
            # return(subplot(pOpt, pErrorsOpt, pComparOptim, nrows = 3, shareX = TRUE))
        }
        
        combPlotOpt
        
    })
    
 
    output$table_OptimTab <- renderDataTable({
        
        click <- event_data("plotly_click", source = "contour")
        
        if (!is.null(click)) {
            #print("Procesando click")
            #print(click)
            k = click[[3]]
            d = click[[4]]
            
            # selected_points <<- selected_points_aux
            
            # print( paste0("k=", k, " d=", d, " esta a ", selected_points[k, d], " en tabla") )
        } 
        
        # if (input$contourType != previous_countour) {
        #     # selected_points <<- selected_points_aux
        # }
        
        names_col_local <- c(names_col[1])
        errors_matrix_local <- matrix(errors_matrix[1, ], nrow = 1)

        # Naive activated with checkbox
        if (input$chbNaiveOpt == 1) {
            names_col_local <- c(names_col_local, names_col[2])
            errors_matrix_local <- rbind(errors_matrix_local, errors_matrix[2, ])
        }
        
        # Seasonal naive activated with checkbox
        if (input$chbSeasNaiveOpt == 1) {
            seasLag <- as.numeric(input$seasNaivOptLag)
            names_col_local <- c(names_col_local, paste0(names_col[3], " (", seasLag, ")") )
            isolate({
                snaive <- ts(y[(train_init - seasLag + 1):(n - seasLag)])
            })
            train_error <- accuracy(snaive[1:length(y_train_err)], y_train_err)
            test_error <- accuracy(snaive[(length(y_train_err) + 1):length(snaive)], y_test_err)
            # errors_matrix_tab2[3, ] <- c(train_error, test_error)
            errors_matrix_local <- rbind(errors_matrix_local, c(train_error, test_error))
        }
        
        for (i_ind in 1:NROW(selected_points)) {
            for (j_ind in 1:NCOL(selected_points)) {
                if (selected_points[i_ind, j_ind]) {
                    i <- ks[i_ind]
                    j <- ds[j_ind]
                    names_col_local <- c(names_col_local, paste0("k = " , i, ", d = " , j))
                    preds <- knn_past(y = y, k = i, d = j, initial = train_init, distance = distance, 
                                      weight = weight, threads = n_threads)
                    train_error <- accuracy(ts(preds[1:length(y_train_err)]), y_train_err)
                    test_error <- accuracy(ts(preds[(length(y_train_err) + 1):length(preds)]), y_test_err)
                    errors_aux <- c(train_error, test_error)
                    errors_matrix_local <- rbind(errors_matrix_local, c(train_error, test_error))
                }
            }
        }
        
        DT::datatable(data.frame(
            Name = names_col_local, trainME = round(errors_matrix_local[, 1], digits = 8), trainRMSE = round(errors_matrix_local[, 2], 8),
            trainMAE = round(errors_matrix_local[, 3], digits = 8), testME = round(errors_matrix_local[, 6], digits = 8),
            testRMSE = round(errors_matrix_local[, 7], 8), testMAE = round(errors_matrix_local[, 8], digits = 8)
        ), colnames = c("Name", "ME (train)", "RMSE (train)", "MAE (train)", "ME (test)", "RMSE (test)", "MAE (test)"))

        
    })
    
    
    
} 

ui <- navbarPage("",
                 tabPanel("Distances",
                          # fluidPage(
                          headerPanel("Distances between elements"),
                          mainPanel(width = 10,
                              plotlyOutput("elemsPlot")
                              # plotlyOutput("neighborsPlot"),
                              # plotlyOutput("distsPlot")
                          ),
                          sidebarPanel(width = 2,
                              tags$head(
                                  tags$style(HTML("hr {border-top: 1px solid #cbcbcb;}"))
                              ),
                              # checkboxInput("chbsnaive", label = "Seasonal Naive", value = FALSE), 
                              textInput(inputId = "selKtabDist", label = "K", value = res$opt_k, 
                                        width = NULL, placeholder = "Value for K (blank for Optimal)"),
                              textInput(inputId = "selDtabDist", label = "D", value = res$opt_d, 
                                        width = NULL, placeholder = "Value for D (blank for Optimal)"),
                              # actionButton("browse", "Browse"),
                              hr(),
                              materialSwitch(inputId = "chbabsDist", label = "Absolute Error", value = TRUE, status = "primary")
                              
                              
                              #prettyCheckbox("chbabs", label = "Absolute Error", value = FALSE, thick = TRUE, shape = "curve", bigger = TRUE)
                              
                          )
                          # ,headerPanel("k-Nearest Neighboors")
                          ,mainPanel(width = 8,
                                    # plotlyOutput("elemsPlot"),
                                    plotlyOutput("neighborsPlot"),
                                    plotlyOutput("distsPlot")
                          )
                          
                 ),
                 
                 tabPanel("Errors",
                          fluidPage(
                              headerPanel("Time Series and Predictions"),
                              mainPanel(
                                  plotlyOutput("errorsPlot")
                              ),
                              sidebarPanel(
                                  tags$head(
                                      tags$style(HTML("hr {border-top: 1px solid #cbcbcb;}"))
                                  ),
                                  checkboxInput("chbnaive", label = "Naive", value = TRUE),
                                  hr(),
                                  checkboxInput("chbsnaive", label = "Seasonal Naive", value = FALSE), 
                                  textInput("s", "Lag:", value = 12),
                                  hr(),
                                  checkboxInput("chbload", label = "Custom", value = FALSE),
                                  textInput("path", "File:", placeholder = "Absolute path to the file"),
                                  actionButton("browse", "Browse"),
                                  hr(),
                                  materialSwitch(inputId = "chbabs", label = "Absolute Error", value = TRUE, status = "primary")
                                  
                                  
                                  #prettyCheckbox("chbabs", label = "Absolute Error", value = FALSE, thick = TRUE, shape = "curve", bigger = TRUE)
                                  
                              ),
                              
                              headerPanel("Errors Table"),
                              sidebarPanel(
                                  DT::dataTableOutput("table_tab1"),
                                  width = 10
                              )
                          )
                          
                 ),
                 
                 
                 tabPanel("Optimization",
                          headerPanel(HTML(paste0("Errors for each <em>k</em> and <em>d</em> (", error_measure, " error)"))),
                          mainPanel( width = 10,
                              plotlyOutput("contourPlot")
                          ),
                          sidebarPanel(width = 2,
                              radioButtons(inputId = "contourType", label = "Type of contour", selected = "default",
                                           choices = list("Default" = "default", "Contour lines under Naive" = "naive", 
                                                          "Top-values color trimmed" = "trim")),
                              hr(),
                              textInput(inputId = "contourMinims", label = "Number of minimums to plot:", 
                                        value = 5, placeholder = "Leave blank to default (5)")
                          ),
                          headerPanel("Time Series and Predictions"),
                          mainPanel( width = 10,
                              plotlyOutput("optPlot") 
                          ),
                          sidebarPanel( width = 2, 
                              tags$head( tags$style(HTML("hr {border-top: 1px solid #cbcbcb;}")) ),
                              checkboxInput("chbNaiveOpt", label = "Naive", value = FALSE),
                              hr(),
                              checkboxInput("chbSeasNaiveOpt", label = "Seasonal Naive", value = FALSE), 
                              textInput(inputId = "seasNaivOptLag", label = "Lag:", value = 12),
                              hr(),
                              materialSwitch(inputId = "chbabs_tab2", label = "Absolute Error", value = TRUE, status = "primary")
                          ),
                          headerPanel("Errors Table"),
                          sidebarPanel(
                              DT::dataTableOutput("table_OptimTab"),
                              width = 11
                          )
                 )
)

# Now run the following: 
runApp(shinyApp(server = server, ui = ui, options = list("quiet")))