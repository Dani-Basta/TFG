library(plotly)
library(shiny)
library(DT)

#                                     Plots for main tab
pMain <- plot_ly(x = dates, y = y, type = "scatter",  name = "Real", mode = "lines", legendgroup = "real", hoverinfo = "x+y")
pMain <- add_trace(pMain, x = sub_dates, y = euc_prox, name = paste("Optimal k =", res$k, "d =", res$d), legendgroup = "optim")
pMain <- add_trace(pMain, x = sub_dates, y = naive, name = "Naive", legendgroup = "naive")
# Separation lines for train and test
pMain <- add_segments(pMain, x = dates[train_init], xend = dates[train_init], y = min_x - 0.10 * (max_x - min_x), 
                      yend = max_x + 0.10 * (max_x - min_x), name = "Train", showlegend = FALSE, text = "Train",  hoverinfo = "text", line = list(color = "gray", width = 1.5, dash = "dash"))
pMain <- add_segments(pMain, x = dates[test_init], xend = dates[test_init], y = min_x - 0.10 * (max_x - min_x), 
                      yend = max_x + 0.10 * (max_x - min_x), name = "Test", showlegend = FALSE, text = "Test",  hoverinfo = "text", line = list(color = "gray", width = 1.5, dash = "dash"))
pMain <- layout(pMain, xaxis = list(rangeslider = list(type = "date")))

#Errors
#If we have more than 1 error to show is more legible if we use a line plot instead of a bar one
pErrMain <- plot_ly(x = sub_dates, y = residuals_matrix[1, ], name = "Optimal error", type = "scatter", mode = "markers", legendgroup = "optim", hoverinfo = "x+y")
pErrMain <- add_trace(pErrMain, x = sub_dates, y = residuals_matrix[2, ], name = "Naive error", legendgroup = "naive") 

combPlotMain <- subplot(pMain, pErrMain, nrows = 2, shareX = TRUE)




#                               Plots for optimization tab
pOpt <- plot_ly(x = dates, y = y, type = "scatter",  name = "Real", mode = "lines", legendgroup = "real", hoverinfo = "x+y")
pOpt <- add_trace(pOpt, x = sub_dates, y = euc_prox, name = paste("Optimal k =", res$k, "d =", res$d), legendgroup = "optim")
pMain <- add_segments(pMain, x = dates[train_init], xend = dates[train_init], y = min_x - 0.10 * (max_x - min_x), 
                      yend = max_x + 0.10 * (max_x - min_x), name = "Train", showlegend = FALSE, text = "Train",  hoverinfo = "text", line = list(color = "gray", width = 1.5, dash = "dash"))
pMain <- add_segments(pMain, x = dates[test_init], xend = dates[test_init], y = min_x - 0.10 * (max_x - min_x), 
                      yend = max_x + 0.10 * (max_x - min_x), name = "Test", showlegend = FALSE, text = "Test",  hoverinfo = "text", line = list(color = "gray", width = 1.5, dash = "dash"))

pOpt <- layout(pOpt, xaxis = list(rangeslider = list(type = "date")))
pOptBase <- pOpt

# Error bars
pBarsOpt <- plot_ly(x = sub_dates, y = abs(x_err - euc_prox), name = "Optimal error", type = "scatter", mode = "markers", legendgroup = "optim", hoverinfo = "x+y")
pBarsOptBase <- pBarsOpt

combPlotOpt <- subplot(pOpt, pBarsOpt, nrows = 2, shareX = TRUE)


#                                     Contour
pContourBase <- plot_ly(x = ks , y = ds, z = res$errors, transpose = TRUE, type = "contour", 
                        #Just indicate the number of line contours to plot
                        #autocontour = TRUE, contours = list(showlabels = TRUE, coloring = "heatmap") , ncontours = num_contours ,
                        
                        #version saying where to start doing lines, space between lines and where to stop
                        contours = list(showlabels = TRUE, coloring = "heatmap", start = cont_min, end = (cont_max_fix + cont_min)/2, size = (((cont_max_fix+cont_min)/2)-cont_min)/num_contours),
                        
                        zmin = cont_min, zmax = cont_max_fix, hoverinfo = "x+y+z")
pContourBase <- layout(pContourBase, xaxis = list(title = "k"), yaxis = list(title = "d") )
pContourBase <- add_trace(pContourBase, type = "scatter", mode = "markers", x = x_minims[1], y = y_minims[1], text = as.character(res$errors[x_minims[1], y_minims[1] ]), marker = list(color = "green"), hoverinfo = "x+y+text", showlegend = FALSE)
pContourBase <- add_trace(pContourBase, type = "scatter", mode = "markers", x = x_minims[2:5], y = y_minims[2:5], text = as.character(res$errors[x_minims[2:5], y_minims[2:5] ][,1]), marker = list(color = "orange"), hoverinfo = "x+y+text", showlegend = FALSE)

pContour <- pContourBase
