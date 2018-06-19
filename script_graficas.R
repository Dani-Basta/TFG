library(plotly)
library(shiny)
library(shinyWidgets)
library(DT)

#                                     Plots for main tab
pMain <- plot_ly(x = dates, y = y, type = "scatter",  name = "Real Time Series", mode = "lines", legendgroup = "real", hoverinfo = "x+y")
pMain <- add_trace(pMain, x = sub_dates, y = optimal, name = paste0("Optimal (k = ", res$k, ", d = ", res$d, ")"), legendgroup = "optim")
# pMain <- add_trace(pMain, x = sub_dates, y = naive, name = "Naive", legendgroup = "naive")
# Separation lines for train and test
pMain <- add_segments(pMain, x = dates[train_init], xend = dates[train_init], y = min_y - 0.05 * (max_y - min_y), 
                      yend = max_y + 0.05 * (max_y - min_y), name = "Train", showlegend = FALSE, text = "Train", 
                      hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
pMain <- add_segments(pMain, x = dates[test_init], xend = dates[test_init], y = min_y - 0.05 * (max_y - min_y), 
                      yend = max_y + 0.05 * (max_y - min_y), name = "Test", showlegend = FALSE, text = "Test", 
                      hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
pMain <- layout(pMain, xaxis = list(rangeslider = list(type = "date")))
pMainBase <- pMain

#Errors
pErrMain <- plot_ly(x = sub_dates, y = residuals_matrix[1, ], name = "Optimal error", type = "scatter", mode = "markers", legendgroup = "optim", hoverinfo = "x+y")
#pErrMain <- add_trace(pErrMain, x = sub_dates, y = residuals_matrix[2, ], name = "Naive error", legendgroup = "naive") 

combPlotMain <- subplot(pMain, pErrMain, nrows = 2, shareX = TRUE)




#                               Plots for optimization tab
pOpt <- plot_ly(x = dates, y = y, type = "scatter",  name = "Real Time Series", mode = "lines", legendgroup = "real", hoverinfo = "x+y")
pOpt <- add_trace(pOpt, x = sub_dates, y = optimal, name = paste0("Optimal (k = ", res$k, ", d = ", res$d, ")"), legendgroup = "optim")
pOpt <- add_segments(pOpt, x = dates[train_init], xend = dates[train_init], y = min_y - 0.05 * (max_y - min_y), 
                     yend = max_y + 0.05 * (max_y - min_y), name = "Train", showlegend = FALSE, text = "Train", 
                     hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
pOpt <- add_segments(pOpt, x = dates[test_init], xend = dates[test_init], y = min_y - 0.05 * (max_y - min_y), 
                     yend = max_y + 0.05 * (max_y - min_y), name = "Test", showlegend = FALSE, text = "Test", 
                     hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
pOpt <- layout(pOpt, xaxis = list(rangeslider = list(type = "date")))

pOptBase <- pOpt

# Error bars
pBarsOpt <- plot_ly(type = "scatter", mode = "markers", hoverinfo = "x+y")
#pBarsOpt <- add_trace(pBarsOpt, x = sub_dates, y = residuals_matrix[1, ], name = "Optimal error", type = "scatter", mode = "markers", legendgroup = "optim", hoverinfo = "x+y")
pBarsOptBase <- pBarsOpt
pBarsOpt <- add_trace(pBarsOpt, x = sub_dates, y = residuals_matrix[1, ], name = "Optimal error", 
                      type = "scatter", mode = "markers", legendgroup = "optim", hoverinfo = "x+y")

combPlotOpt <- subplot(pOpt, pBarsOpt, nrows = 2, shareX = TRUE)


#                                     Contour
#Default
pContourBase <- plot_ly(x = ks , y = ds, z = res$errors, transpose = TRUE, type = "contour", source = "contour", 
                        autocontour = TRUE, contours = list(showlabels = TRUE, coloring = "heatmap"), colorscale = "Jet",
                        ncontours = 4*num_contours, hoverinfo = "x+y+z")
pContourBase <- layout(pContourBase, xaxis = list(title = "k"), yaxis = list(title = "d") )
pContourBase <- add_trace(pContourBase, type = "scatter", mode = "markers", x = x_minims[1], y = y_minims[1], 
                          text = as.character(res$errors[x_minims[1], y_minims[1] ]), 
                          marker = list(color = "green"), hoverinfo = "x+y+text", showlegend = FALSE)
pContourBase <- add_trace(pContourBase, type = "scatter", mode = "markers", x = x_minims[2:5], y = y_minims[2:5], 
                          text = as.character(res$errors[x_minims[2:5], y_minims[2:5] ][,1]), 
                          marker = list(color = "orange"), hoverinfo = "x+y+text", showlegend = FALSE)

#Top-values trimmed
pContourTrim <- plot_ly(x = ks , y = ds, z = res$errors, transpose = TRUE, type = "contour", source = "contour", 
                        colorscale = "Jet", contours = list(showlabels = TRUE, coloring = "heatmap", 
                                        start = cont_min, end = (cont_max_fix + cont_min)/2, 
                                        size = (((cont_max_fix+cont_min)/2)-cont_min)/num_contours),
                        zmin = cont_min, zmax = cont_max_fix, hoverinfo = "x+y+z")
pContourTrim <- layout(pContourTrim, xaxis = list(title = "k"), yaxis = list(title = "d") )
pContourTrim <- add_trace(pContourTrim, type = "scatter", mode = "markers", x = x_minims[1], y = y_minims[1], text = as.character(res$errors[x_minims[1], y_minims[1] ]), marker = list(color = "green"), hoverinfo = "x+y+text", showlegend = FALSE)
pContourTrim <- add_trace(pContourTrim, type = "scatter", mode = "markers", x = x_minims[2:5], y = y_minims[2:5], text = as.character(res$errors[x_minims[2:5], y_minims[2:5] ][,1]), marker = list(color = "orange"), hoverinfo = "x+y+text", showlegend = FALSE)

pContour <- pContourBase

