library(plotly)
library(shiny)
library(shinyWidgets)
library(DT)
library(rdist)

colPalette <- c("royalblue", "seagreen", "orange", "aquamarine", "blueviolet", "darkred", "peru","darkcyan" , "salmon")

#                                     Plots for main tab
pMain <- plot_ly(x = dates, y = y, type = "scatter",  name = "Observed Time Series", mode = "lines", source = "main", 
                 legendgroup = "real", hoverinfo = "x+y" , line = list(color = colPalette[1]))

# pMain <- add_trace(pMain, x = sub_dates, y = naive, name = "Naive", legendgroup = "naive")

# Separation lines for train and test
# pMain <- add_segments(pMain, x = dates[train_init], xend = dates[train_init], y = min_y - 0.05 * (max_y - min_y),
#                       yend = max_y + 0.05 * (max_y - min_y), name = "Train", showlegend = FALSE, text = "Train",
#                       hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
# pMain <- add_segments(pMain, x = dates[test_init], xend = dates[test_init], y = min_y - 0.05 * (max_y - min_y),
#                       yend = max_y + 0.05 * (max_y - min_y), name = "Test", showlegend = FALSE, text = "Test",
#                       hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))

# train superior
pMain <- add_segments(pMain, x = dates[train_init+1], xend = dates[test_init], y = 1.05 * max(y[(train_init+1):test_init]),
                      yend = 1.05 * max(y[(train_init+1):test_init]), name = "Train", showlegend = FALSE, text = paste0("Train (", dates[train_init+1]," / ", dates[test_init], ")"),
                      hoverinfo = "text", legendgroup = "lines", line = list(color = colPalette[1], width = 1.5, dash = "dash"))
# train inferior
pMain <- add_segments(pMain, x = dates[train_init+1], xend = dates[test_init], y = 0.95 * min(y[(train_init+1):test_init]),
                      yend = 0.95 * min(y[(train_init+1):test_init]), name = "Train", showlegend = FALSE, paste0("Train (", dates[train_init+1]," / ", dates[test_init], ")"),
                      hoverinfo = "text", legendgroup = "lines", line = list(color = colPalette[1], width = 1.5, dash = "dash"))
# test superior
pMain <- add_segments(pMain, x = dates[test_init+1], xend = dates[length(dates)], y = 1.05 * max(y[(test_init+1):n]),
                      yend = 1.05 * max(y[(test_init+1):n]), name = "Test", showlegend = FALSE, text = paste0("Test (", dates[test_init+1]," / ", dates[length(dates)], ")"),
                      hoverinfo = "text", legendgroup = "lines", line = list(color = colPalette[1], width = 1.5, dash = "dash"))
# test inferior
pMain <- add_segments(pMain, x = dates[test_init+1], xend = dates[length(dates)], y = 0.95 * min(y[(test_init+1):n]),
                      yend = 0.95 * min(y[(test_init+1):n]), name = "Test", showlegend = FALSE, text = paste0("Test (", dates[test_init+1]," / ", dates[length(dates)], ")"),
                      hoverinfo = "text", legendgroup = "lines", line = list(color = colPalette[1], width = 1.5, dash = "dash"))

pMainShapes <- list(
    list(type = "rect",
         fillcolor = colPalette[1], line = list(color = colPalette[1]), opacity = 0.1,
         x0 = dates[train_init+1], x1 = dates[test_init], xref = "x", yref = "y", 
         # y0 = min_y - 0.05 * (max_y - min_y), y1 = max_y + 0.05 * (max_y - min_y)),
         y0 = 0.95 * min(y[(train_init+1):test_init]), y1 = 1.05 * max(y[(train_init+1):test_init]) ),
    list(type = "rect",
         fillcolor = colPalette[1], line = list(color = colPalette[1]), opacity = 0.15,
         x0 = dates[test_init], x1 = dates[length(dates)], xref = "x", yref = "y",
         # y0 = min_y - 0.05 * (max_y - min_y), y1 = max_y + 0.05 * (max_y - min_y))))
         y0 = 0.95 * min(y[(test_init+1):n]), y1 = 1.05 * max(y[(test_init+1):n]) ) ) 

pMainLayout <- pMain

pMain <- layout(pMain, xaxis = list(range = list( dates[1], dates[length(dates)]), 
                rangeslider = list(range = list( dates[1], dates[length(dates)]) )), shapes = pMainShapes)

pMainBase <- pMain


pMain <- add_trace(pMain, x = sub_dates, y = optimal$mean, legendgroup = "optim", 
                   name = paste0("Optimal (k = ", res$opt_k, ", d = ", res$opt_d, ")"), line = list(color = colPalette[2]))

#Errors
pErrMain <- plot_ly(x = sub_dates, y = residuals_matrix[1, ], legendgroup = "optim", hoverinfo = "x+y", 
                    name = "Optimal error", type = "scatter", mode = "markers")
#pErrMain <- add_trace(pErrMain, x = sub_dates, y = residuals_matrix[2, ], name = "Naive error", legendgroup = "naive") 

combPlotMain <- subplot(pMain, pErrMain, nrows = 2, shareX = TRUE )





#                               Plots for optimization tab
pOpt <- plot_ly(x = dates, y = y, type = "scatter",  name = "Observed Time Series", 
                mode = "lines", legendgroup = "real", hoverinfo = "x+y", line = list(color = colPalette[1]))
pOpt <- add_trace(pOpt, x = sub_dates, y = optimal$mean, legendgroup = "optim", 
                  name = paste0("Optimal (k = ", res$opt_k, ", d = ", res$opt_d, ")"), line = list(color = colPalette[2]))

pOpt <- add_segments(pOpt, x = dates[train_init], xend = dates[train_init], y = min_y - 0.05 * (max_y - min_y), 
                     yend = max_y + 0.05 * (max_y - min_y), name = "Train", showlegend = FALSE, text = "Train", 
                     hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
pOpt <- add_segments(pOpt, x = dates[test_init], xend = dates[test_init], y = min_y - 0.05 * (max_y - min_y), 
                     yend = max_y + 0.05 * (max_y - min_y), name = "Test", showlegend = FALSE, text = "Test", 
                     hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
pOpt <- layout(pOpt, xaxis = list(rangeslider = list(visible = TRUE)))

pOptBase <- pOpt

# Error bars
pErrorsOpt <- plot_ly(type = "bar", hoverinfo = "x+y") %>% layout(title = "Prediction errors")
#pBarsOpt <- add_trace(pErrossOpt, x = sub_dates, y = residuals_matrix[1, ], name = "Optimal error", type = "scatter", mode = "markers", legendgroup = "optim", hoverinfo = "x+y")
pBarsOptBase <- pErrorsOpt
# pErrossOpt <- add_trace(pErrossOpt, x = sub_dates, y = abs(residuals_matrix[1, ]), name = "Optimal", 
#                       type = "scatter", mode = "markers", legendgroup = "optim")

combPlotOpt <- subplot(pOpt, pBarsOptBase, nrows = 2, shareX = TRUE)

pLinesBaseOpt <- plot_ly(type = "scatter", mode = "lines" , hoverinfo = "x+y" ) %>% layout(title = "Prediction errors")






#                                     Contour
#Default
pContourBase <- plot_ly(x = ks , y = ds, z = res$errors, transpose = TRUE, type = "contour", source = "contour", 
                        colorscale = "Jet", contours = list(showlabels = TRUE, coloring = "heatmap", 
                                                            start = cont_min, end = cont_max,
                                                            size = (cont_max - cont_min)/(4*num_contours)),
                        hoverinfo = "x+y+z")
pContourBase <- layout(pContourBase, xaxis = list(title = "k"), yaxis = list(title = "d") )
pContourBase <- layout(pContourBase, xaxis = list(title = list(text = "k")), yaxis = list(title = list(text = "d")) )
pContourBase <- add_trace(pContourBase, type = "scatter", mode = "markers", x = x_minims[1], y = y_minims[1],
                          text = paste0("Best combination\n", as.character( round(res$errors[x_minims[1], y_minims[1] ], digits = 8) ) ),
                          marker = list(color = "rgb(0, 250, 0)", size = 10), hoverinfo = "x+y+text", showlegend = FALSE)
# pContourBase <- add_trace(pContourBase, type = "scatter", mode = "markers", x = x_minims[2:5], y = y_minims[2:5], 
#                           text = paste0(c("2nd", "3rd", "4th", "5th"), " better \n", as.character(res$errors[x_minims[2:5], y_minims[2:5] ][1,]) ), 
#                           marker = list(color = "orange"), hoverinfo = "x+y+text", showlegend = FALSE)

#Worse values than Naive trimmmed
pContourNaive <- plot_ly(x = ks , y = ds, z = res$errors, transpose = TRUE, type = "contour", source = "contour", 
                        colorscale = "Jet", contours = list(showlabels = TRUE, coloring = "heatmap", 
                                                            start = cont_min, end = naive_total_error),
                        zmin = cont_min, zmax = cont_max, hoverinfo = "x+y+z")
pContourNaive <- layout(pContourNaive, xaxis = list(title = "k"), yaxis = list(title = "d") )
pContourNaive <- add_trace(pContourNaive, type = "scatter", mode = "markers", x = x_minims[1], y = y_minims[1],
                           text = paste0("Best combination\n", as.character( round(res$errors[x_minims[1], y_minims[1] ], digits = 8) ) ),
                           marker = list(color = "rgb(0, 250, 0)", size = 10), hoverinfo = "x+y+text", showlegend = FALSE)
# pContourNaive <- add_trace(pContourNaive, type = "scatter", mode = "markers", x = x_minims[2:5], y = y_minims[2:5], 
#                           text = paste0(c("2nd", "3rd", "4th", "5th"), " better \n", as.character(res$errors[x_minims[2:5], y_minims[2:5] ][1,]) ), 
#                           marker = list(color = "orange"), hoverinfo = "x+y+text", showlegend = FALSE)

#Top-values trimmed
pContourTrim <- plot_ly(x = ks , y = ds, z = res$errors, transpose = TRUE, type = "contour", source = "contour", 
                        colorscale = "Jet", contours = list(showlabels = TRUE, coloring = "heatmap", 
                                                            start = cont_min, end = (cont_max_fix + cont_min)/2, 
                                                            size = (((cont_max_fix + cont_min)/2) - cont_min)/num_contours),
                        zmin = cont_min, zmax = cont_max_fix, hoverinfo = "x+y+z")
pContourTrim <- layout(pContourTrim, xaxis = list(title = "k"), yaxis = list(title = "d") )
pContourTrim <- add_trace(pContourTrim, type = "scatter", mode = "markers", x = x_minims[1], y = y_minims[1], 
                          text = paste0("Best combination\n", as.character( round(res$errors[x_minims[1], y_minims[1] ], digits = 8) ) ),
                          marker = list(color = "rgb(0, 250, 0)", size = 10), hoverinfo = "x+y+text", showlegend = FALSE)
# pContourTrim <- add_trace(pContourTrim, type = "scatter", mode = "markers", x = x_minims[2:5], 
#                           y = y_minims[2:5], text = paste0(c("2nd", "3rd", "4th", "5th"), " better \n", as.character(res$errors[x_minims[2:5], y_minims[2:5] ][1,]) ), 
#                           marker = list(color = seq(0, 1), colorscale = "[[0, 'rgb(0,255,0)'], [1, 'rgb(0,50,0)']]", size = 8), hoverinfo = "x+y+text", showlegend = FALSE)

pContour <- pContourBase

