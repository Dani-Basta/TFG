library(plotly)
library(shiny)
library(DT)

#                                     Plots for main tab
pMain <- plot_ly(x = dates, y = x, type = "scatter",  name = "Real", mode = "lines", legendgroup = "real", hoverinfo = "x+y")
pMain <- add_trace(pMain, x = sub_dates, y = euc_prox, name = "Optimal", legendgroup = "optim")
pMain <- add_trace(pMain, x = sub_dates, y = naive, name = "Naive", legendgroup = "naive") 
#pMain <- add_trace(pMain, x = sub_dates, y = exp_smoothing, name = "Exp Smooth", legendgroup = "pred")
pMain <- add_segments(pMain, x = dates[train_init], xend = dates[train_init], y = min_x - 0.10 * (max_x - min_x), 
                      yend = max_x + 0.10 * (max_x - min_x), name = "Train", showlegend = FALSE, 
                      hoverinfo = "all", legendgroup = "lines" )
pMain <- add_segments(pMain, x = dates[test_init], xend = dates[test_init], y = min_x - 0.10 * (max_x - min_x), 
                      yend = max_x + 0.10 * (max_x - min_x), name = "Test", showlegend = FALSE, 
                      hoverinfo = "all", legendgroup = "lines")

pMain <- layout(pMain, xaxis = list(rangeslider = list(type = "date")))

#                       Normal errors
#If we have more than 1 error to show is more legible if we use a line plot instead of a bar one
#pErrMain <- 
#plot_ly(x = sub_dates, y = x_err - euc_prox, name = "Proximity error", type = "bar"), legendgroup = "error"
#plot_ly(x = sub_dates, y = x_err - euc_prox, name = "Proximity error", type = "scatter", mode = "lines", legendgroup = "error")
#pErrMain <- add_trace(pErrMain, x = sub_dates, y = x_err - naive, name = "Naive error", legendgroup = "error") 
#pErrMain <- add_trace(pErrMain, x = sub_dates, y = x_err - exp_smoothing, name = "Exponential Smoothing error", legendgroup = "error")
#In series with very high values and low errors, graphic can be nearly invisible if scalated to the series
#pErrMain <- layout(pErrMain, yaxis = list( range = c( max(x_err)*-1, max(x_err) ) ))


#                       Absolute errors
#If we have more than 1 error to show is more legible if we use a line plot instead of a bar one
pErrMain <- 
    #plot_ly(x = sub_dates, y = abs(x_err - euc_prox), name = "Proximity error", type = "bar", legendgroup = "error")
    plot_ly(x = sub_dates, y = abs(x_err - euc_prox), name = "Optimal error", type = "scatter", mode = "markers", legendgroup = "optim", hoverinfo = "x+y")

pErrMain <- add_trace(pErrMain, x = sub_dates, y = abs(x_err - naive), name = "Naive error", legendgroup = "naive") 
#pErrMain <- add_trace(pErrMain, x = sub_dates, y = abs(x_err - exp_smoothing), name = "Exp Smooth error", legendgroup = "error")


combPlotMain <- subplot(pMain, pErrMain, nrows = 2, shareX = TRUE)




#                               Plots for optimization tab
pOpt <- plot_ly(x = dates, y = x, type = "scatter",  name = "Real", mode = "lines", legendgroup = "real", hoverinfo = "x+y")
pOpt <- add_trace(pOpt, x = sub_dates, y = euc_prox, name = "Optim", legendgroup = "optim")
pOpt <- add_segments(pOpt, x = dates[train_init], xend = dates[train_init], y = min_x - 0.10 * (max_x - min_x), legendgroup = "lines", 
                     yend = max_x + 0.10 * (max_x - min_x), name = "Train", showlegend = FALSE, hoverinfo = "all")
pOpt <- add_segments(pOpt, x = dates[test_init], xend = dates[test_init], y = min_x - 0.10 * (max_x - min_x), legendgroup = "lines", 
                     yend = max_x + 0.10 * (max_x - min_x), name = "Test", showlegend = FALSE, hoverinfo = "all") 

pOpt <- layout(pOpt, xaxis = list(rangeslider = list(type = "date")))
pOptBase <- pOpt

# Error bars
pBarsOpt <- plot_ly(x = sub_dates, y = abs(x_err - euc_prox), name = "Optim error", type = "scatter", mode = "markers", legendgroup = "optim", hoverinfo = "x+y")
pBarsOptBase <- pBarsOpt

combPlotOpt <- subplot(pOpt, pBarsOpt, nrows = 2, shareX = TRUE)

pContourBase <- plot_ly(x = ks , y = ds, z = res$errors, transpose = TRUE, type = "contour", 
                        #Just indicate the number of line contours to plot
                        #autocontour = TRUE, contours = list(showlabels = TRUE, coloring = "heatmap") , ncontours = num_contours ,
                        
                        #version saying where to start doing lines, space between lines and where to stop
                        contours = list(showlabels = TRUE, coloring = "heatmap", start = cont_min, end = (cont_max_fix + cont_min)/2, size = (((cont_max_fix+cont_min)/2)-cont_min)/num_contours),
                        
                        zmin = cont_min, zmax = cont_max_fix, hoverinfo = "x+y+z")
pContourBase <- layout(pContourBase, xaxis = list(title = "K"), yaxis = list(title = "D") )
###provisional
#(pContour, type = "scatter", mode = "markers",  x = x_minims, y = y_minims, marker = list(color = "red"), showlegend = FALSE)

pContourBase <- add_trace(pContourBase, type = "scatter", mode = "markers", x = x_minims[1], y = y_minims[1], text = as.character(res$errors[x_minims[1], y_minims[1] ]), marker = list(color = "green"), hoverinfo = "x+y+text", showlegend = FALSE)
pContourBase <- add_trace(pContourBase, type = "scatter", mode = "markers", x = x_minims[2:5], y = y_minims[2:5], text = as.character(res$errors[x_minims[2:5], y_minims[2:5] ][,1]), marker = list(color = "orange"), hoverinfo = "x+y+text", showlegend = FALSE)

pContour <- pContourBase
