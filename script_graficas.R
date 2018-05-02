library(plotly)
library(shiny)
library(DT)

#                                     Plots for main tab
pMain <- plot_ly(x = dates, y = x, type = "scatter",  name = "Real", mode = "lines")
pMain <- add_trace(pMain, x = sub_dates, y = euc_prox, name = "Proximity")
pMain <- add_trace(pMain, x = sub_dates, y = naive, name = "Naive") 
pMain <- add_trace(pMain, x = sub_dates, y = exp_smoothing, name = "Exponential Smoothing")
pMain <- add_segments(pMain, x = dates[train_init], xend = dates[train_init], y = min_x - 0.10 * (max_x - min_x), yend = max_x + 0.10 * (max_x - min_x), name = "Train")
pMain <- add_segments(pMain, x = dates[test_init], xend = dates[test_init], y = min_x - 0.10 * (max_x - min_x), yend = max_x + 0.10 * (max_x - min_x), name = "Test")

pMain <- layout(pMain, title = "Prediccion con distancia Euclídea", xaxis = list(rangeslider = list(type = "date")))

# Error bars
pBarsMain <- plot_ly(x = sub_dates, y = x_err - euc_prox, name = "Proximity error", type = "bar")
pBarsMain <- add_trace(pBarsMain, x = sub_dates, y = x_err - naive, name = "Naive error") 
pBarsMain <- add_trace(pBarsMain, x = sub_dates, y = x_err - exp_smoothing, name = "Exponential Smoothing error") 
pBarsMain <- layout(pBarsMain, yaxis = list( range = c( max(x_err)*-1, max(x_err) ) ))
combPlotMain <- subplot(pMain, pBarsMain, nrows = 2, shareX = TRUE)

#                               Plots for optimization tab
pOpt <- plot_ly(x = dates, y = x, type = "scatter",  name = "Real", mode = "lines")
pOpt <- add_trace(pOpt, x = sub_dates, y = euc_prox, name = "Proximity")
pOpt <- add_segments(pOpt, x = dates[init], xend = dates[init], y = min_x - 0.10 * (max_x - min_x), yend = max_x + 0.10 * (max_x - min_x), name = "Train")
pOpt <- add_segments(pOpt, x = dates[test_init], xend = dates[test_init], y = min_x - 0.10 * (max_x - min_x), yend = max_x + 0.10 * (max_x - min_x), name = "Test")


pOpt <- layout(pOpt, title = "Prediccion con distancia Euclídea", xaxis = list(rangeslider = list(type = "date")))

# Error bars
pBarsOpt <- plot_ly(x = sub_dates, y = x_err - euc_prox, name = "Prediction error", type = "bar")

combPlotOpt <- subplot(pOpt, pBarsOpt, nrows = 2, shareX = TRUE)

pContour <- plot_ly(x = ks , y = ds, z = t(res$errors), type = "contour", autocontour = TRUE, contours = list(showlabels = TRUE, coloring = "heatmap"))
pContour <- layout(pContour, title = "MAE Optimization", xaxis = list(title = "K"), yaxis = list(title = "D") )
