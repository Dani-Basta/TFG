library(plotly)
library(shiny)


pOpt <- plot_ly( y = subX, type = "scatter",  name = "Real", mode = "lines")
pOpt <- add_trace(pOpt, y = naive, name = "Naive") 
pOpt <- add_trace(pOpt, y = EucPro, name = "Proximity") 
# pOpt <- add_trace(pOpt, y = ses, name = "SES", mode = plotMode) 
pOpt <- layout(pOpt, title = "Prediccion con distancia Euclídea", xaxis = list(rangeslider = list(type = "date")))

#p2 <- plot_ly(x = 1:(NROW(subX)-1), type = "scatter", y = subX[1:(NROW(subX)-1)], name = "Real", mode = "lines")
#p2 <- add_trace(p2, y = EucPro[2:NROW(EucPro)], name = "Proximity") 
#p2 <- add_trace(p2, y = EucTre[2:NROW(EucTre)], name = "Trend") 
#p2 <- add_trace(p2, y = EucSame[2:NROW(EucSame)], name = "Same"
#p2 <- layout(p2, title = "Prediccion desplazada")

pBars <- plot_ly( y = subX - EucPro, name = "Prediction Error", type = "bar")
pBars <- add_trace(pBars, y = subX - naive, name = "Naive Error") 
pBars <- layout(pBars, yaxis = list( range = c( max(subX)*-1, max(subX) ) ))

combPlot <- subplot(pOpt, pBars, nrows = 2, shareX = TRUE)

pOpt <- plot_ly(x = ks , y = ds, z = t(res$errors), type = "contour", autocontour = TRUE, contours = list(showlabels = TRUE, coloring = "heatmap") )
pOpt <- layout(pOpt, title = "MAE Optimization", xaxis = list(title = "K"), yaxis = list(title = "D") )
