library(plotly)
library(shiny)

plotMode <- "lines"

p1 <- plot_ly(x = 1:NROW(subX), type = "scatter", y = subX, name = "Real", mode = plotMode)
p1 <- add_trace(p1, y = EucPro, name = "Proximity", mode = plotMode) 
p1 <- add_trace(p1, y = EucTre, name = "Trend", mode = plotMode) 
p1 <- add_trace(p1, y = EucSame, name = "Same", mode = plotMode)
p1 <- layout(p1, title = "Prediccion con distancia Euclídea", xaxis = list(rangeslider = list(type = "date")))

#p2 <- plot_ly(x = 1:(NROW(subX)-1), type = "scatter", y = subX[1:(NROW(subX)-1)], name = "Real", mode = plotMode)
#p2 <- add_trace(p2, y = EucPro[2:NROW(EucPro)], name = "Proximity", mode = plotMode) 
#p2 <- add_trace(p2, y = EucTre[2:NROW(EucTre)], name = "Trend", mode = plotMode) 
#p2 <- add_trace(p2, y = EucSame[2:NROW(EucSame)], name = "Same", mode = plotMode)
#p2 <- layout(p2, title = "Prediccion desplazada")

pB <- plot_ly(x = 1:NROW(subX), y = subX - EucPro, name = "Error", type = "bar")
pB <- layout(pB, yaxis = list( range = c( max(subX)*-1, max(subX) ) ))

combPlot <- subplot(p1, pB, nrows = 2, shareX = TRUE)

pO <- plot_ly(z = res$errors, type = "contour", autocontour = TRUE, contours = list(showlabels = TRUE, coloring = "heatmap") )
pO <- layout(pO, title = "MAE Optimization", xaxis = list(title = "D"), yaxis = list(title = "K") )