#' An object of class \code{"forecast"} is a list usually containing at least the following elements: 
#' \item{model}{A list containing information about the fitted model} 
#      La k, la d, distancia,  media...
#' \item{method}{The name of the forecasting method as a character string} 
#' \item{mean}{Point forecasts as a time series}
#      La prediccion en si misma
#      
#' \item{lower}{Lower limits for prediction intervals} 
#'     Devolver tantos NAs como longitud de la prediccion 
#' \item{upper}{Upper limits for prediction intervals} 
#' 
#' \item{level}{The confidence values associated with the prediction intervals} 
#      Valor estadistico esta complicado, tal vez algo relacionado con el coeficiente que comentamos?
#            Variabilidad de los k-vecinos? 
#' \item{x}{The original time series (either \code{object} itself or the time series 
#'     used to create the model stored as \code{object}).} 
#' \item{residuals}{Residuals from the fitted model. For models with additive errors, 
#'     the residuals will be x minus the fitted values.} 
#  
#' \item{neigbors}
#' \item{init}
#' 
#' 
#' \item{fitted}{Fitted values (one-step forecasts)}