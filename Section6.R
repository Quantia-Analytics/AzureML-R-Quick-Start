# If running in ML Studio uncomment the first line with maml.mapInputPort().
cadairydata <- maml.mapInputPort(1)

## Create a new column as a POSIXct object
cadairydata$Time <- as.POSIXct(strptime(paste(as.character(cadairydata$Year), "-", as.character(cadairydata$Month.Number), "-01 00:00:00", sep = ""), "%Y-%m-%d %H:%M:%S"))

cadairytrain <- cadairydata[1:216, ]

Ylabs  <- list("Log CA Cotage Cheese Production, 1000s lb",
               "Log CA Ice Cream Production, 1000s lb",
               "Log CA Milk Production 1000s lb",
               "Log North CA Milk Milk Fat Price per 1000 lb")

Map(function(y, Ylabs){plot(cadairytrain$Time, y, xlab = "Time", ylab = Ylabs, type = "l")}, cadairytrain[, 4:7], Ylabs)

######################################
## This code is for model exploritory analysis and should
## not be included in the Azure Execute R Script module.

# milk.lm <- lm(Milk.Prod ~ Time + I(Month.Count^2) + I(Month.Count^3), data = cadairytrain)
# summary(milk.lm)

# milk.lm <- update(milk.lm, . ~ . - I(Month.Count^2))
# summary(milk.lm)

# milk.lm2 <- update(milk.lm, . ~ . + Month - 1)
# summary(milk.lm)

## End of exploritoray code. 
######################################

## Compute trend model. 
milk.lm <- lm(Milk.Prod ~ Time + I(Month.Count^3), data = cadairytrain)

## Compute the seasonal model. The Month variable adds the seasonal component
## and the -1 term removes the intercept so the model is not over determined.
milk.lm2 <- lm(Milk.Prod ~ Time + I(Month.Count^3) + Month - 1, data = cadairytrain)

## Compute predictions from our two models.
predict1  <- predict(milk.lm, cadairytrain)
predict2  <- predict(milk.lm2, cadairytrain)

## Plot the data and the predictions for both models
plot(cadairytrain$Time, cadairytrain$Milk.Prod, xlab = "Time", ylab = "Log CA Milk Production 1000s lb", type = "l")
lines(cadairytrain$Time, predict1, lty = 2, col = 2)

plot(cadairytrain$Time, cadairytrain$Milk.Prod, xlab = "Time", ylab = "Log CA Milk Production 1000s lb", type = "l")
lines(cadairytrain$Time, predict2, lty = 2, col = 2)

## Compute and plot the residuals for the seasonal model.
residuals <- cadairytrain$Milk.Prod - predict2
plot(cadairytrain$Time, residuals[1:216], xlab = "Time", ylab ="Residuals of Seasonal Model")


## Show the diagnostic plots for the model
plot(milk.lm2, ask = FALSE)

RMS.error <- function(series1, series2, is.log = TRUE, min.length = 2){
  ## Function to compute the RMS error or difference between two
  ## series or vectors. 
  
  messages <- c("ERROR: Input arguments to function RMS.error of wrong type encountered",
                "ERROR: Input vector to function RMS.error is too short",
                "ERROR: Input vectors to function RMS.error must be of same length",
                "WARNING: Funtion rms.error has received invald input time series.")
  
  ## Check the arguments. 
  if(!is.numeric(series1) | !is.numeric(series2) | !is.logical(is.log) | !is.numeric(min.length)) {
    warning(messages[1])
    return(NA)}
  
  if(length(series1) < min.length) {
    warning(messages[2])
    return(NA)}
  
  if((length(series1) != length(series2))) {
    warning(messages[3])
    return(NA)}

  ## If is.log is TRUE exponentiate the values, else just copy.
  if(is.log) {
    tryCatch( { 
      temp1 <- exp(series1)
      temp2 <- exp(series2) },
      error = function(e){warning(messages[4]); NA}
    )
  } else {
    temp1 <- series1
    temp2 <- series2
  }
  
  ## Compute the RMS error value. 
  tryCatch( {
    sqrt(sum((temp1 - temp2)^2) / length(temp1))}, 
    error = function(e){warning(messages[4]); NA})
}

## Compute predictions from our two models for the full data set.
predict1  <- predict(milk.lm, cadairydata)
predict2  <- predict(milk.lm2, cadairydata)

## Compute the RMS error in a dataframe. 
## Include the row names in the first column so they will
## appear in the output of the Execute R Script.  
RMS.df  <-  data.frame(
  rowNames = c("Trend Model", "Seasonal Model"),
  Traing = c(
    RMS.error(predict1[1:216], cadairydata$Milk.Prod[1:216]),
    RMS.error(predict2[1:216], cadairydata$Milk.Prod[1:216])),
  Forecast = c(
    RMS.error(predict1[217:228], cadairydata$Milk.Prod[217:228]),
    RMS.error(predict2[217:228], cadairydata$Milk.Prod[217:228]))
)

RMS.df

## The following line should be executed only when running in
## Azure ML Studio. 
maml.mapOutputPort('RMS.df') 