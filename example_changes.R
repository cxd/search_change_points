source("lib/search_changepoints.R")

data("WWWusage")

dataSet <- data.frame(minute=1:length(WWWusage),
                      users=WWWusage)

# search changes for up to 10 minutes
lagCols <- c("t", sapply(1:10, function(t) paste0("tm",t)))
df2 <- search_changepoints_numeric(dataSet, 1:nrow(dataSet), 1, "minute", "users", cols=lagCols)
df2



data("airquality")
dataSet2 <- data.frame(
  Ozone=airquality$Ozone,
  Temp=airquality$Temp,
  SolarR=airquality$Solar.R,
  Day=airquality$Day,
  Month=airquality$Month,
  n=1:nrow(airquality))

dataSet2 <- dataSet2[complete.cases(dataSet2),]

lagCols <- c("t", sapply(1:7, function(t) paste0("tm",t)))
df3 <- search_changepoints_numeric(dataSet2, 1:nrow(dataSet2), 1, "n", "Ozone", cols=lagCols)
df3


par.old <- par(mfrow=c(2,1))

plot_interesting_changes(dataSet, "minute", "users", df2, "WWWusage", method="bartlett")

plot_interesting_changes(dataSet2, "n", "Ozone", df3, "AirQuality", method="bartlett")

par(par.old)




