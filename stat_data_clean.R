# data_clean for project thesis Autumn 2018
# date format %Y/%m/%d
#Remember to have this .R file in the same directory as the files you are loading 
#Spread data must span the same time horizon, and contain exactly the same amount of rows/data
#Input must contain columns containing the following data with exact references as follows: "Date", "Settlement", "ContractName"


library(moments)
library(quantmod)
library(Quandl)
library(PerformanceAnalytics)
library(stargazer)
library(tseries)
library(zoo)
library(readxl)
library(ggplot2)
library(glue)
library(colorspace)
library(reshape2)

setwd("\\\\sambaad.stud.ntnu.no/jorgesop/Documents/Prosjektoppgave H218/R prog") #specifiy working directory

#load the files here
# CHANGE THE TWO LINES BELOW WHEN CHANGING TIME SERIES
spread1 <- read_excel(paste("tyskQ.xlsx"))
spread2 <- read_excel("tyskY.xlsx")

#spread = spread2 - spread1 in our output data frame

# CHANGE THE TWO LINES BELOW WHEN CHANGING TIME SERIES
series1 <- c("NorY")
series2 <- c("GerY")

rows = nrow(spread1)

Series = spread1$Date
spread_out = data.frame(Series) #Established new data frame with same time span as input in the first column
spread_out$Settlement1 = spread1$Settlement
spread_out$Settlement2 = spread2$Settlement

x = 1;
y = 1;

while(is.na(spread1$ContractName[x])){ #finds the first contract specificier in the list for spread1
  x = x + 1
}

while(is.na(spread2$ContractName[y])){ #finds the first contract specificier in the list for spread2
  y = y + 1
}

roll1 = spread1$ContractName[x] #establishes the first contracts that are being analyzed
roll2 = spread2$ContractName[y]

for (i in 1:rows){ #looping through the whole data range
  
  if(is.na(spread1$Settlement[i] - spread2$Settlement[i])){ #checks if input data lacks settlement price, if so, the spread is not applicable
    spread_out$Spread[i] = NA
    next 
  }
  
  if(spread1$ContractName[i] != roll1 | spread2$ContractName[i] != roll2){ #checks if we are rolling on the current date, 1 = roll
    spread_out$Roll[i] = 1
  }
  else{
    spread_out$Roll[i] = 0
  }
  roll1 = spread1$ContractName[i] 
  roll2 = spread2$ContractName[i]
  
  #simple spread, gives values
  spread_out$Spread[i] = spread2$Settlement[i] - spread1$Settlement[i] #calculates spread
}


for (j in 2:rows){ #looping through the whole data range
  if(is.na(spread_out$Spread[j]-spread_out$Spread[j-1])){
    spread_out$pChange[j] = NA
    spread_out$dSpread[j] = NA
  }
  spread_out$pChange[j] = (spread_out$Spread[j]-spread_out$Spread[j-1])/spread_out$Spread[j-1]
  spread_out$RSpread[j] = spread2$Settlement[j]/spread2$Settlement[j-1] - spread1$Settlement[j]/spread1$Settlement[j-1]
  spread_out$dSpread[j] = (spread_out$Spread[j]-spread_out$Spread[j-1]) #diff in spread
}

spread_out_clean <- spread_out
spread_out_clean <- spread_out_clean[!(is.na(spread_out_clean$dSpread)),]
spread_out_clean <- spread_out_clean[!(is.na(spread_out_clean$RSpread)),]
spread_out_clean <- spread_out_clean[!(is.na(spread_out_clean$Spread)),]
spread_out_clean <- spread_out_clean[!(is.na(spread_out_clean$pChange)),]
spread_out_clean <- spread_out_clean[spread_out_clean$Roll  != 1,]

##################################################################################

library("PerformanceAnalytics")

DeltaSpread = spread_out_clean$dSpread

# Plotting the spread
dates <- as.Date(spread_out_clean[,1], format="%d.%m.%Y")
spread <- spread_out_clean[,7] #column 7 gives delta spread, ie spread diff from i-1 to i
plot(spread ~ dates, type="l", main=paste(series2, "-", series1),
     xlab="Dates", ylab="Spread Delta")

spreadseries1 = spread_out_clean[,2]
spreadseries2 = spread_out_clean[,3]

timeseries = data.frame(dates, spreadseries1, spreadseries2)

#plot the time series
p <- ggplot(data = NULL) + geom_line(data = timeseries, aes(x = timeseries$dates, y = timeseries$spreadseries1, colour = "NorY")) + 
                geom_line(data = timeseries, aes(x = timeseries$dates, y = timeseries$spreadseries2, colour = "GerY"))+
                labs(x = "Date", y = "EUR/MW", title = paste("Time series of ", series2, " and ", series1), colour = "Series")
print(p)

# Plotting histogram of the spread
#hist(spread, main=paste(series2, "-", series1), breaks=50, xlab="Spread (simple return)")


# Plotting histogram of the spread with Gaussian overlay
x<-seq(0,100,0.01) 
hist(spread, main=paste(series2, "-", series1), density=25, breaks=50,
     prob=TRUE, xlab="Spread Delta")
curve(dnorm(x, mean=mean(spread), sd=sd(spread)), col="darkblue",lwd=2, add=TRUE, yaxt="n") # adds gausian

# Descriptive statistics
#   Table of descriptive statistics of the spread
#   Table of test results: Normality (JB), Stationarity (DF, ADF) and 
#   Serial correlation (Q)  with up to 10 lags

df_table <- data.frame(matrix(0, 3, 12))
colnames(df_table) <- c("N", "Min", "Max", "Mean", "Median", "Std. dev", "Skewness", 
                        "Kurtosis", "JB", "DF", "ADF (10 lags)", "Ljung-Box (10 lags)")
rownames(df_table) <- c(paste(series2, "-", series1), "Critical Values", "Comment")

df_table[1,1] <- length(spread)
df_table[1,2] <- min(spread)
df_table[1,3] <- max(spread)
df_table[1,4] <- mean(spread)
df_table[1,5] <- median(spread)
df_table[1,6] <- sd(spread)
df_table[1,7] <- skewness(spread)
df_table[1,8] <- kurtosis(spread)
df_table[1,9] <- jarque.bera.test(spread)$statistic
df_table[1,10] <- adf.test(spread, alternative="stationary", k=0)$statistic
df_table[1,11] <- adf.test(spread, alternative="stationary", k=10)$statistic # Quite negative => stationary and H0 is rejected
df_table[1,12] <- Box.test(spread, type="Ljung-Box", lag=10)$statistic #Chi-squared distributed with m (= maximimum lag length) degrees of freedom
#phillips - perron test?


#critical values for test:
jb_crit = qchisq(0.95, df=2) #follows a chi squared dist with two degrees of freedom
adf_crit = -3.43 #from book
lb_crit = qchisq(0.95, df= 10) #(m=number of lags) degrees of freedom

df_table[2,9] = jb_crit
df_table[2,10] = adf_crit
df_table[2,11] = adf_crit
df_table[2,12] = lb_crit


for (i in 9:ncol(df_table)){
  
  if (df_table[2,i]<0 && df_table[1,i] < df_table[2,i] || df_table[2,i]>0 && df_table[1,i] > df_table[2,i]){
    df_table[3,i] = "Reject"
  }
  else{
    df_table[3,i] = "H0 True"
  }
}


# LaTeX output
stargazer(df_table, type = "text", title="Descriptive statistics", align = TRUE,
          digits=1, summary=FALSE, flip=TRUE)

conf_levels <- c(0.90, 0.95, 0.99)
position <- c("Long", "Short")
var_table <- data.frame(matrix(0, length(conf_levels),length(position)))
colnames(var_table) <- position
rownames(var_table) <- paste(conf_levels, "VaR")

# Long VaR
for (i in 1:dim(var_table)[1]) {# VaR conf. level
    var_table[i,1] <- quantile(spread, 1-conf_levels[i])
}

# Short VaR
for (i in 1:dim(var_table)[1]) {# VaR conf. level
  var_table[i,2] <- quantile(spread, conf_levels[i])
}

# LaTeX output
stargazer(var_table, type = "text", title=paste("Value-at-Risk"),
          digits=3, summary=FALSE)



#########################################
#Kjør riskmetrics, simple volatility forecast
#variance for entire series, lambda
#Find annualized volatility

n = 252 #number of periods in a year, daily scale - check sample

vol = StdDev(DeltaSpread, n)

df_vol = data.frame(dates, spread)

volplot = ggplot(data = df_vol, aes(x = dates, y = spread)) + 
                   geom_line(aes(colour= "first difference")) +
                    geom_abline(slope=0, intercept = vol, aes(colour = "+sigma"))+
                    geom_abline(slope=0, intercept = -vol, aes(colour = "-sigma"))+
                  labs(x = "Date", y = "First Difference", title = paste("First difference of ", series2, " and ", series1, " spread"), colour = "Series")
print(volplot)

#Riskmetrics

#rm_vol = ewma(df_vol, lambda = 0.96)



#Kjør volatilitetsmodelling - GARCH ++

