print("This file was created within RStudio")
print("And now it lives on GitHub")

# Load the CO2 dataset in R
data(co2) 

# Take first differences to remove the trend 
co2_1stdiff=diff(co2,differences=1)

# Filter via moving averages to remove the seasonality 
co2_ma=filter(co2,filter=c(1/24,rep(1/12,11),1/24),sides=2)

par(mfrow=c(3,1), cex.lab=1.2,cex.main=1.2)
plot(co2) # plot the original data 
plot(co2_1stdiff) # plot the first differences (removes trend, highlights seasonality)
plot(co2_ma) # plot the filtered series via moving averages (removes the seasonality, highlights the trend)

#
# Simulate data with no temporal structure (white noise)
#
set.seed(2021)
T=200
t =1:T
y_white_noise=rnorm(T, mean=0, sd=1)
#
# Define a time series object in R: 
# Assume the data correspond to annual observations starting in January 1960 
#
yt=ts(y_white_noise, start=c(1960), frequency=1)
#
# plot the simulated time series, their sample ACF and their sample PACF
#
par(mfrow = c(1, 3), cex.lab = 1.3, cex.main = 1.3)
yt=ts(y_white_noise, start=c(1960), frequency=1)
plot(yt, type = 'l', col='red', xlab = 'time (t)', ylab = "Y(t)")
acf(yt, lag.max = 20, xlab = "lag",
    ylab = "Sample ACF",ylim=c(-1,1),main="")
pacf(yt, lag.max = 20,xlab = "lag",
     ylab = "Sample PACF",ylim=c(-1,1),main="")


###############################
#####    sample ar(1)     #####
###############################
#
# sample data from 2 ar(1) processes and plot their ACF and PACF functions
#
set.seed(2021)
T=500 # number of time points
#
# sample data from an ar(1) with ar coefficient phi = 0.9 and variance 1
#
v=1.0 # innovation variance
sd=sqrt(v) #innovation stantard deviation
phi1=0.9 # ar coefficient
yt1=arima.sim(n = T, model = list(ar = phi1), sd = sd)
#
# sample data from an ar(1) with ar coefficient phi = -0.9 and variance 1
#
phi2=-0.9 # ar coefficient
yt2=arima.sim(n = T, model = list(ar = phi2), sd = sd)

par(mfrow = c(2, 1), cex.lab = 1.3)
plot(yt1,main=expression(phi==0.9))
plot(yt2,main=expression(phi==-0.9))

par(mfrow = c(3, 2), cex.lab = 1.3)
lag.max=50 # max lag

#
## plot true ACFs for both processes
#
cov_0=sd^2/(1-phi1^2) # compute auto-covariance at h=0
cov_h=phi1^(0:lag.max)*cov_0 # compute auto-covariance at h
plot(0:lag.max, cov_h/cov_0, pch = 1, type = 'h', col = 'red',
     ylab = "true ACF", xlab = "Lag",ylim=c(-1,1), main=expression(phi==0.9))

cov_0=sd^2/(1-phi2^2) # compute auto-covariance at h=0
cov_h=phi2^(0:lag.max)*cov_0 # compute auto-covariance at h
# Plot autocorrelation function (ACF)
plot(0:lag.max, cov_h/cov_0, pch = 1, type = 'h', col = 'red',
     ylab = "true ACF", xlab = "Lag",ylim=c(-1,1),main=expression(phi==-0.9))

## plot sample ACFs for both processes
#
acf(yt1, lag.max = lag.max, type = "correlation", ylab = "sample ACF",
    lty = 1, ylim = c(-1, 1), main = " ")
acf(yt2, lag.max = lag.max, type = "correlation", ylab = "sample ACF",
    lty = 1, ylim = c(-1, 1), main = " ")
## plot sample PACFs for both processes
#
pacf(yt1, lag.ma = lag.max, ylab = "sample PACF", ylim=c(-1,1),main="")
pacf(yt2, lag.ma = lag.max, ylab = "sample PACF", ylim=c(-1,1),main="")
