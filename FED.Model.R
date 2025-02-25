# Yale Shiller Data:  http://www.econ.yale.edu/~shiller/data.html

library (readxl)
# url <- "http://www.econ.yale.edu/~shiller/data/ie_data.xls"
url <- "https://img1.wsimg.com/blobby/go/e5e77e0b-59d1-44d9-ab25-4763ac982e53/downloads/2fb6ea8f-2c13-4e80-b854-14e14caa8ee8/ie_data.xls"
temp <- tempfile()
download.file(url, temp, mode = "wb")
data <- read_excel(temp, sheet = 5, col_types = "numeric")
unlink(temp)

# clean data
data <- head(data[-c(1: 6), -c(14, 16)], -1)
names(data) <- c("Date", "S&P_Comp", "Dividend", "Earnings", "CPI", "Date_fract", "Long_Interest", "Real_Price", "Real_Div", "Real_TR_Price", "Real_Earn", "Real_TR_Earn", "Cycl_Adj_CAPE", "Cycl_Adj_TR_CAPE", "Excess_CAPE_yld", "Tot_Bond_ret_M", "Real_Tot_Bond_ret", "10Y_Annual_Stock_Real_ret", "10Y_Annual_Bond_Real_ret", "10Y_Annual_Excess_ret")

## Notes
# Dividend, Earnings are interpolated from quartely obervations and annualised
# Long_Interest is interpolated from yearly observations until 1953
# Real_Price = S&P_Comp / CPI * CPI(current), Real_Div = Dividend / CPI * CPI(current)
# Real_TR_Price is the dividend adjusted return
# Real_Earn = Earnings / CPI * CPI(current)
# Real_TR_Earn = Earnings * Real_TR_Price / S&P_Comp
# Cycl_Adj_CAPE = Real_Price / mean(Real_Earn, 10), Cycl_Adj_TR_CAPE = Real_TR_Price / mean(Real_TR_Earn, 10)
# Excess_CAPE_yld = 1 / Cycl_Adj_CAPE - Long_Interest / 100 + mean(dCPI, 10)
#
# Future Returns
# Tot_Bond_ret_M(t)
# Real_Tot_Bond_ret(t) = Tot_Bond_ret_M(t-1) * Real_Tot_Bond_ret (t-1) * CPI(t-1) / CPI(t)
# 10Y_Annual_Stock_Real_ret
# 10Y_Annual_Bond_Real_ret
# 10Y_Annual_Excess_ret = 10Y_Annual_Stock_Real_ret - 10Y_Annual_Bond_Real_ret

# Fight the Fed Model - Clifford Asness

# Exhibit 1 - S&P500 E/P vs 10Y Yields
library (TTR)
range <- c(which(data$Date_fract > 1965)[1]: which(data$Date_fract > 2002)[1])
ep <- 1 / data$Cycl_Adj_CAPE
plot(data$Date_fract[range], ep[range], main = "S&P 500 E/P vs 10Y Yields",
     xlab = "", ylab = "", ylim = c(0.02, 0.16), type = 'l', yaxt = "n")
axis(2, at = pretty(ep[range]), lab = paste0(pretty(ep[range]) * 100, "%"))
lines(data$Date_fract[range], data$Long_Interest[range] / 100, lty = 2)
cor(1 / data$Cycl_Adj_CAPE[range], data$Long_Interest[range] / 100, use = "pairwise.complete.obs") # +0.81 in the article
# Notes: try different trailing eg 5 years, or include total return
# plot(data$Date_fract[range], SMA(head(data$Earnings, -3), 12)[range] / data$`S&P_Comp`[range], main = "S&P500 E/P vs 10Y yields",
#      xlab = "", ylab = "", ylim = c(0.02, 0.16), type = 'l')

# Full period
range <- c(1: (which(is.na(data$Earnings))[1] - 1))
plot(data$Date_fract[range], 1 / data$Cycl_Adj_CAPE[range], main = "S&P500 E/P vs 10Y yields",
      xlab = "", ylab = "", ylim = c(0, 0.2), type = 'l')
lines(data$Date_fract[range], data$Long_Interest[range] / 100, lty = 2)
cor(SMA(data$Earnings[range], 12) / data$`S&P_Comp`[range], data$Long_Interest[range] / 100, use = "pairwise.complete.obs")
plot(data$Date_fract[range], runCor(SMA(data$Earnings[range], 60) / data$`S&P_Comp`[range], data$Long_Interest[range] / 100, 120), 
     xlab = "", ylab = "", type = 'l', col = 'red')
grid()

# Equation 5 - Nominal Earning Growth vc CPI Inflation (10Y rolling)
neg <- (tail(data$Earnings, -120) / head(data$Earnings, -120)) ^ 0.1 - 1
inf <- (tail(data$CPI, -120) / head(data$CPI, -120)) ^ 0.1 - 1
range <- c(which(data$Date_fract > 1926)[1]: (which(data$Date_fract >= 2002)[1] - 1)) - 120
lm <- lm(neg[range] ~ inf[range])  # 0.02 + 0.94 inf, R2 0.365 in the article
summary(lm)

# Full period
lm <- lm(neg ~ inf)
summary(lm)

# Exhibit 2 - S&P500 Real Return by Interest Rates 
data$'10Y_Annual_Stock_Real_ret_back' <- c(rep(NA, 120), (tail(data$Real_TR_Price, -120) / head(data$Real_TR_Price, -120)) ^ 0.1 - 1)
range <- c(which(data$Date_fract > 1965)[1]: (which(data$Date_fract > 2002)[1] - 1))
bins <- cut(data$Long_Interest[range], breaks = quantile(data$Long_Interest[range], probs = seq(0, 1, 0.2)))
TR.before <- aggregate(data$'10Y_Annual_Stock_Real_ret_back'[range], list(bins), mean)
TR.after <- aggregate(data$'10Y_Annual_Stock_Real_ret'[range], list(bins), mean)
df <- merge(TR.before, TR.after, by = "Group.1")
colnames(df) <- c("group", "before", "after")
library (reshape2)
df <- melt(df)
library (ggplot2)
ggplot(df, aes(x = group, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") 

# Full period
range <- c((max(which(is.na(data$'10Y_Annual_Stock_Real_ret_back'))) + 1): (min(which(is.na(data$'10Y_Annual_Stock_Real_ret'))) - 1))
bins <- cut(data$Long_Interest[range], breaks = quantile(data$Long_Interest[range], probs = seq(0, 1, 0.2)))
TR.before <- aggregate(data$'10Y_Annual_Stock_Real_ret_back'[range], list(bins), mean)
TR.after <- aggregate(data$'10Y_Annual_Stock_Real_ret'[range], list(bins), mean)
df <- merge(TR.before, TR.after, by = "Group.1")
colnames(df) <- c("group", "before", "after")
df <- melt(df)
ggplot(df, aes(x = group, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge")
# not applicable in different rate regimes (likely inflation related)

# Exhibit 3, 4, 5
EP <- 1 / data$Cycl_Adj_CAPE
Y <- data$Long_Interest / 100
RTR20 <- (tail(data$Real_TR_Price, -240) / head(data$Real_TR_Price, -240)) ^ 0.05 - 1
RTR10 <- (tail(data$Real_TR_Price, -120) / head(data$Real_TR_Price, -120)) ^ 0.1 - 1
RTR1 <- (tail(data$Real_TR_Price, -12) / head(data$Real_TR_Price, -12)) - 1

# a. 1881-2001
range <-  c(which(data$Date_fract > 1881)[1]: (which(data$Date_fract > 2002)[1] - 1))
# E/P
lm20.a1 <- lm(RTR20[range] ~ EP[range])
lm10.a1 <- lm(RTR10[range] ~ EP[range])
lm1.a1 <- lm(RTR1[range] ~ EP[range])
# (E/P - Y)
lm20.a2 <- lm(RTR20[range] ~ (EP - Y)[range])
lm10.a2 <- lm(RTR10[range] ~ (EP - Y)[range])
lm1.a2 <- lm(RTR1[range] ~ (EP - Y)[range])
# E/P + Y
lm20.a3 <- lm(RTR20[range] ~ EP[range] + Y[range])
lm10.a3 <- lm(RTR10[range] ~ EP[range] + Y[range])
lm1.a3 <- lm(RTR1[range] ~ EP[range] + Y[range])

# b. 1926-2001
range <-  c(which(data$Date_fract > 1926)[1]: (which(data$Date_fract > 2002)[1] - 1))
# E/P
lm20.b1 <- lm(RTR20[range] ~ EP[range])
lm10.b1 <- lm(RTR10[range] ~ EP[range])
lm1.b1 <- lm(RTR1[range] ~ EP[range])
# (E/P - Y)
lm20.b2 <- lm(RTR20[range] ~ (EP - Y)[range])
lm10.b2 <- lm(RTR10[range] ~ (EP - Y)[range])
lm1.b2 <- lm(RTR1[range] ~ (EP - Y)[range])
# E/P + Y
lm20.b3 <- lm(RTR20[range] ~ EP[range] + Y[range])
lm10.b3 <- lm(RTR10[range] ~ EP[range] + Y[range])
lm1.b3 <- lm(RTR1[range] ~ EP[range] + Y[range])

# c. 1955-2001
range <-  c(which(data$Date_fract > 1955)[1]: (which(data$Date_fract > 2002)[1] - 1))
# E/P
lm20.c1 <- lm(RTR20[range] ~ EP[range])
lm10.c1 <- lm(RTR10[range] ~ EP[range])
lm1.c1 <- lm(RTR1[range] ~ EP[range])
# (E/P - Y)
lm20.c2 <- lm(RTR20[range] ~ (EP - Y)[range])
lm10.c2 <- lm(RTR10[range] ~ (EP - Y)[range])
lm1.c2 <- lm(RTR1[range] ~ (EP - Y)[range])
# E/P + Y
lm20.c3 <- lm(RTR20[range] ~ EP[range] + Y[range])
lm10.c3 <- lm(RTR10[range] ~ EP[range] + Y[range])
lm1.c3 <- lm(RTR1[range] ~ EP[range] + Y[range])

# d. 1982-2001
range <-  c(which(data$Date_fract > 1982)[1]: (which(data$Date_fract > 2002)[1] - 1))
# E/P
lm20.d1 <- lm(RTR20[range] ~ EP[range])
lm10.d1 <- lm(RTR10[range] ~ EP[range])
lm1.d1 <- lm(RTR1[range] ~ EP[range])
# (E/P - Y)
lm20.d2 <- lm(RTR20[range] ~ (EP - Y)[range])
lm10.d2 <- lm(RTR10[range] ~ (EP - Y)[range])
lm1.d2 <- lm(RTR1[range] ~ (EP - Y)[range])
# E/P + Y
lm20.d3 <- lm(RTR20[range] ~ EP[range] + Y[range])
lm10.d3 <- lm(RTR10[range] ~ EP[range] + Y[range])
lm1.d3 <- lm(RTR1[range] ~ EP[range] + Y[range])

# e. 1955-2013
range <-  c(which(data$Date_fract > 1955)[1]: (which(data$Date_fract > 2014)[1] - 1))
# E/P
lm20.e1 <- lm(RTR20[range] ~ EP[range])
lm10.e1 <- lm(RTR10[range] ~ EP[range])
lm1.e1 <- lm(RTR1[range] ~ EP[range])
# (E/P - Y)
lm20.e2 <- lm(RTR20[range] ~ (EP - Y)[range])
lm10.e2 <- lm(RTR10[range] ~ (EP - Y)[range])
lm1.e2 <- lm(RTR1[range] ~ (EP - Y)[range])
# E/P + Y
lm20.e3 <- lm(RTR20[range] ~ EP[range] + Y[range])
lm10.e3 <- lm(RTR10[range] ~ EP[range] + Y[range])
lm1.e3 <- lm(RTR1[range] ~ EP[range] + Y[range])

mods <- do.call(paste0, expand.grid("lm", c(1, 10, 20), ".", c("a", "b", "c", "d", "e"), c(1: 3)))
res <- data.frame(matrix("", length(mods), 8))
rownames(res) <- mods
colnames(res) <- c("Period", "I", "I_p", "x1", "x1_p", "x2", "x2_p", "adjR2")

for (i in 1: length(mods)) {
  tmp <- summary(get(mods[i]))
  res[i, 2: 3] <- tmp$coefficients[1, c(1, 4)]
  if (dim(tmp$coefficients)[1] == 2) res[i, 4: 5] <- tmp$coefficients[2, c(1, 4)] else res[i, 4: 7] <- c(t(tmp$coefficients[2: 3, c(1, 4)]))
  res$adjR2[i] <- tmp$adj.r.squared
}

res5 <- data.frame(matrix("", 12, 10))
colnames(res5) <- c("Date", "Intercept", "I_p", "EP", "EP_p", "Y", "Y_p", "EP-Y", "EP-Y_p", "Adj.R2")
res5$Date <- c(rep('1881-2001', 3), rep('1926-2001', 3), rep('1955-2001', 3), rep('1982-2001', 3))
res4 <- res5[1: 6, ]
res3 <- res5[1: 9, ]

# Exhibit 3 - Forecasting 10 years S&P500 Real Returns
tmp <- c(1, 4, 7)
res3[tmp, c(2: 5, 10)] <- res[tmp + 1, c(2: 5, 8)]
res3[tmp + 1, c(2, 3, 8, 9, 10)] <- res[tmp + 16, c(2: 5, 8)]
res3[tmp + 2, c(2: 7, 10)] <- res[tmp + 31, -1]

# Exhibit 4 - Forecasting 20 years S&P500 Real Returns
tmp <- c(1, 4)
res4[tmp, c(2: 5, 10)] <- res[tmp + 2, c(2: 5, 8)]
res4[tmp + 1, c(2, 3, 8, 9, 10)] <- res[tmp + 17, c(2: 5, 8)]
res4[tmp + 2, c(2: 7, 10)] <- res[tmp + 32, -1]

# Exhibit 5 - Forecasting 1 year S&P500 Real Returns
tmp <- c(1, 4, 7, 10)
res5[tmp, c(2: 5, 10)] <- res[tmp, c(2: 5, 8)]
res5[tmp + 1, c(2, 3, 8, 9, 10)] <- res[tmp + 15, c(2: 5, 8)]
res5[tmp + 2, c(2: 7, 10)] <- res[tmp + 30, -1]

# Exhibit 6 S&P500 E/P vs 10Y Yields
range <- c(which(data$Date_fract > 1926)[1]: (which(data$Date_fract > 2002)[1] -1))
plot(data$Date_fract[range], ep[range], main = "S&P 500 E/P vs 10Y Yields",
     xlab = "", ylab = "", ylim = c(0.02, 0.16), type = 'l', yaxt = "n")
axis(2, at = pretty(ep[range]), lab = paste0(pretty(ep[range]) * 100, "%"))
lines(data$Date_fract[range], data$Long_Interest[range] / 100, lty = 2)
grid()
cor(1 / data$Cycl_Adj_CAPE[range], data$Long_Interest[range] / 100, use = "pairwise.complete.obs")

# 1926-2023
range <- c(which(data$Date_fract > 1926)[1]: which(data$Date_fract > 2024)[1])
plot(data$Date_fract[range], ep[range], main = "S&P 500 E/P vs 10Y Yields",
     xlab = "", ylab = "", ylim = c(0.02, 0.16), type = 'l', yaxt = "n")
axis(2, at = pretty(ep[range]), lab = paste0(pretty(ep[range]) * 100, "%"))
lines(data$Date_fract[range], data$Long_Interest[range] / 100, lty = 2)
grid()
cor(1 / data$Cycl_Adj_CAPE[range], data$Long_Interest[range] / 100, use = "pairwise.complete.obs")

# Equation 7
S.vol <- sqrt(diff(log(data$`S&P_Comp`)) ^ 2 * 12)
B.vol <- sqrt(diff(log((1 + data$Long_Interest / 100) ^ -10)) ^ 2 * 12)
S.vol <- c(NA, SMA(S.vol, 240))
B.vol <- c(NA, SMA(B.vol, 240))
range <-  c(which(data$Date_fract > 1926)[1]: (which(data$Date_fract > 2002)[1] - 1))
lm.7 <- lm(EP[range] ~ Y[range] + S.vol[range] + B.vol[range])
summary(lm.7) # EP = 0.03 + 0.96 Y + 0.37 S.vol - 0.78 B.vol, adj.R2 62% in the article

# Exhibit 7 S&P500 P/E and P/E fitted on Y and volatility
plot(data$Date_fract[range], data$Cycl_Adj_CAPE[range], main = "S&P500 P/E and P/E fitted on Y and volatility",
     xlab = "", ylab = "", ylim = c(0, 50), type = 'l')
lines(data$Date_fract[range], 1 / lm.7$fitted.values, lty = 2)

# Forecast 2002-2023
range <-  c(which(data$Date_fract > 2002)[1]: (which(data$Date_fract > 2024)[1] - 1))
new.data <- data.frame(Y[range], S.vol[range], B.vol[range])
ep.7 <- predict(lm.7, new.data)
ep.7.dates <- data$Date_fract[range]
range <-  c(which(data$Date_fract > 1926)[1]: (which(data$Date_fract > 2024)[1] - 1))
plot(data$Date_fract[range], data$Cycl_Adj_CAPE[range], main = "S&P500 P/E and P/E fitted on Y and volatility",
     xlab = "", ylab = "", ylim = c(0, 100), type = 'l')
range <-  c(which(data$Date_fract > 1926)[1]: (which(data$Date_fract > 2002)[1] - 1))
lines(data$Date_fract[range], 1 / lm.7$fitted.values, lty = 2)
lines(ep.7.dates, 1 / ep.7, lty = 2, col = 2)

# NOTE: Dynamic linear model could be more insightful

# 1926-2023
range <-  c(which(data$Date_fract > 1926)[1]: (which(data$Date_fract > 2024)[1] - 1))
lm.7a <- lm(EP[range] ~ Y[range] + S.vol[range] + B.vol[range])
summary(lm.7a)
plot(data$Date_fract[range], data$Cycl_Adj_CAPE[range], main = "S&P500 P/E and P/E fitted on Y and volatility",
     xlab = "", ylab = "", ylim = c(0, 50), type = 'l')
lines(data$Date_fract[range], 1 / lm.7a$fitted.values, lty = 2)
grid()

# 1965-2023
range <-  c(which(data$Date_fract > 1965)[1]: (which(data$Date_fract > 2024)[1] - 1))
lm.7b <- lm(EP[range] ~ Y[range] + S.vol[range] + B.vol[range])
summary(lm.7b)
plot(data$Date_fract[range], data$Cycl_Adj_CAPE[range], main = "S&P500 P/E and P/E fitted on Y and volatility",
     xlab = "", ylab = "", ylim = c(0, 50), type = 'l')
lines(data$Date_fract[range], 1 / lm.7b$fitted.values, lty = 2)

# Full period
lm.7f <- lm(EP ~ Y + S.vol + B.vol)
summary(lm.7f)
plot(tail(data$Date_fract, -240), tail(data$Cycl_Adj_CAPE, -240), main = "S&P500 P/E and P/E fitted on Y and volatility",
     xlab = "", ylab = "", ylim = c(0, 50), type = 'l')
lines(tail(data$Date_fract, -240), 1 / lm.7f$fitted.values, lty = 2)
