## Valuation Models

# Buffet Indicator
# https://www.currentmarketvaluation.com/models/buffett-indicator.php
# Named after Warren Buffett, who called the ratio "the best single measure of where valuations stand at any given moment".
# Criticism:  1. does not consider how stocks are valued relative to interest rates
#             2. the stock market valuation reflects international activity while GDP does not

library (fredr)
fredr_set_key("af5ba0626bb9b91c5dc2d33ff2f8a6ab")

mcap.fed <- fredr(
  series_id = "NCBEILQ027S", # Nonfinancial Corporate Business; Corporate Equities; Liability, Level ($M)
  observation_start = as.Date("1945-01-01"),
  observation_end = as.Date(Sys.Date())
)
# mcap.wilshire <- bdh("FTW5000 Index", "PX_LAST", as.Date("1945-01-01"))

gdp.fed <- fredr(
  series_id = "GDP", # Gross Domestic Product SA ($B)
  observation_start = as.Date("1945-01-01"),
  observation_end = as.Date(Sys.Date())
)

df <- merge(mcap.fed, gdp.fed, by = "date", all = TRUE)
df <- na.omit(df[, c(1, 3, 7)])
df$ratio <- log(df[, 2] / 1000 / df[, 3])

lm.buff <- lm(df$ratio ~ df$date)
sd2.buff <- sd(abs(lm.buff$residuals)) * 3
plot(df$date, df$ratio)
abline(lm.buff, col = 4)
abline(lm.buff$coefficients[1] + sd2.buff, lm.buff$coefficients[2], col = 2)
abline(lm.buff$coefficients[1] - sd2.buff, lm.buff$coefficients[2], col = 2)

# PE Ratio
# https://www.currentmarketvaluation.com/models/price-earnings.php
# Criticism:  1. does not consider how stocks are valued relative to interest rates
#             2. the ten-year earnings average component in the CAPE ratio produces a lag

lm.pe.ratio <- lm(data$Cycl_Adj_TR_CAPE ~ data$Date_fract)
sd3.pe.ratio <- sd(abs(lm.pe.ratio$residuals)) * 3
plot(data$Date_fract, data$Cycl_Adj_TR_CAPE)
abline(lm.pe.ratio, col = 4)
abline(lm.pe.ratio$coefficients[1] + sd3.pe.ratio, lm.pe.ratio$coefficients[2], col = 2)
abline(lm.pe.ratio$coefficients[1] - sd3.pe.ratio, lm.pe.ratio$coefficients[2], col = 2)

# Mean reversion

lm.mean.rev <- lm(log(data$Real_TR_Price) ~ data$Date_fract)
sd3.mean.rev <- sd(abs(lm.mean.rev$residuals)) * 3
plot(data$Date_fract, log(data$Real_TR_Price))
abline(lm.mean.rev, col = 4)
abline(lm.mean.rev$coefficients[1] + sd3.mean.rev, lm.mean.rev$coefficients[2], col = 2)
abline(lm.mean.rev$coefficients[1] - sd3.mean.rev, lm.mean.rev$coefficients[2], col = 2)

plot(data$Date_fract, lm.mean.rev$residuals)
abline(h = 0, col = 4)
abline(h = c(1, -1) * sd(lm.mean.rev$residuals) * 1.5, col = 2)

# Relative strength (Equity vs Bonds)

ltint.dev <- (data$Long_Interest - mean(data$Long_Interest)) / sd(data$Long_Interest)
plot(data$Date_fract, ltint.dev, ylim = c(-5, 5))
points(data$Date_fract, lm.mean.rev$residuals / sd(lm.mean.rev$residuals), col = 2)
points(data$Date_fract, ltint.dev - lm.mean.rev$residuals / sd(lm.mean.rev$residuals), col = 3)
points(data$Date_fract, ltint.dev + lm.mean.rev$residuals / sd(lm.mean.rev$residuals), col = 4)

test <- spectrum(ltint.dev + lm.mean.rev$residuals / sd(lm.mean.rev$residuals))
test <- spectrum(ltint.dev - lm.mean.rev$residuals / sd(lm.mean.rev$residuals))
test <- spectrum(c(rep(0,200), ltint.dev, rep(0,200)))

