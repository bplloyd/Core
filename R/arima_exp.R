library(fpp)
data("h02")


lh02 <- log(h02)
par(mfrow=c(2,1))
plot(h02, ylab="H02 sales (million scripts)", xlab="Year")
plot(lh02, ylab="Log H02 sales", xlab="Year")

tsdisplay(diff(lh02,12),
          main="Seasonally differenced H02 scripts", xlab="Year")

tsdisplay(ts_m_clean, lag.max = 60,
          main="distribution rate", xlab="Year")

tsdisplay(diff(diff(ts_m_clean,12)),
          main="Seasonally differenced distribution rate", xlab="Year")

auto.arima(diff(diff(ts_m_clean,12)))

aa_m_clean_aicc = auto.arima(window(ts_m_clean, start = c(2007,1), end = c(2015,12)),
                        max.p = 5,
                        max.q = 5,
                        max.Q = 5,
                        max.P = 5,
                        max.d = 3,
                        max.D = 3,
                        trace = T,
                        ic = "aicc",
                        stepwise = F)


aa_m_clean_aic = auto.arima(ts_m_clean,
                             max.p = 4,
                             max.q = 4,
                             max.Q = 4,
                             max.P = 4,
                             max.d = 3,
                             max.D = 3,
                             trace = T,
                             ic = "aic")

aa_m_clean_bic = auto.arima(ts_m_clean,
                            max.p = 4,
                            max.q = 4,
                            max.Q = 4,
                            max.P = 4,
                            max.d = 3,
                            max.D = 3,
                            trace = T,
                            ic = "bic")

aa_m = auto.arima(ts_m)
