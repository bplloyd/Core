library(ggseas)
library(xts)

pef = coreFund

cols = c("DistributionRate")

xts_m = na.omit(cull_Data(pef, "m")[, c("DistributionRate")])
xts_m = xts_m['2007/', ]

xts_m_clean  = as.xts(PerformanceAnalytics::clean.boudt(xts_m)[[1]])

# data_m = cull_ggplot_data(xts_m,
#                           cols = c("Calls_Total_Gross", "Distributions_Total", "CashFlows_Net", "FMV"),
#                           cumulativeCols = c("Calls_Total_Gross", "Distributions_Total", "CashFlows_Net"))


df_data = cull_ggplot_data(xts_m, cols = cols)
df_data_clean = cull_ggplot_data(xts_m_clean, cols = cols)

df_data = cull_ggplot_data(na.omit(PerformanceAnalytics::CalculateReturns(sp_m)))
#df_data = cull_ggplot_data(sp_m)


 # meth = "seas"
meth = "stl"
# meth = "decompose"


type = "additive"
# type = "multiplicative"


ggsdc(df_data_clean,
      aes(x = date, y = value),
      method = meth,
      type = type,
      start = c(2005, 3),
      frequency = PerformanceAnalytics::Frequency(xts_m)[1],
      s.window = "periodic"

      ) +

  geom_line() +

  scale_x_date(breaks = df_data$date[endpoints(df_data$date, on = "years")]) +

  theme(axis.text.x = element_text(angle = 30)) +

  ggtitle(label = paste0(df_data$variable[1], " - ", meth, " - ", type))

ggplot(data = df_data, mapping =  aes(x = date, y = value)) + geom_line()
  stat_stl(frequency = 12, s.window = "periodic")

ts_m = ts(data = as.vector(xts_m),
          start = c(lubridate::year(index(xts_m)[1]), lubridate::month(index(xts_m)[1])),
          end = c(lubridate::year(index(xts_m)[nrow(xts_m)]), lubridate::month(index(xts_m)[nrow(xts_m)])),
          frequency = 12)
ts_m_clean = ts(data = as.vector(xts_m_clean),
          start = c(lubridate::year(index(xts_m_clean)[1]), lubridate::month(index(xts_m_clean)[1])),
          end = c(lubridate::year(index(xts_m_clean)[nrow(xts_m_clean)]), lubridate::month(index(xts_m_clean)[nrow(xts_m_clean)])),
          frequency = 12)


decomp = stl(ts_m,
    # s.window = "periodic",
    # s.window = 12,
    s.window = 6,
    #s.window = "periodic",
    #t.window = 19,
    robust = T,
)

plot(decomp)

for(i in seq(from = 3, to = 21, by = 2))
  plot(stl(ts_m_clean,
           # s.window = "periodic",
            # s.window = 12,
           # s.window = 6,
            s.window = i,
           t.window = 19,
           robust = T,
           ), main = paste0("s.window = ", i ))

plot(stl(ts_m,
         s.window = "periodic",
         # s.window = 12,
         # s.window = 6,
         # s.window = i,
         t.window = 19,
         robust = T),
     main = "s.window = periodic")







# p = ggplot2::ggplot(data = data_m, ggplot2::aes(x = date, y = value, color = variable)) + ggplot2::geom_line(yintercept = 0)
# p = p + ggplot2::scale_x_date(breaks = data_m$date[xts::endpoints(data_m$date, on = "years")])
# p = p + ggplot2::theme(axis.text.x = element_text(angle = 30))
# p = p + theme(plot.subtitle = element_text(vjust = 1),
#     plot.caption = element_text(vjust = 1),
#     axis.line = element_line(size = 0.2,
#         linetype = "solid"), axis.ticks = element_line(colour = "gray15",
#         size = 1), panel.grid.major = element_line(colour = "gray95",
#         size = 0.05), panel.grid.minor = element_line(colour = "gray95",
#         size = 0.05), axis.text = element_text(family = "mono",
#         size = 15, colour = "gray28", hjust = 1,
#         vjust = 0.25), axis.text.x = element_text(size = 10),
#     axis.text.y = element_text(family = "mono",
#         size = 10), panel.background = element_rect(fill = "gray98",
#         colour = "antiquewhite4"))
# #p = p + scale_y_continuous(expand = c(-110, 0), limits = c(0, 1000))
# p = p + geom_hline(yintercept = 0)
# p
