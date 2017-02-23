ggCashFlows = function(pef, freq = "m", scaledBy=NULL, title = NULL){
  df = cull_ggplot_data(pef@PeriodData[,c("Commitment", "Calls_Total_Net", "Calls_Total_Gross", "Distributions_Total", "CashFlows_Net", "FMV")]
                        #, melt_cols = c("Commitment", "Calls_Total_Net", "Calls_Total_Gross", "Distributions_Total", "FMV"),
                        ,cumulativeCols = c("Commitment", "Calls_Total_Net", "Calls_Total_Gross", "Distributions_Total", "CashFlows_Net")
                        , scaledBy = scaledBy)


  df[df$cashflow %in% c("Commitment", "Calls_Total_Gross", "Calls_Total_Net"), "value"] = -df[df$cashflow %in% c("Commitment", "Calls_Total_Gross", "Calls_Total_Net"),"value"]

   p =  ggplot2::ggplot(data = df, ggplot2::aes(x=date, y=value, color = cashflow)) + ggplot2::geom_line(size = 1) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0)) +
      #ggplot2::theme_minimal() +
      ggplot2::theme_bw() +
      #ggplot2::theme_light() +
      ggplot2::facet_wrap( ~ vintage) +
      ggplot2::ggtitle(title)
      #theme_bw()
     # theme(plot.subtitle = element_text(vjust = 1),
      #     plot.caption = element_text(vjust = 1),
      #     panel.grid.major = element_line(colour = "gray89",
      #                                     size = 0),
      #     panel.grid.minor = element_line(colour = "gray89",
      #                                     size = 0),
      #     axis.text = element_text(family = "mono",
      #                               size = 11),
      #     legend.text = element_text(size = 9),
      #     panel.background = element_rect(fill = "gray96"),
      #     legend.position = "bottom", legend.direction = "horizontal")

  p

}
