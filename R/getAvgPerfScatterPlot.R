#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getAvgPerfScatterPlot = function(df.perf) {

	df.perf$variable = factor(df.perf$variable, levels = TECHNIQUES)
	g = ggplot(df.perf, aes(x = defaults, y = value, colour = variable, shape = variable))
	g = g + geom_point() + theme_bw()
	g = g + scale_color_manual(values=CUSTOM.COLORS[-1])
	g = g + geom_abline (slope=1, linetype = "dashed", color="black")
	g = g + labs(x = "Default performance", y = "Optimized performance", colour="Algorithm", shape="Algorithm")
  	g = g + theme(legend.title = element_blank())
  	return(g) 
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
