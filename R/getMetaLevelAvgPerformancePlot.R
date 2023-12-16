
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getMetaLevelAvgPerformancePlot = function (data, df.stats) {
  
  # -------------------------
  # main plot 
  # -------------------------
  
  g = ggplot(data = data, mapping = aes(x = algo, y = avgAuc, fill = Setup,
    group = Setup, colour = Setup, shape = Setup, linetype = Setup))
  g = g + geom_line() + geom_point()
  g = g + geom_hline(yintercept = 0.5, colour = "black", linetype = "dotted")
  g = g + geom_ribbon(aes(ymin=avgAuc-sdAuc, ymax=avgAuc+sdAuc), alpha = 0.15, colour=NA)
  g = g + facet_grid(.~task) + theme_bw()
  g = g + scale_shape_manual(values = CUSTOM.SHAPES)
  g = g + scale_colour_manual(values = LOCAL.COLOR)
  g = g + scale_fill_manual(values = LOCAL.COLOR)
  g = g + labs(color="Meta-features", fill="Meta-features", shape="Meta-features",
    linetype="Meta-features")
  g = g + labs(x = "Meta-learners", y = "Average AUC")
  g = g + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 7))

  # -------------------------
  # adding statistical results
  # -------------------------
   
 # Best for each scenario
  df.stats$Setup = NA
  # -------------------------
  # situations when using all the features is statistically better (good)
  # -------------------------
  
  ids.all = which(df.stats$Best == "all" & df.stats$stats == "TRUE")
  g = g + geom_point(data = df.stats[ids.all, ],
    aes(x = algo, y = 0.4),
    size = 1, colour = "darkgreen",
    fill = "darkgreen",shape = 24, Setup = NA)

  # -------------------------
  # situations when using all the features is not statistically worse (good)
  # -------------------------

  ids.other = which(df.stats$Best != "all" & df.stats$stats == "TRUE")
  g = g + geom_point(data = df.stats[ids.other,],
    aes(x = algo, y = 0.4), size = 1, colour = "red",
    fill = "red",shape = 25, Setup = NA)

  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
