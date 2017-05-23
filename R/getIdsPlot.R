#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getIdsBoxPlot = function(fullIds, measure = "mean") {

  temp = aggregateIds(fullIds = fullIds, measure = "mean")
  df.melted = melt(temp, id.vars = ncol(temp))

  g = ggplot(df.melted, aes(x = variable, y = value))
  g = g + geom_boxplot(width=0.5, colour="black", fill="white", outlier.size=1.0)
  g = g + theme_classic()
  g = g + ylab("(avg) Number of evaluations") + xlab("Tuning technique")
  return(g)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getIdsHistoPlot = function(fullIds) {

  # temp = aggregateIds(fullIds = fullIds, measure = "mean")
  aux = lapply(1:length(fullIds), function(i) {
    fullIds[[i]]$data.id = i
    return(fullIds[[i]])
  })
  temp = do.call("rbind", aux)

  df.melted = melt(temp, id.vars = ncol(temp))

  g = ggplot(df.melted, aes(df.melted$value))
  g = g + geom_histogram(bins=10)
  g = g + theme_bw()
  g = g + facet_grid(variable~.)
  g = g + scale_x_continuous(breaks=c(1, 300, 600, 900))
  g = g + xlab("Number of evaluations")

  return(g) 

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
 
 # scatter per avg perf vs best ids

  # histograma de cada tecnica
  # g = g + scale_color_manual(values=CUSTOM.COLORS[-1], labels = TECHNIQUES[-1]) 
  # g = g + geom_point(size = 1.3)
    # g = g + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 5))
  # g = g + theme(legend.title = element_blank(), legend.text = element_text(size = 8))
  # g = g + theme(legend.key.height = unit(0.3, "cm"), legend.key.width = unit(0.4, "cm"))
  # g = g + scale_shape_manual(values = c(16, 15, 3), labels = c("defaults", "% of maj class", "maximum acc"))
  # g = g + theme(legend.position = c(.92,.25), legend.background = element_rect(colour = "black"))

# }

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------