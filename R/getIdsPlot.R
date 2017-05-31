#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getIdsBoxPlot = function(fullIds, measure = "mean") {

  temp = aggregateIds(fullIds = fullIds, measure = "mean")
  temp$X = 1:nrow(temp)

  df.melted = melt(temp, id.vars = ncol(temp))

  g = ggplot(df.melted, aes(x = variable, y = value))
  g = g + geom_boxplot(width=0.5, colour="black", fill="white", outlier.size=1.0)
  g = g + theme_classic()
  g = g + scale_y_continuous(breaks=c(0, 100, 200, 300, 400, 500))
  g = g + ylab("(avg) Number of evaluations") + xlab("Tuning technique")
  return(g)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getIdsHistoPlot = function(fullIds) {

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

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------