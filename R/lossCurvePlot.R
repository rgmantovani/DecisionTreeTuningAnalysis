#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# # algo = "J48"
# # algo = "rpart"
# algo = "classif.ctree"

# measure = "loss"
# # # measure = "rank"

# algo.name = gsub(x = algo, pattern="classif.", replacement = "")
# algo.path = paste("data", algo, "results", sep="/")
# all.dirs  = list.files(path = algo.path, full.names = TRUE)

# algo.path.list = getOptPath(algo = algo, all.dirs = all.dirs)

# algo.path.list = lapply(algo.path.list, function(elem) {

#   if(any(is.na(elem))) {
#     return(NULL)
#   } else {
#     return(elem)
#   }
# })

# algo.path.list = Filter(Negate(is.null), algo.path.list)

# # algo.paths = lapply(algo.path.list, avgRankPath)
# algo.paths = lapply(algo.path.list, avgLossPath)
# df.algo.paths = Reduce("+", algo.paths)/length(algo.paths)
# df = df.algo.paths

lossCurvePlot = function(df, measure) {

  checkmate::assertChoice(x=measure, choices=c("rank", "loss")) 

  colnames(df) = c("id", "Defaults", "PSO", "GA", "EDA", "RS", "SMBO", "Irace")
  new.df = melt(df, id.vars = 1)

  g = ggplot(new.df, mapping = aes(x = id, y = value, colour = variable, linetype = variable))
  g = g + geom_line() + theme_bw()
  g = g + scale_x_continuous(limits = c(1,900), breaks =c(1,150,300,450,600,750,900))
 
  if(measure == "rank") {
    g = g + scale_y_continuous(limits = c(1,7), breaks = c(1,2,3,4,5,6,7))
    g = g + labs(x = "Number of evaluations", y = "Average Rank\n(Balanced Accuracy)")
  } else {
    g = g + labs(x = "Number of evaluations", y = "Average Loss\n(Balanced Accuracy)")
  }

  g = g + scale_colour_manual(values = CUSTOM.COLORS)
  g = g + scale_linetype_manual(values = CUSTOM.LINETYPES)
  g = g + theme(legend.background = element_rect(colour = "black"))
  g = g + theme(legend.title = element_blank(), legend.text = element_text(size = 8))
  g = g + theme(legend.key.height = unit(0.3, "cm"), legend.key.width = unit(0.4, "cm"))
  g = g + labs(colour = "Technique", linetype = "Technique")

  # ggsave(g, file = paste0("output/LossCurve_avgLoss_", algo, ".pdf"), dpi = 500,
  #   width = 6.09, height = 2.17)

  # ggsave(g, file = paste0("output/LossCurve_avgLoss_", algo, "_Rank.pdf"), dpi = 500,
  #   width = 6.09, height = 2.17)

  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
