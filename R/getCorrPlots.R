#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getCorrPlots = function(algo, df.cor) {

  # TODO :Generalize

  # df.aux = cbind(1:nrow(df.cor), df.cor)
  # colnames(df.aux)[1] = "X"
  # # df.aux[,1] = as.factor(df.aux[,1])

  # melted.df = melt(df.aux[,1:36], id.vars = 1)
  # melted.df$variable = gsub(x = melted.df$variable, pattern = "\\.", replacement = " vs ")

  # g = ggplot(melted.df, aes(x = X, y = variable, fill = value))
  # g = g + geom_tile() #colour = 'black')
  # g = g + theme_classic()
  # g = g + xlab("Dataset") + ylab("Pair of J48 HPs")
  # g = g + theme(axis.text.y = element_text(size = 7))
  # g = g + theme(legend.title =  element_text(size = 10))
  # g = g + scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
  #    midpoint = 0, limit = c(-1,1), space = "Lab", 
  #    name="Spearman\nCorrelation")
  # g = g + scale_x_continuous(limits=c(1, 94), breaks=c(1,23,47,70,94))

  # ggsave(g, file = "output/corHeatMap.pdf", dpi = 500, width = 7, height = 5, units = "in")

  # melted.df2 = melt(df.aux[, c(1, 38:46)], id.vars = 1)
  # melted.df2$variable = gsub(x = melted.df2$variable, pattern = "\\.Perf", replacement = "")

  # g2 = ggplot(melted.df2, aes(x = X, y = variable, fill = value))
  # g2 = g2 + geom_tile() 
  # g2 = g2 + theme_classic()
  # g2 = g2 + xlab("Dataset") + ylab("J48 HP")
  # g2 = g2 + theme(axis.text.y = element_text(size = 7))
  # g2 = g2 + theme(legend.title =  element_text(size = 10))

  # g2 = g2 + scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
  #    midpoint = 0, limit = c(-1,1), space = "Lab", 
  #    name="Spearman\nCorrelation")
  # g2 = g2 + scale_x_continuous(limits=c(0, 94), breaks=c(0,23,47,70,94))


  # ggsave(g2, file = "output/corAccHeatMap.pdf", dpi = 500, width = 7, height = 2, units = "in")

}


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
