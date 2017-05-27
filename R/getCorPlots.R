#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getCorPlots = function(df.cor, algo.name) {

  df.aux = data.frame(cbind(1:nrow(df.cor), df.cor))
  colnames(df.aux)[1] = "X"

  first.id = as.numeric(which(grepl(".Perf", colnames(df.aux)))[1])

  #----------------------------
  # HPs correlation
  #----------------------------

  melted.df = melt(df.aux[,1:(first.id-1)], id.vars = 1)
  melted.df$variable = gsub(x = melted.df$variable, pattern = "\\.", replacement = " vs ")

  g1 = ggplot(melted.df, aes(x = X, y = variable, fill = value))
  g1 = g1 + geom_tile() + theme_classic()
  g1 = g1 + xlab("Dataset") + ylab(paste("Pair of", toupper(algo.name), "HPs"))  
  g1 = g1 + theme(axis.text.y = element_text(size = 7))
  g1 = g1 + theme(legend.title =  element_text(size = 10))
  g1 = g1 + scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
     midpoint = 0, limit = c(-1,1), space = "Lab", 
     name="Spearman\nCorrelation")
  g1 = g1 + scale_x_continuous(limits=c(1, nrow(df.cor)), 
    breaks=c(1, nrow(df.cor)/3, 2*(nrow(df.cor)/3), nrow(df.cor)))

  #----------------------------
  # HPs x Performance correlation
  #----------------------------
  
  melted.df2 = melt(df.aux[, c(1, first.id:ncol(df.aux))], id.vars = 1)
  melted.df2$variable = gsub(x = melted.df2$variable, pattern = "\\.Perf", replacement = "")

  g2 = ggplot(melted.df2, aes(x = X, y = variable, fill = value))
  g2 = g2 + geom_tile() + theme_classic()
  g2 = g2 + xlab("Dataset") + ylab(paste(toupper(algo.name), "HPs"))
  g2 = g2 + theme(axis.text.y = element_text(size = 7))
  g2 = g2 + theme(legend.title =  element_text(size = 10))

  g2 = g2 + scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
     midpoint = 0, limit = c(-1,1), space = "Lab", 
     name="Spearman\nCorrelation")
  g2 = g2 + scale_x_continuous(limits=c(1, nrow(df.cor)), 
    breaks=c(1, nrow(df.cor)/3, 2*(nrow(df.cor)/3), nrow(df.cor)))

  obj = list(g1 = g1, g2 = g2)
  return(obj)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
