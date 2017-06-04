#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getFanovaPlot = function(df, algo.name, threshold = 0.001) {

  colnames(df) = gsub(x = colnames(df), pattern = "x", replacement="vs")

  sel.ids = which((colMeans(df)/100) > threshold)

  ids = which(grepl("vs", colnames(df)))
  data.id = 1:nrow(df)
  temp = data.frame(cbind(data.id, df))
  
  temp = temp[, sel.ids]
  rownames(temp) = NULL

  melted.df = melt(temp, id.vars = 1)
  melted.df$value = melted.df$value /100
 
  g = ggplot(melted.df, aes(x = data.id, y = variable, fill = value))
  g = g + geom_tile() + theme_classic()
  g = g + xlab("Dataset") + ylab(paste(algo.name, "HPs or pair of HPs"))
  g = g + theme(axis.text.y = element_text(size = 7))
  g = g + theme(legend.title =  element_text(size = 10))

  g = g + scale_fill_gradient2(low = "white", high = "darkred", mid = "red",
     midpoint = 0.5, space = "Lab", limit = c(0, 1),#
     name="functional \nAnova marginals\n")

  my.breaks = round(c(1, nrow(df)/3, 2*(nrow(df)/3), nrow(df)))
  g = g + scale_x_continuous(limits=c(1, nrow(df)), breaks=my.breaks)

  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

