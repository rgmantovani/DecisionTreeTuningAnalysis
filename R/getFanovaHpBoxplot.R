#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getFanovaHpBoxplot = function(df) {

  colnames(df) = gsub(x = colnames(df), 
    pattern = "x", replacement="vs")

  colnames(df) = gsub(x = colnames(df), pattern = "mavsdepth", 
    replacement = "maxdepth")

  ids = which(grepl("vs", colnames(df)))
  data.id = 1:nrow(df)
  temp = data.frame(cbind(data.id, df[,-ids]))
  
  # temp = temp[, sel.ids]
  rownames(temp) = NULL

  melted.df = melt(temp, id.vars = 1)
  melted.df$value = melted.df$value /100
 
  g = ggplot(melted.df, aes(x = variable, y = value))
  g = g + geom_boxplot() 
  g = g + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 9))
  g = g + labs(x = "Hyperparameter", y = "fAnova relative importance")
  g

  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

