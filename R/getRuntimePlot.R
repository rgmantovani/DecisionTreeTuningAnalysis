#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getRuntimePlot = function(df.time, ids.order) {

  df.temp = df.time
  df.temp$time = factor(df.temp$time, levels=c("tuning", "training", "testing"))
  
  df.temp[which(df.temp$technique == "mbo"), "technique"]    = "SMBO"
  df.temp[which(df.temp$technique == "eda"), "technique"]    = "EDA"
  df.temp[which(df.temp$technique == "ga"), "technique"]     = "GA"
  df.temp[which(df.temp$technique == "random"), "technique"] = "RS"
  df.temp[which(df.temp$technique == "pso"), "technique"]    = "PSO"
  df.temp[which(df.temp$technique == "irace"), "technique"]  = "Irace"

  for(i in 1:4) {
    df.temp[,i] = as.numeric(df.temp[,i])
  }

  df.temp$technique = as.factor(x=df.temp$technique)
  df.temp$data.id = as.numeric(as.factor(df.temp$dataset))

  # ids ordered
  aux = lapply(ids.order, function(id) {
    ids = which(df.temp[, "data.id"] == id)
    return(df.temp[ids, ])
  })

  df.ordered = do.call("rbind", aux)
  df.ordered$data.id = factor(as.character(df.ordered$data.id), levels = unique(df.ordered$data.id))
  df.ordered$technique = factor(df.ordered$technique, 
    levels = levels(df.ordered$technique)[c(4,3,7,2,5,6,1)])
 
  g = ggplot(df.ordered, aes(x = data.id, y = median, color = technique, group = technique,
    linetype = technique))
  g = g + geom_line()
  g = g + facet_grid(time ~ ., scales = "free")
  g = g + theme_bw()
  g = g + scale_y_continuous(trans="log10") 
  g = g + ylab("log10(time) in sec") 
  g = g + xlab("dataset id")
  g = g + theme(legend.background = element_rect(colour = "black"))
  g = g + theme(legend.title = element_blank(), legend.text = element_text(size = 8))
  g = g + theme(legend.key.height = unit(0.3, "cm"), legend.key.width = unit(0.4, "cm"))
  g = g + theme(axis.text=element_text(size=6.5))
  g = g + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 5))
  g = g + scale_color_manual(values=CUSTOM.COLORS, labels = TECHNIQUES)
  g = g + scale_linetype_manual(values=CUSTOM.LINETYPES, labels = TECHNIQUES) 
 
  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
