#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getAvgPerfPlot = function(av.results, df.stats, put.annotations = TRUE) {

  df = av.results
  Best = colnames(df)[apply(df, 1, which.max)]
  df = cbind(1:nrow(df), df, Best, df.stats) 
  colnames(df)[1] = "data.id"
  colnames(df)[ncol(df)] = "Sign"

  mapping = read.csv("data/mapping.csv")
  mapping$UCI.name = as.character(mapping$UCI.name)

  aux.name = lapply(1:nrow(df), function(i) {
    id = which(mapping$UCI.name == rownames(df)[i])
    if(length(id) == 0) {
      return(c(NA,NA))
    }
    return(mapping[id,1:2])
  })

  maps = cbind(do.call("rbind", aux.name), df) #, df.stats)
  maps[,3] = NULL

  df = maps
  # order df as DF
  ids.order = order(df$defaults, decreasing = TRUE) 
  df = df[ids.order,]

  df[,1] = factor(df[,1], levels = df[,1])
  
  df.aux = df$Sign
  df.melted = melt(df, id.vars = c(1,2,10,11)) 

  g = ggplot(df.melted, aes(x = data.id, y = value, color = variable, group = variable,
    linetype = variable))
  g = g + geom_line() + ylab("(balanced) accuracy") + xlab("Dataset id") + theme_bw()
  g = g + scale_color_manual(values=CUSTOM.COLORS, labels = levels(df.melted$variable)) 
  g = g + scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1))
  g = g + scale_linetype_manual(values=CUSTOM.LINETYPES, labels = levels(df.melted$variable))
  g = g + theme(legend.position = c(.07,.49), legend.background = element_rect(colour = "black"))
  g = g + theme(legend.title = element_blank(), legend.text = element_text(size = 8))
  g = g + theme(legend.key.height = unit(0.3, "cm"), legend.key.width = unit(0.4, "cm"))
  g = g + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 6))

  if(put.annotations) {
    for(i in 1:nrow(df)) {
      g = g + annotate("text", x = i, y=0.05, label = df[i, "Best"], size = 1.8, angle = 90, 
        hjust = 0)
      if(df.aux[i] == "DF-Sign") {
        sp = data.frame(data.id = i, value = 0.015, variable = "defaults")
        g = g + geom_point(data = sp, aes(x = data.id, y = value, shape = "df"), 
          fill = "darkred", colour = "darkred")
      } else if (df.aux[i] == "Other-Sign") {
        sp = data.frame(data.id = i, value = 0.015, variable = "defaults")
        g = g + geom_point(data = sp, aes(x = data.id, y = value, shape = "tun"), 
          fill = "darkgreen", colour = "darkgreen")
      }
    }
  }
  g = g + scale_shape_manual(values = c("df"=25, "tun"=24)) 
  g = g + guides(shape=FALSE) +  guides(fill = FALSE)

  obj = list(g = g, ids.order = ids.order)
  return(obj)
}

#--------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------