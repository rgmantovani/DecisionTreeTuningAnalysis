#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getCharacteristInfo = function(df.info, measure, variable) {

  id.meas  = which(grepl(paste(measure, variable, sep = "."), colnames(df.info)))
  id.sd    = which(grepl(paste("sd", variable, sep = "."), colnames(df.info)))
  id.min   = which(grepl(paste("min", variable, sep = "."), colnames(df.info)))
  id.max   = which(grepl(paste("max", variable, sep = "."), colnames(df.info)))
  
  df = cbind(
    df.info[ , c("data.id", "dataset", "technique")], 
    df.info[ , c(id.meas, id.sd, id.min, id.max)]
  )

  df$variable = variable
  colnames(df)[4:7] = c("value", "sd", "min", "max")
  return(df)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

InnerMeltData = function(df.info, measure = "median") {

  df2 = getCharacteristInfo(df.info = df.info, measure = measure, variable = "tree.size")
  df.final = df2

  df.final$data.id = as.factor(as.character(df.final$data.id))
  df.final$data.id = factor(df.final$data.id, levels=1:94)
  df.final[which(df.final$technique == "mbo"), "technique"] = "smbo"
  return(df.final)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getTreeSizePlot = function(df.info, ids.order, measure = "median") {

  df.final = InnerMeltData(df.info = df.info, measure = measure)

  # reordering according to the defaults performance
  df.final[,1] = as.numeric(df.final[,1])
  aux = lapply(ids.order, function(id) {
    ids = which(df.final[, "data.id"] == id)
    return(df.final[ids, ])
  })

  df.ordered = do.call("rbind", aux)
  df.ordered[which(df.ordered$dataset == "meta-"), "dataset"] = "meta-data"
  df.ordered$dataset = gsub(df.ordered$dataset, pattern = "analcat_", replacement = "analcatdata_")

  mapping = read.csv("data/mapping.csv")
  mapping$UCI.name = as.character(mapping$UCI.name)

  aux.name = lapply(1:nrow(df.ordered), function(i) {

    id = which(mapping$UCI.name == df.ordered[i, "dataset"])
    if(length(id) == 0) {
      return(c(NA, NA))
    }
    return(mapping[id, 1:2])
  })

  tmp = cbind(do.call("rbind", aux.name), df.ordered) #, df.stats)
  tmp[,3:4] = NULL

  df.ordered = tmp
  df.ordered$data.id = factor(as.character(df.ordered$data.id), levels = unique(df.ordered$data.id))


  LOCAL.LINETYPES = CUSTOM.LINETYPES
  LOCAL.LINETYPES[1] = "dotdash"

  g = ggplot(df.ordered, aes(x = data.id, y = value, color = technique, group = technique,
    linetype = technique))
  g = g + geom_line() + ylab("Tree size (log2)") + xlab("Dataset id") + theme_bw()
  g = g + geom_ribbon(aes(ymin=value-sd, ymax=value+sd, fill = technique), alpha = 0.2, colour=NA) 

  g = g + scale_color_manual(values = CUSTOM.COLORS, labels = TECHNIQUES) 
  g = g + scale_linetype_manual(values = LOCAL.LINETYPES, labels = TECHNIQUES)
  g = g + scale_fill_manual(values = CUSTOM.COLORS, labels = TECHNIQUES)
  g = g + scale_y_continuous(trans='log2')
  
  g = g + theme(legend.background = element_rect(colour = "black"))
  g = g + theme(legend.title = element_blank(), legend.text = element_text(size = 8))
  g = g + theme(legend.key.height = unit(0.3, "cm"), legend.key.width = unit(0.4, "cm"))
  g = g + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 6))

  # remove legend
  g = g + theme(legend.position="none")

  return(g)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
