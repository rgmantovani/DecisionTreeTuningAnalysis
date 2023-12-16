#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getMetaLevelStatisticalDifferences = function(meta.data = meta.data, 
  agg.meta.data = agg.meta.data) {

  algos = unique(agg.meta.data$algo)
  tasks = unique(agg.meta.data$task)

  combinations = expand.grid(algos, tasks)
  aux = lapply(1:nrow(combinations), function(k) {

      # print(combinations[k,])
      sub = dplyr::filter(agg.meta.data, task == combinations[k,2] & algo == combinations[k,1])
      sub = sub[order(sub$avgAuc, decreasing = TRUE),]
     
      # performances of the best setup
      Best   = sub$Setup[1]
      sub.tech1 = dplyr::filter(meta.data, algo == combinations[k,1])
      sub.tech1 = sub.tech1[grepl(x = sub.tech1$task, pattern = combinations[k,2]),]
      sub.tech1 = sub.tech1[grepl(x = sub.tech1$task, pattern = Best),]
    
      # performances of the second best setup
      Second = sub$Setup[2]
      sub.tech2 = dplyr::filter(meta.data, algo == combinations[k,1])
      sub.tech2 = sub.tech2[grepl(x = sub.tech2$task, pattern = combinations[k,2]),]
      sub.tech2 = sub.tech2[grepl(x = sub.tech2$task, pattern = Second),] 

      # statistical different between them in terms of the 30 repetitions of AUC
      stats = getPairedStats(tech1 = sub.tech1$auc, tech2 = sub.tech2$auc, conf = 0.95)
      ret = cbind(Best, Second, stats)
      # print(ret)
      return(ret)
  })

  df.stats = data.frame(do.call("rbind", aux))
  df.stats = cbind(combinations, df.stats)
  colnames(df.stats)[1:2] = c("algo", "task")
  return(df.stats)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getPairedStats = function(tech1, tech2, conf = 0.95) {

  alpha = 1 - conf
  obj = wilcox.test(x = tech1, y = tech2, paired = TRUE)
  p.value = obj$p.value

  if(is.na(p.value) | is.nan(p.value)) {
    return(FALSE)
  }
  return(p.value < alpha)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
