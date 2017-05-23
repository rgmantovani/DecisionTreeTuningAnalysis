#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getStatsDf = function(all.results, conf = 0.95) {

  alpha = 1 - conf

  inner.aux = lapply(all.results, function(mat) { 
  
    max.id = which.max(colMeans(mat[, -1]))
    alg = mat[,names(max.id)]
    dfs = mat[,"defaults"]
    obj = wilcox.test(x = dfs, y = alg, paired = TRUE)
    p.value = obj$p.value
   
    if(mean(dfs) >= mean(alg)) {
      if(is.na(p.value)) return("DF-noSign")
      if(p.value >= alpha) {
        return("DF-noSign")
      } else {
        return("DF-Sign")
      }
    } else {
      if(is.na(p.value)) return("Other-noSign")
       if(p.value >= alpha) {
        return("Other-noSign")
      } else {
        return("Other-Sign")
      }
    }
  })

  df.stats = data.frame(do.call("rbind", inner.aux))
  colnames(df.stats) = c("Sign")
  return(df.stats)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
