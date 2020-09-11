#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getStatsDf = function(all.results, conf = 0.95) {

  alpha = 1 - conf

  inner.aux = lapply(all.results, function(mat) { 
  
    max.id = which.max(colMeans(x = mat[, -1], na.rm = TRUE))
    alg = mat[,names(max.id)]
    dfs = mat[,"defaults"]
    obj = wilcox.test(x = dfs, y = alg, paired = TRUE)
    p.value = obj$p.value
   
    if(mean(x = dfs, na.rm = TRUE) >= mean(x = alg, na.rm = TRUE)) {
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

getStatMat = function(results) {

  p.mat = matrix(NA, ncol = length(TECHNIQUES), nrow=length(TECHNIQUES))
  colnames(p.mat) = TECHNIQUES
  rownames(p.mat) = TECHNIQUES

  for(i in 1:nrow(p.mat)) {
    for(j in 1:ncol(p.mat)) {
      if (i != j) {
        obj = wilcox.test(x  = results[,i], y = results[,j] , paired = TRUE)
        p.mat[i,j] = obj$p.value
      }
    }
  }

  ret.01 = p.mat < 0.01 # conf = 0.99 %
  ret.05 = p.mat < 0.05 # conf = 0.95 %
  ret.1  = p.mat < 0.1  # conf = 0.90%

  obj = list(ret.01 = ret.01, ret.05 = ret.05, ret.1 = ret.1)
  return(obj)
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
