#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

aggregateIds = function(fullIds, measure = "mean") {

  aux = lapply(fullIds, function(mat) {
    inner.aux = lapply(1:30, function(i){
      begin = ((i-1) * 10) + 1
      end   = i * 10
      return(apply(mat[begin:end, ], 2, measure))
    })
    return(do.call("rbind", inner.aux))
  })
  
  aux = lapply(aux, function(mat){
    return(colMeans(mat))
  })
  av.ids = data.frame(do.call("rbind", aux))
  return(av.ids)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

aggregatePerformance = function(all.results, measure = "mean") {

  aux = lapply(all.results, function(mat) {
    return(apply(mat, 2, measure))
  })
  av.perf = data.frame(do.call("rbind", aux))
  return(av.perf)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
