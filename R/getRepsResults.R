#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getRepsResults = function(algo, all.dirs) {

  aux = lapply(all.dirs, function(data.dir) {

    inner.names = c("defaults", "pso", "ga", "eda", "random", "mbo", "irace")
    tech.aux = lapply(inner.names, function(tech) {
      tech.dir = paste0(data.dir, "/", algo, "/", tech)
      if(!dir.exists(path = tech.dir)) {
        return (rep(NA, 30))
      }

      rep.dirs = list.files(path = tech.dir, full.names = TRUE)
      rep.aux = lapply(rep.dirs, function(rep) {
      
        dataset = gsub(x = data.dir, pattern = paste0("data/|",algo,"|/results/"), replacement = "")
        read = try(load(paste0(rep, "/perf_", dataset, ".RData"),verbose = FALSE), silent = TRUE)
        if (inherits(read, "try-error")) {
          return(NA)
        }
        
        return(mean(1-ret.perf$ber))
      })
      ret = do.call("rbind", rep.aux)
      length(ret) = 30
      return(ret)
    }) # tech.aux

    df = data.frame(do.call("cbind", tech.aux))
  }) #aux
  
  aux = lapply(aux, function(pos){
    colnames(pos) = TECHNIQUES
    return(pos)
  })

  return(aux)  
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
