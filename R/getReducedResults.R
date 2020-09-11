#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getReducedResults = function(algo, tech) {

  all.dirs = list.files("data/reducedSpace/output/", full.names = TRUE)

  aux = lapply(all.dirs, function(data.dir) {
    # print(data.dir)
    tech.dir = paste0(data.dir,"/", algo, "/", tech)
    if(!dir.exists(path = tech.dir)) {
        return (rep(NA, 30))
    }

    rep.dirs = list.files(path = tech.dir, full.names = TRUE)
    rep.aux = lapply(rep.dirs, function(rep) {
      
      dataset = gsub(x = data.dir, 
        pattern = paste0("data/reducedSpace/output//|",algo,"|/results/"), 
        replacement = "")
        
      read = try(suppressWarnings(load(paste0(rep, "/perf_", dataset, ".RData"),
        verbose = FALSE)), silent = TRUE)
        
      if (inherits(read, "try-error")) {
        return(NA)
      }        
      return(mean(1-ret.perf$ber))
    })
      
    ret = do.call("rbind", rep.aux)
    length(ret) = 30
    return(ret)
    df = data.frame(ret)
    return(df)
  })

  return(aux)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
