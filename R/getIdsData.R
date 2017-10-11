#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

innerAuxTechIds = function(algo, tech, dataset) {

  tech.dir = paste0("data/", algo, "/results/", dataset, "/", algo, "/", tech)

  if(!dir.exists(path = tech.dir)) {
    return(rep(x = NA, times = 30))
  }

  rep.dirs = list.files(path = tech.dir, full.names = TRUE)
  rep.aux = lapply(rep.dirs, function(rep) {

    rep.file = paste0(rep, "/opt_params_", dataset, ".RData")
    read = try(suppressWarnings(load(rep.file, verbose = FALSE)), silent = TRUE)

    if (inherits(read, "try-error")) { 
      return(data.frame()) 
    }

    traces.list = ret.params.list[[1]][[1]]
    ids = lapply(traces.list, function(trace) {
      df = as.data.frame(trace$opt.path)
      return(which.min(df$ber))
    })
    return(ids)
  })
  return(unlist(rep.aux))
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------


getIdsData = function(algo, dataset, ids.dir) {

  job = paste0(ids.dir, "/", dataset, ".RData")
  if(file.exists(path = job)) {
    suppressWarnings(load(file = job))
  } else {

    inner.names = c("pso", "ga", "eda", "random", "mbo", "irace")
    tech.aux = lapply(inner.names, function(tech){
      return(innerAuxTechIds(algo = algo, tech = tech, dataset = dataset))
    })

    ret = data.frame(do.call("cbind", tech.aux))
    colnames(ret) = TECHNIQUES[-1]
    save(ret, file = job)
  }
  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
