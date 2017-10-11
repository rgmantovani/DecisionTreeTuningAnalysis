#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getCorVector = function(params.cor, acc.cor) {

  aux1 = data.frame(t(acc.cor))
  colnames(aux1) = paste0(colnames(aux1), ".Perf")

  # combining params.cor and acc.cor 
  aux2 = params.cor[upper.tri(params.cor)]
  values = expand.grid(colnames(params.cor), row.names(params.cor))
  values = paste(values[,1], values[,2], sep=".")
  values = values[upper.tri(params.cor)]
  names(aux2) = values

  ret = unlist(c(aux2, aux1))
  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

innerAuxTech = function(algo, tech, dataset) {

  tech.dir = paste0("data/", algo, "/results/", dataset, "/", algo, "/", tech)
  if(!dir.exists(path = tech.dir)) {
    return(data.frame())
  }

  rep.dirs = list.files(path = tech.dir, full.names = TRUE)
  rep.aux = lapply(rep.dirs, function(rep) {
      
    rep.file = paste0(rep, "/opt_params_", dataset, ".RData")
    read = try(suppressWarnings(load(rep.file, verbose = FALSE)), silent = TRUE)

    if (inherits(read, "try-error")) { 
      return(data.frame()) 
    }

    traces.list = ret.params.list[[1]][[1]]
    inner.traces = lapply(traces.list, function(trace) {
      return(as.data.frame(trace$opt.path))
    })

    inner.traces = do.call("rbind", inner.traces)
    return(inner.traces)
  })

  rep.traces = do.call("rbind", rep.aux)
  return(rep.traces)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getCorrData = function(algo, dataset, cor.dir) {

  job = paste0(cor.dir, "/", dataset, ".RData")
  if(file.exists(path = job)){
    # cat(" - Correlations already calculated \n")
    suppressWarnings(load(file = job))
  } else {

    cat("- generating corr for the first time\n")
    inner.names = c("pso", "ga", "eda", "random", "mbo", "irace")

    tech.aux = lapply(inner.names, function(tech) {
      return(innerAuxTech(algo = algo, tech = tech, dataset = dataset))
    }) 

    techs.traces = plyr::rbind.fill(tech.aux)
    techs.traces$dob = techs.traces$eol = NULL
    techs.traces$error.message = techs.traces$exec.time = NULL
    techs.traces$ber.test.mean = 1 - techs.traces$ber.test.mean

    big.df = techs.traces
    big.df[is.na(big.df)] = 0
    colnames(big.df)[ncol(big.df)] = "Perf"
    perf.id = ncol(big.df)
     
    params.cor = cor(x = big.df[ ,1:(perf.id-1)], method = "spearman", use= "everything")
    acc.cor = cor(x = big.df[ ,1:(perf.id-1)], y = big.df[,perf.id], method = "spearman", 
      use = "everything")

    ret = getCorVector(params.cor = params.cor, acc.cor = acc.cor)
    save(ret, file = job)
  }

  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
