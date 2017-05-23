#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getModelsInfo = function(algo) {

  files = list.files(path = paste("data", algo, "models", sep="/"), full.names=TRUE)
  aux = lapply(files, function(file) {
    return(getDatasetModelInfo(file = file))
  })
  
  df.info = do.call("rbind", aux)
  data.id = rep(x=1:length(files), each=length(unique(aux[[1]]$technique)))
  ret = cbind(df.info, data.id)
  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getDatasetModelInfo = function(file) {

  load(file, verbose = FALSE)

  # min, max, mean, median, sd (for each technique)
  unique(df.full$tun)
  aux.tun = lapply(unique(df.full$tun), function(tech) {

    ids = which(df.full$tun == tech)
    temp = df.full[ids, ]
    
    min.vec    = apply(temp[,1:4], 2, min)
    names(min.vec) = paste("min", names(min.vec), sep=".") 

    max.vec    = apply(temp[,1:4], 2, max)
    names(max.vec) = paste("max", names(max.vec), sep=".") 

    mean.vec   = apply(temp[,1:4], 2, mean)
    names(mean.vec) = paste("mean", names(mean.vec), sep=".") 

    median.vec = apply(temp[,1:4], 2, median)
    names(median.vec) = paste("median", names(median.vec), sep=".") 

    sd.vec     = apply(temp[,1:4], 2, sd)
    names(sd.vec) = paste("sd", names(sd.vec), sep=".") 

    ret = c(min.vec, max.vec, median.vec, mean.vec, sd.vec)

    return(ret)
  })
  df = data.frame(do.call("rbind", aux.tun))
  df$technique = unique(df.full$tun)
  df$dataset = gsub(x=file, pattern=paste0("data|/|models|",algo,"|.RData"), replacement="")
  return(df)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
