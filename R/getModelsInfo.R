#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getModelsInfo = function(algo) {

  # ---------------------------------------------
  # listing models' files previously extracted
  # ---------------------------------------------
  files = list.files(path = paste("data/hptuning_full_space", algo, "models_stats", sep="/"), 
    full.names=TRUE)
  
  # ---------------------------------------------
  # Retrieving models stats 
  # ---------------------------------------------
  aux = lapply(files, function(file) {
    return(getDatasetModelInfo(file = file))
  })
  
  # ---------------------------------------------
  # Generating a data frame
  # ---------------------------------------------

  df.info = do.call("rbind", aux)
  data.id = as.numeric(as.factor(df.info$dataset))
  ret = cbind(df.info, data.id)
  return(ret)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getDatasetModelInfo = function(file) {

  load(file, verbose = FALSE)

  aux.tun = lapply(unique(df.full$tun), function(tech) {

    ids = which(df.full$tun == tech)
    temp = df.full[ids, ]
    
    min.vec = apply(temp[,1:(ncol(temp)-2)], 2, min, na.rm = TRUE)
    names(min.vec) = paste("min", names(min.vec), sep=".") 

    max.vec = apply(temp[,1:(ncol(temp)-2)], 2, max, na.rm = TRUE)
    names(max.vec) = paste("max", names(max.vec), sep=".") 

    mean.vec = apply(temp[,1:(ncol(temp)-2)], 2, mean, na.rm = TRUE)
    names(mean.vec) = paste("mean", names(mean.vec), sep=".") 

    median.vec = apply(temp[,1:(ncol(temp)-2)], 2, median, na.rm = TRUE)
    names(median.vec) = paste("median", names(median.vec), sep=".") 

    sd.vec = apply(temp[,1:(ncol(temp)-2)], 2, sd, na.rm = TRUE)
    names(sd.vec) = paste("sd", names(sd.vec), sep=".") 

    ret = c(min.vec, max.vec, median.vec, mean.vec, sd.vec)

    return(ret)
  })
  df = data.frame(do.call("rbind", aux.tun))
  df$technique = unique(df.full$tun)
  df$dataset = gsub(x=file, pattern=paste0("data/hptuning_full_space/|/|models_stats|",algo,"|.RData"), replacement="")
  return(df)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
