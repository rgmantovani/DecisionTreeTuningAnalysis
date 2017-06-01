#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

renameVector = function(df, algo) {

  temp = df
  colnames(temp) = c("Fanova", "HPs")

  if(algo == "classif.J48") {
  
    temp$HPs = gsub(x = temp$HPs, pattern = "X0", replacement = "C")
    temp$HPs = gsub(x = temp$HPs, pattern = "X1", replacement = "M")
    temp$HPs = gsub(x = temp$HPs, pattern = "X2", replacement = "N")
    temp$HPs = gsub(x = temp$HPs, pattern = "X3", replacement = "O")
    temp$HPs = gsub(x = temp$HPs, pattern = "X4", replacement = "R")
    temp$HPs = gsub(x = temp$HPs, pattern = "X5", replacement = "B")
    temp$HPs = gsub(x = temp$HPs, pattern = "X6", replacement = "S")
    temp$HPs = gsub(x = temp$HPs, pattern = "X7", replacement = "A")
    temp$HPs = gsub(x = temp$HPs, pattern = "X8", replacement = "J")
  
  } else {
  
    temp$HPs = gsub(x = temp$HPs, pattern = "X0", replacement = "cp")
    temp$HPs = gsub(x = temp$HPs, pattern = "X1", replacement = "minsplit")
    temp$HPs = gsub(x = temp$HPs, pattern = "X2", replacement = "minbucket")
    temp$HPs = gsub(x = temp$HPs, pattern = "X3", replacement = "maxdepth")
    temp$HPs = gsub(x = temp$HPs, pattern = "X4", replacement = "usesurrogate")
    temp$HPs = gsub(x = temp$HPs, pattern = "X5", replacement = "surrogatestyle")
  
  }

  ret = temp[,1]
  names(ret) = temp[,2]
  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getFanovaData = function(algo) {

  tun.dirs = list.files(path = paste0("data/", algo, "/results/"))

  fan.dir  = paste("data", algo, "fanova", sep="/")
  files = list.files(path = fan.dir)

  ids = which(gsub(x = files, pattern="rpart_|.csv", replacement="") %in% tun.dirs)
  inter.files = files[ids]

  aux = lapply(inter.files, function(file) {
    df = read.csv(file = paste0(fan.dir, "/", file), header = FALSE)
    df2 = renameVector(df = df, algo = algo)
    return(df2)
  })

  df = do.call("rbind", aux)
  datasets = gsub(x = inter.files, pattern = ".csv", replacement = "")
  rownames(df) = datasets

  return(df)
}

#--------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------