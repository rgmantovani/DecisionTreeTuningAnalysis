#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

checkExperiments = function(algo) {

  datasets = read.csv(file = "data/datasets_list.csv")

  algo.dir = paste("data", algo, "results", sep="/")
  aux = lapply(datasets$UCI.Dataset.name, function(data) {

    print(as.character(data))
    data.dir = paste(algo.dir, data, sep="/")

    if(!dir.exists(path = data.dir)) {
      df = data.frame(t(rep(x = 0, time=7)))
      colnames(df) = c("defaults", "eda", "ga", "irace", "mbo", "pso", "random")
      return(df)
    }

    # techniques
    tech.dirs = list.files(path = paste(data.dir, algo, sep="/"))

    aux.tech = lapply(tech.dirs, function(tech) {
      rep.dirs = list.files(paste(data.dir, algo, tech, sep="/"))
      aux.rep = lapply(rep.dirs, function(rep) {
        job.file = paste0(paste(data.dir, algo, tech, rep, sep="/"), "/perf_", data, ".RData")
        return(file.exists(job.file))
      })
      count = length(which(unlist(aux.rep)))
      return(count)    
    })
    df = data.frame(t(do.call("rbind", aux.tech)))
    colnames(df) = tech.dirs
    return(df)
  })

  df.full = plyr::rbind.fill(aux)
  rownames(df.full) = datasets$UCI.Dataset.name
  write.csv(df.full, file = paste0(algo, "_check.csv")) 

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

checkExperiments(algo = "classif.J48")
checkExperiments(algo = "classif.rpart")

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
