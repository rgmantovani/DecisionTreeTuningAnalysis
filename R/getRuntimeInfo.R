#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getTrainingTime = function(algo, all.dirs) {

  local.dir = paste0("data/", algo, "/runtime/training/")
  if(!dir.exists(path = local.dir)) {
    dir.create(path = local.dir, recursive = TRUE)
  }

  inner.names = c("defaults", "pso", "ga", "eda", "random", "mbo", "irace")

  # For each one of the datasets
  aux = lapply(all.dirs, function(data.dir) {
    
    print(data.dir)
    data.name = gsub(x = data.dir, pattern = paste0("data/|",algo,"|/results/"), replacement = "")
    job.file = paste0(local.dir, data.name, ".RData")

    if(file.exists(file = job.file)) {
      load(file = job.file)
    } else {

      tech.aux = lapply(inner.names, function(tech) {
        tech.dir = paste0(data.dir, "/", algo, "/", tech)
        if(!dir.exists(path = tech.dir)) {
          vet = c(NA, NA, NA, NA, tech, data.name, "training")
          return (vet)
        }

        rep.dirs = list.files(path = tech.dir, full.names = TRUE)
        rep.aux = lapply(rep.dirs, function(rep) {

          if(tech == "defaults") {
            read = try(load(paste0(rep, "/perf_", data.name, ".RData"),verbose = FALSE), silent = TRUE)
            if (inherits(read, "try-error")) { return(NA) }
            ret = mean(ret.perf$timetrain)
          } else {
            read = try(load(paste0(rep, "/opt_params_", data.name, ".RData"),verbose = FALSE), silent = TRUE)
            if (inherits(read, "try-error")) { return(NA) }
            inner.aux = lapply(ret.params.list[[1]][[1]], function(it) {
              v = data.frame(it$opt.path)$exec.time
              return(mean(v))
            })
            ret = mean(unlist(inner.aux))
          }
          return(ret)
        }) # rep.aux

        tmp = unlist(rep.aux)
        values = c(median(tmp, na.rm = TRUE), min(tmp, na.rm = TRUE), max(tmp, na.rm = TRUE), sd(tmp, na.rm = TRUE))
        df.ret = data.frame(t(values))
        df.ret$technique = tech
        df.ret$dataset   = data.name
        df.ret$time = "training"
        colnames(df.ret)[1:4] = c("median", "min", "max", "sd")
        return(df.ret)
     
      }) # tech.aux
      
      df = data.frame(do.call("rbind", tech.aux))
      save(x = df, file = job.file)
    }
    return(df)
  })
  df.training = do.call("rbind", aux)
  return(df.training)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------