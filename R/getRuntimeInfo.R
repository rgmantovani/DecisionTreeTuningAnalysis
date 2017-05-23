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
    
    data.name = gsub(x = data.dir, pattern = paste0("data/|",algo,"|/results/"), replacement = "")

    job.file = paste0(local.dir, data.name, ".RData")
    if(file.exists(file = job.file)) {
   
      # cat(" loading previous \n")
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
        values = c(median(tmp), min(tmp), max(tmp), sd(tmp))
        df.ret = data.frame(t(values))
        df.ret$technique = tech
        df.ret$dataset   = data.name
        df.ret$time = "training"
        colnames(df.ret)[1:4] = c("median", "min", "max", "sd")
        return(df.ret)
     
      }) # tech.aux
      
      df = data.frame(do.call("rbind", tech.aux))
      # cat(" saving new \n")
      save(x = df, file = job.file)
    }
    return(df)
  })
  df.training = do.call("rbind", aux)
  return(df.training)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# Does not need temp files (it runs faster)

getAllRuntime = function(algo, all.dirs) {

  inner.names = c("defaults", "pso", "ga", "eda", "random", "mbo", "irace")
  aux = lapply(all.dirs, function(data.dir) {

    data.name = gsub(x = data.dir, pattern = paste0("data/|",algo,"|/results/"), replacement = "")
    tech.aux = lapply(inner.names, function(tech) {
  
      tech.dir = paste0(data.dir, "/", algo, "/", tech)
      if(!dir.exists(path = tech.dir)) {

        mat = matrix(data = NA, ncol = 2, nrow = 30)
        colnames(mat) = c("timetrain", "timepredict")
        return (mat)
      }

      rep.dirs = list.files(path = tech.dir, full.names = TRUE)
      rep.aux = lapply(rep.dirs, function(rep) {
        read = try(load(paste0(rep, "/perf_", data.name, ".RData"),verbose = FALSE), silent = TRUE)
        if (inherits(read, "try-error")) { return(NA) }
        return(colMeans(ret.perf[,6:7]))
      })
      return(do.call("rbind", rep.aux))
    }) #tech.aux

    df = data.frame(do.call("cbind", tech.aux))
    return(df)
  })

  z = outer(inner.names, c("tuningTime", "testingTime"), paste, sep=".") 

  aux = lapply(aux, function(pos){
    colnames(pos) = as.vector(t(z)) 
    return(pos)
  })

  return(aux)  
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getTuningTimes = function(algo, all.dirs) {

  datasets  = gsub(x = all.dirs, pattern = paste0("data/|",algo,"|/results/"), replacement = "")
  time.list = getAllRuntime(algo = algo, all.dirs = all.dirs)

  tuning.list = lapply(time.list, function(elem) {
    ids     = grep("tuningTime", colnames(elem))
    sel     = elem[, ids]
    medians = apply(sel, 2, sd)
    mins    = apply(sel, 2, sd)
    maxs    = apply(sel, 2, sd)
    sds     = apply(sel, 2, sd)

    inner.df = data.frame(cbind(medians, mins, maxs, sds))
    inner.df$technique = gsub(x=rownames(inner.df), pattern = ".tuningTime", replacement="")
    rownames(inner.df) = NULL
    return(inner.df)
  })

  df.tuning = do.call("rbind", tuning.list)
  df.tuning$dataset = rep(datasets, each=length(TECHNIQUES))
  df.tuning$time    = "tuning"
  colnames(df.tuning)[1:4] = c("median", "min", "max", "sd")


  testing.list = lapply(time.list, function(elem) {
    ids     = grep("testingTime", colnames(elem))
    sel     = elem[, ids]
    medians = apply(sel, 2, sd)
    mins    = apply(sel, 2, sd)
    maxs    = apply(sel, 2, sd)
    sds     = apply(sel, 2, sd)

    inner.df = data.frame(cbind(medians, mins, maxs, sds))
    inner.df$technique = gsub(x=rownames(inner.df), pattern = ".testingTime", replacement="")
    rownames(inner.df) = NULL
    return(inner.df)
  })

  df.testing = do.call("rbind", testing.list)
  df.testing$dataset = rep(datasets, each=length(TECHNIQUES))
  df.testing$time    = "testing"
  colnames(df.testing)[1:4] = c("median", "min", "max", "sd")

  df.total = rbind(df.tuning, df.testing)

  # remove defaults with tuning (there is no tuning for defaults)
  remove.ids = which(df.total$technique == "defaults" & df.total$time == "tuning")

  return(df.total[-remove.ids, ])
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------