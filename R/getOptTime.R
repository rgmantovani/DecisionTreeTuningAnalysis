#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

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
      ret = do.call("rbind", rep.aux)
      return(ret)
    }) #tech.aux
    df = do.call(rowr::cbind.fill, tech.aux)
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
  ret = df.total[-remove.ids, ]
  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
