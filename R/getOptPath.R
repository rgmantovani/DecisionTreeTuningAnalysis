#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

optPathAux = function(opt.path) {

  df = as.data.frame(opt.path$opt.path)
  df$ber.test.mean = 1 - df$ber.test.mean
  colnames(df)[which(colnames(df) == "ber.test.mean")] = "bac.test.mean"
  df$best.bac = 0
  for(i in 1:nrow(df)) {
    df$best.bac[i] = max(df$bac.test.mean[1:i])
  }

  sub = df[,"best.bac"]
  sub = data.frame(cbind(1:nrow(df), sub))
  colnames(sub) = c("id", "best")

  if(nrow(sub) < 900) {

    n = 900 - nrow(sub)
    aux = lapply(1:n, function(i) {
      return(sub[nrow(sub), ])
    })

    aux = data.frame(do.call("rbind", aux))
    rownames(aux) = NULL
    sub = rbind(sub, aux)
    sub$id = 1:900

  }
  return(sub)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

ExtractPathsDf = function()

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getOptPath = function(algo, all.dirs) {

  aux = lapply(all.dirs, function(data.dir) {

    dataset = gsub(x = data.dir, pattern = paste0("data/|",algo,"|/results/"), replacement = "")
    dir.create(paste0("data/cache/", algo, "/"), recursive = TRUE, showWarning = FALSE)
    outfile = paste0("data/cache/", algo, "/path_", dataset, ".RData")

    if(file.exists(outfile)) {

      load(outfile) #df.path
      df.path

    } else {

      # print(dataset)

      tech.aux = lapply(INNER.NAMES, function(tech) {

        print(tech)

        tech.dir = paste0(data.dir, "/", algo, "/", tech)
        rep.dirs = list.files(path = tech.dir, full.names = TRUE)
        rep.aux = lapply(rep.dirs, function(rep) {

          if(tech == "defaults") {

            job = paste0(rep, "/perf_", dataset, ".RData")
            load(job)
            perf = 1 - mean(ret.perf$ber)

           res.df = data.frame(cbind(1:900, rep(perf, times = 900)))
           colnames(res.df) = c("id", "defaults")

          } else {
            job = paste0(rep, "/opt_params_", dataset, ".RData")
            suppressWarnings(load(job, verbose = FALSE))

            opt.paths = ret.params.list[[1]][[1]]
            opt.list = lapply(opt.paths, optPathAux)
            res.df = Reduce("+", opt.list)/length(opt.list)
          }

          colnames(res.df)[2] = tech
          return(res.df)
        })

        rep.aux = lapply(rep.aux, function(elem) {
          aux = na.omit(elem)
          if(nrow(aux) != 900) {
            return(NULL)
          } else {
            return(aux)
          }
        })
        rep.aux = Filter(Negate(is.null), rep.aux)

        tmp = do.call("cbind",lapply(rep.aux, function(rep) {rep[,2]}))
        if(is.null(tmp)) {
          ret = data.frame(cbind(1:900, NA))
        } else {
          ret = data.frame(cbind(1:900, apply(tmp, 1, median)))
        }
        colnames(ret) = c("id", tech)

        return(ret)
      })

      df.path =  Reduce(function(...) merge(..., all=T), tech.aux)
      save(df.path, file = outfile)
    }
  })

  return(aux)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
