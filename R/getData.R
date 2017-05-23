#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# Returns all the results for all datasets/repetitions/techniques

getAllRepetitions = function(algo, all.dirs) {

  aux = lapply(all.dirs, function(data.dir) {

    inner.names = c("defaults", "pso", "ga", "eda", "random", "mbo", "irace")
    tech.aux = lapply(inner.names, function(tech) {
   
      tech.dir = paste0(data.dir, "/", algo, "/", tech)
      if(!dir.exists(path = tech.dir)) {
        return (rep(NA, 30))
      }

      rep.dirs = list.files(path = tech.dir, full.names = TRUE)
      rep.aux = lapply(rep.dirs, function(rep) {
       
        data.name = gsub(x = data.dir, pattern = paste0("data/|",algo,"|/results/"), replacement = "")
        
        read = try(load(paste0(rep, "/perf_", data.name, ".RData"),verbose = FALSE), silent = TRUE)
        if (inherits(read, "try-error")) {
          return(NA)
        }
        
        return(mean(1-ret.perf$ber))
      })
      return(do.call("rbind", rep.aux))
    }) # tech.aux

    df = data.frame(do.call("cbind", tech.aux))
  }) #aux
  
  aux = lapply(aux, function(pos){
    colnames(pos) = TECHNIQUES
    return(pos)
  })

  return(aux)  
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# getAllBestIds = function(all.dirs) {

#   if(!dir.exists(path = "ids/")) {
#     dir.create(path = "ids/")
#   }

#   aux = lapply(all.dirs, function(data.dir) {
    
#     # print(data.dir)
#     data.name = gsub(x = data.dir, pattern = "data/tuningResults/", replacement = "")
#     if(file.exists(path = paste0("ids/", data.name, ".RData"))) {
#       # cat(" - Ids already existed \n")
#       load(file = paste0("ids/", data.name, ".RData"))
#     } else {
#       # cat("- generating ids for the first time\n")
#       inner.names = c("pso", "ga", "eda", "random", "mbo")
#       tech.aux = lapply(inner.names, function(tech) {
   
#       tech.dir = paste0(data.dir, "/classif.J48/", tech)
#         if(!dir.exists(path = tech.dir)) {
#           return (as.data.frame(matrix(data = NA, ncol = 1, nrow = 30)))
#         }

#         rep.dirs = list.files(path = tech.dir, full.names = TRUE)
#         rep.aux = lapply(rep.dirs, function(rep) {
        
#           read = try(load(paste0(rep, "/opt_params_", data.name, ".RData"),verbose = FALSE), silent = TRUE)
#           if (inherits(read, "try-error")) {
#             return(data.frame())
#           }
#           traces.list = ret.params.list[[1]]$classif.J48.imputed.preproc.tuned
#           ids = lapply(traces.list, function(trace) {
#             df = as.data.frame(trace$opt.path)
#             return(which.min(df$ber))
#           })
#           return(ids)
#         })
#         return(unlist(rep.aux))
#       })
#       ret = data.frame(do.call("cbind", tech.aux))
#       colnames(ret) = TECHNIQUES[-1]
#       save(ret, file = paste0("ids/", data.name, ".RData"))
#     }
#     return(ret)
#   })
#   return(aux)
# }

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
