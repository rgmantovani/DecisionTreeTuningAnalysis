#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

.getDefaultValue = function(name) {

  switch(name,

    # J48 hyperparameters
    O = { value = "FALSE"}, 
    R = { value = "FALSE"}, 
    B = { value = "FALSE"}, 
    S = { value = "FALSE"}, 
    A = { value = "FALSE"}, 
    J = { value = "FALSE"}, 
    M = { value = 2}, 
    C = { value = 0.25}, 
    N = { value = 3}, 
  
    # CTree hyperparameters
    stump        = { value = "FALSE"}, 
    maxdepth     = { value = 30}, 
    minbucket    = { value = 7}, 
    minsplit     = { value = 20}, 
    mtry         = { value = 0}, 
    mincriterion = { value = 0.95},
  
    # CART hyperparameters
    cp             = { value = 0.01},  
    surrogatestyle = { value = "0"}, 
    usesurrogate   = { value = "2"}
  )

  return(value)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getParamsDistributionPlots = function(algo) {

  dir.create(path = "plots/params/", recursive = TRUE, showWarnings = FALSE)
 
  algo.name = gsub(x = algo, pattern="classif.", replacement = "")
  algo.path = paste("data/hptuning_full_space/", algo, "results", sep="/")
  
  # ----------------------------
  # Listing all optmization paths
  # ----------------------------
  param.files = list.files(path = algo.path, full.names = TRUE, 
    recursive = TRUE, pattern = "opt_params")
  
  # ----------------------------
  # Subsecting for one repetition and irace
  # ----------------------------
  
  random.ids = grep(x = param.files, pattern="irace")
  param.files = param.files[random.ids]

  rep.ids = grep(x = param.files, pattern="rep1")
  param.files = param.files[rep.ids]

  # ----------------------------
  # ----------------------------
  
  aux = lapply(param.files, function(job) {

    suppressWarnings(load(job)) 
    param.list = lapply(ret.params.list[[1]][[1]], function(res) {
      ret = c(unlist(res$x), res$y)
      return(ret)
    })

    df = data.frame(do.call("rbind", param.list))
    df$ber.test.mean = 1 - df$ber.test.mean
    return(df)
  })

  all.df = plyr::rbind.fill(aux)

  if(algo == "classif.J48") {
    all.df$M[all.df$M < 1]  = NA
    all.df$N[all.df$N > 10] = NA
    all.df$C[all.df$C > 0.5]  = NA
  }

  colnames(all.df)[which(colnames(all.df) == "ber.test.mean")] = "Accuracy"

  if(algo == "classif.J48") {
    all.df$O = as.factor(as.logical(all.df$O))
    all.df$R = as.factor(as.logical(all.df$R))
    all.df$B = as.factor(as.logical(all.df$B))
    all.df$S = as.factor(as.logical(all.df$S))
    all.df$A = as.factor(as.logical(all.df$A))
    all.df$J = as.factor(as.logical(all.df$J))
    all.df$M = as.numeric(all.df$M)
    all.df$C = as.numeric(all.df$C)
    all.df$N = as.numeric(all.df$N)
  }
  if(algo == "classif.ctree") {
    all.df$stump = as.factor(as.logical(all.df$stump))
  } else if(algo == "classif.rpart") {
    all.df$usesurrogate   = as.factor(all.df$usesurrogate)
    all.df$surrogatestyle = as.factor(all.df$surrogatestyle)
  }

  # -----------------
  # For all the HPs in the hyperspace
  # -----------------
  for(name in colnames(all.df)) {
    if(name != "Accuracy") {
      print(name)
      data = all.df[, c(name, "Accuracy")]
      temp = melt(data, id.vars = 2)
      g = ggplot(na.omit(temp), mapping = aes(x = value))
      if(class(data[,1]) != "numeric") {
        g = g + geom_bar(aes(fill=factor(value)), colour = "black", position = "dodge", width = 0.5)
      } else {
        g = g + geom_density(aes(fill=variable))
      }
      g = g + geom_vline(xintercept = .getDefaultValue(name = name), linetype="dashed", color = "black")
      g = g + guides(fill=FALSE) 
      g = g + theme_bw()

      # -----------------
      # exporting files
      # -----------------
      
      ggsave(g, file = paste0("plots/params/", algo.name, "_", name, "_param.pdf"),  width = 4.13, height = 2.95, dpi = 500)
      ggsave(g, file = paste0("plots/params/", algo.name, "_", name, "_param.jpeg"), width = 4.13, height = 2.95, dpi = 500)
      ggsave(g, file = paste0("plots/params/", algo.name, "_", name, "_param.eps"),  width = 4.13, height = 2.95, dpi = 500)
    
      # -----------------
      
    }
  }
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
