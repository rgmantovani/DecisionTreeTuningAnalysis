#----------------------------------------------------------------------------------------------------------------
# Extract stats from J48 trees
#----------------------------------------------------------------------------------------------------------------

extractJ48Trees = function(job, task, lrn, tun, seed) {

   if(tun == "defaults") {

    t = system.time(model <- mlr::train(learner = lrn, task = task))
    real.model = getLearnerModel(model, more.unwrap = TRUE)

    num.leaves = real.model$classifier$measureNumLeaves()
    tree.size  = real.model$classifier$measureTreeSize()
    num.rules  = real.model$classifier$measureNumRules()
    train.time = t[3]
    feat = c(tree.size, num.leaves, num.rules, train.time)
    aux.models = list(data.frame(t(feat)))
      
  } else {
          
    load(job) 
    params = lapply(ret.params.list[[1]][[1]], function(res) {
      return(res$x)
    })

    aux.models = lapply(params, function(setting) {

      new.lrn = setHyperPars(learner = lrn, par.vals = setting)
      t <- system.time(model <- mlr::train(learner = new.lrn, task = task))
      real.model = getLearnerModel(model, more.unwrap = TRUE)

      num.leaves = real.model$classifier$measureNumLeaves()
      tree.size  = real.model$classifier$measureTreeSize()
      num.rules  = real.model$classifier$measureNumRules()
      train.time = t[3]

      feat = c(tree.size, num.leaves, num.rules, train.time)
      return(feat)
    })
  }

  df.models = data.frame(do.call("rbind", aux.models))
  colnames(df.models) = c("tree.size", "num.leaves", "num.rules", "train.time")
  df.models$rep = seed
  df.models = colMeans(df.models)

  return(df.models)
}

#----------------------------------------------------------------------------------------------------------------
# # Extract stats from CART trees
#----------------------------------------------------------------------------------------------------------------

extractRpartTrees = function(job, task, lrn, tun, seed) {

  if(tun == "defaults") {

    t = system.time(model <- mlr::train(learner = lrn, task = task))
    real.model = getLearnerModel(model, more.unwrap = TRUE)

    obj = partykit::as.party(real.model) 
    tree.size = length(obj)
    train.time = t[3]

    feat = c(tree.size, train.time)
    aux.models = list(data.frame(t(feat)))

  } else { 

    load(job) 
    params = lapply(ret.params.list[[1]][[1]], function(res) {
      return(res$x)
    })

    aux.models = lapply(params, function(setting) {
          
      new.lrn = setHyperPars(learner = lrn, par.vals = setting)
      t = system.time(model <- mlr::train(learner = new.lrn, task = task))
      real.model = getLearnerModel(model, more.unwrap = TRUE)
      

      obj = partykit::as.party(real.model) 
      tree.size = length(obj)
      train.time = t[3]
     
      feat = c(tree.size, train.time)
      return(feat)
    })
  }

  df.models = data.frame(do.call("rbind", aux.models))
  colnames(df.models) = c("tree.size", "train.time")
  df.models$rep = seed
  df.models = colMeans(df.models)
  
  return(df.models)  
}

#----------------------------------------------------------------------------------------------------------------
# Extract stats from Ctree trees
#----------------------------------------------------------------------------------------------------------------

visitTree = function(node) {

  dat <- data.frame (x=node$nodeID)
  inner.df <<- rbind(inner.df, dat)

  if(node$terminal) {
    return (NULL)
  } else {
    visitTree(node = node$left)
    visitTree(node = node$right)
  }
}

extractCTreeTrees = function(job, task, lrn, tun, seed) {
 
  if(tun == "defaults") {

    t = system.time(model <- mlr::train(learner = lrn, task = task))
    real.model = getLearnerModel(model, more.unwrap = TRUE)

    inner.df <<- data.frame()
    ok = visitTree(node = real.model@tree)

    tree.size = max(inner.df)
    train.time = t[3]

    feat = c(tree.size, train.time)
    aux.models = list(data.frame(t(feat)))

  } else { 

    load(job) 
    params = lapply(ret.params.list[[1]][[1]], function(res) {
      return(res$x)
    })

    aux.models = lapply(params, function(setting) {
      
      # print("-------------")
      # print(unlist(setting))
    
      new.lrn = setHyperPars(learner = lrn, par.vals = setting)
      err <- tryCatch({
          t <- system.time(model <- mlr::train(learner = new.lrn, task = task))
        },
        error=function(e) {
          cat(" - error when training with hyperparameter found by the technique\n")
          feat = c(NA, NA)
          print(feat)
          return(feat)
        }, 
        warning=function(w) {
          cat(" - warning while training: the model not converged \n")
          feat = c(NA, NA)
          print(feat)
          return(feat)
        }
      )
      
      # returning NAs
      if(all(is.na(err))) {
        print(err)
        return(err)
      }

      real.model = getLearnerModel(model, more.unwrap = TRUE)
     
      # visit tree and return
      inner.df <<- data.frame()
      ok = visitTree(node = real.model@tree)

      tree.size = max(inner.df)
      train.time = t[3]
      feat = c(tree.size, train.time)
      return(feat)
    })
  }

  df.models = data.frame(do.call("rbind", aux.models))
  df.models = na.omit(df.models)
  colnames(df.models) = c("tree.size", "train.time")
  df.models$rep = seed
  df.models = colMeans(df.models)

  return(df.models)
}

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
# 