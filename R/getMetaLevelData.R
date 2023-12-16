#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getMetaLevelData = function(all.files) {

  aux = lapply(all.files, function(file){
  	# print(file)
    values = load(file, verbose = FALSE) 
    ret = get(values)
  	return(ret)
  })

  df = do.call("rbind", aux)

  # selecting only 95% of the statistical rule
  df = df[grepl(df$task, pattern = "_95_"), ]

  # featsel == none ,tuning  == none, balance == none,norm == no_norm, resamp  == 10-CV
  sub = dplyr::filter(df, featsel == "none" & tuning == "none" & balance == "none" 
  	& norm == "no_norm" & resamp == "10-CV")

  # selecting just the simple results
  # [task, algo, auc, rep]
  select.columns = c("task", "algo", "auc", "rep")
  ret =  sub[,which(colnames(sub) %in% select.columns)]

  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

aggregateMetaLevelData = function(meta.data) {

	algos = unique(meta.data$algo)
	tasks = unique(meta.data$task)

	combinations = expand.grid(algos, tasks)

	aux = lapply(1:nrow(combinations), function(k) {
		sel = dplyr::filter(meta.data, task == combinations[k,2] & algo == combinations[k,1])
		sel = sel[1:30, ]

		ret = sel[1,1:2]
		ret$avgAuc = mean(sel$auc)
		ret$sdAuc  = sd(sel$auc)
		return(ret)
	})

	avg.df = do.call("rbind", aux)
	
	avg.df$Setup = gsub(x = avg.df$task, 
    pattern = "classif.J48_95_165d_|classif.rpart_95_165d_|_original_dist|classif.J48_165d_95_|classif.rpart_165d_95_", 
    replacement = "")

  # renaming task
  avg.df$task = gsub(x = avg.df$task, pattern = "classif.|_95_165d_|_165d_95_|complex_|simple_|all_|original_dist", 
  	replacement = "")
  avg.df$task = as.factor(avg.df$task)
  avg.df$algo = factor(avg.df$algo, levels = c("RF", "SVM", "GP", "KNN", "CART", "NB", "LR"))

	return(avg.df)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------