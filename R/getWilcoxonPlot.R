#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

.innerAuxFnc = function(mat, alpha) {

  df = c()
  for(i in 1:nrow(mat)) {
    for(j in 1:ncol(mat)) {
      if (i < j) {
       vect = c(TECHNIQUES[i], TECHNIQUES[j], mat[i,j], alpha)
      } else {
        vect = c(TECHNIQUES[i], TECHNIQUES[j], NA, alpha)
      }
      df = rbind(df, vect)
    }
  }
  df = data.frame(df)
  colnames(df) = c("Tech1", "Tech2", "Sign", "Alpha")
  return(df)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getWilcoxonPlot = function(obj) {

  # organize data
  df1 = .innerAuxFnc(mat = obj$ret.01, alpha = 0.01)
  df2 = .innerAuxFnc(mat = obj$ret.05, alpha = 0.05)
  df3 = .innerAuxFnc(mat = obj$ret.1,  alpha = 0.1)

  df.comp = rbind(df1, df2, df3)

  df.comp$Sign = as.character(df.comp$Sign)
  df.comp$Sign[which(df.comp$Sign == "TRUE")]  = 0.5
  df.comp$Sign[which(df.comp$Sign == "FALSE")] = NA
  df.comp$Sign[which(is.na(df.comp$Sign))] = NA
  df.comp$Sign = as.numeric(df.comp$Sign)

  df.comp$Tech1 = factor(df.comp$Tech1, levels = rev(TECHNIQUES))
  df.comp$Tech2 = factor(df.comp$Tech2, levels = rev(TECHNIQUES))

  g = ggplot(df.comp, mapping = aes(x = factor(Tech2), y = factor(Tech1),
    value = Sign, colour = Alpha, group = Alpha, shape = Alpha, size = Sign))
  g = g + geom_point(position = position_dodge(width = .5))
  g = g + scale_size_area(max_size = 1.7) + guides(size = FALSE)
  g = g + theme_minimal()
  g = g + scale_color_manual(values = c("blue", "black", "red"))
  g = g + labs(x="",y="")
  g = g + scale_x_discrete(breaks = TECHNIQUES, labels = TECHNIQUES)

  return(g)

}


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
