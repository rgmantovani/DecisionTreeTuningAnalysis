#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getFanovaPlot = function() {

  # TODO: Generalize

  # # read results
  # files = list.files(path = "data/outputs_fanova/", full.names = TRUE)
  # files = files[1:82]

  # aux = lapply(files, function(file) {
  #   print(file)
  #   obj = read.csv(file, header = FALSE)
  #   df = data.frame(t(obj[,1]))
  #   c.names = obj[,2]
  #   c.names = gsub(x = c.names, pattern = "X0", replacement = "C")
  #   c.names = gsub(x = c.names, pattern = "X1", replacement = "M")
  #   c.names = gsub(x = c.names, pattern = "X2", replacement = "N")
  #   c.names = gsub(x = c.names, pattern = "X3", replacement = "O")
  #   c.names = gsub(x = c.names, pattern = "X4", replacement = "R")
  #   c.names = gsub(x = c.names, pattern = "X5", replacement = "B")
  #   c.names = gsub(x = c.names, pattern = "X6", replacement = "S")
  #   c.names = gsub(x = c.names, pattern = "X7", replacement = "A")
  #   c.names = gsub(x = c.names, pattern = "X8", replacement = "J")
  #   colnames(df) = c.names
  #   return(df)
  # })

  # mat = plyr::rbind.fill(aux)
  # data.id = 1:nrow(mat)
  # mat = cbind(data.id, mat)

  # colnames(mat) = gsub(x = colnames(mat), pattern = "x", replacement = "vs")

  # # Just Hyper-parameters
  # ids = which(colnames(mat) %in% c("data.id", "C", "M", "N", "O", "R", "B", "S", "A", "J"))
  # sub.mat = cbind(mat[, ids], mat[, -ids])

  # melted.df = melt(sub.mat, id.vars = 1)
  # melted.df$value = melted.df$value /100

  # g = ggplot(melted.df, aes(x = data.id, y = variable, fill = value))
  # g = g + geom_tile() #colour = 'black')
  # g = g + theme_classic()
  # g = g + xlab("Dataset") + ylab("J48 HPs or pair of HPs")
  # g = g + theme(axis.text.y = element_text(size = 7))
  # g = g + theme(legend.title =  element_text(size = 10))

  # g = g + scale_fill_gradient2(low = "white", high = "darkred", mid = "red",
  #    midpoint = 0.5, space = "Lab", limit = c(0, 1),#
  #    name="functional \nAnova marginals\n")

  # ggsave(g, file = "output/fanovaHeatmap.pdf", dpi = 500, width = 7, height = 5, units = "in")

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

