#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

library("reshape2")
library("ggplot2")
library("gridExtra")
library("checkmate")
library("mlr")

mlr::configureMlr(on.learner.error = "warn")
mlr::configureMlr(show.info = TRUE)

BUDGET = 900

TECHNIQUES = c("defaults", "PSO", "GA", "EDA", "RS", "SMBO", "Irace")

CUSTOM.COLORS = c("black", "purple", "green", "darkorange", "red", 
  "darkblue", "darkcyan", "darkgreen", "darkmagenta", "darkgoldenrod", 
  "antiquewhite4")

CUSTOM.LINETYPES = c("dotted", "solid", "solid", "solid", "solid", 
  "solid", "solid", "dashed", "solid", "dashed", "solid")

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
