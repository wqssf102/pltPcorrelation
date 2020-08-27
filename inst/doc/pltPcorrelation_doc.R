## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(pltPcorrelation)
data(mydt)
res <- cal_pcor(pcor_dt =mydt)
## view result
head(res)
##plot
pltpcor(res,high = "red",low = "blue",mid = "gray")

