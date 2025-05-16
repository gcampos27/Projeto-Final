library(plumber)

pr <- plumb("C:/Projeto II/PrevisãoAdaptabilidade/Back-End/plumber.R")

pr_set_debug(pr, TRUE)

pr$run(port = 8000)
