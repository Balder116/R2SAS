# R2SAS
Convert the simple linear model R code to SAS code

## how to install the R package
devtools::install_github("Balder116/R2SAS")

## example
SLMRtoSAS(lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,data = iris))
