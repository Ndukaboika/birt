## code to prepare `DATASET` dataset goes here


algebra <- read.csv("data-raw/algebra.csv")
algebra <- as.matrix(algebra)
usethis::use_data(algebra, overwrite = TRUE)
