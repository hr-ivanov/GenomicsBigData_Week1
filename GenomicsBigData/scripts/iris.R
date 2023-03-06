dput(head(iris))
iris
saveRDS(iris, file="data_output/iris.rds")
some_data <- readRDS(file="data_output/iris.rds")
some_data
