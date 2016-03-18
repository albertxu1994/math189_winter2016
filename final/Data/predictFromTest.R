library(party)

ct <- ctree(fault_severity ~ . - id - location, data=mergedTraining)

test <- read.csv("test.csv", header=T, stringsAsFactors=F)
test$location <- as.numeric(gsub("location ", "", test$location))
mergedTest <- merge(test, mergedCols, id="id", all.x=T)
mergedTest <- mergedTest[,colSums(is.na(mergedTest)) < nrow(mergedTest)]
mergedTest[,4:ncol(mergedTest)][!is.na(mergedTest[,4:ncol(mergedTest)])] <- 1

