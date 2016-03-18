library(party)

# read in test data, and merge it with the data from the other files
test <- read.csv("test.csv", header=T, stringsAsFactors=F)
test$location <- as.numeric(gsub("location ", "", test$location))
mergedTest <- merge(test, mergedCols, id="id", all.x=T)
mergedTest <- mergedTest[,colSums(is.na(mergedTest)) < nrow(mergedTest)]
mergedTest[,4:ncol(mergedTest)][!is.na(mergedTest[,4:ncol(mergedTest)])] <- 1

# only keep columns that are in the training set and the test set
commonCols <- colnames(mergedTraining)[colnames(mergedTraining) %in% colnames(mergedTest)]
mergedTraining <- mergedTraining[,c("fault_severity", commonCols)]
mergedTest <- mergedTest[,commonCols]

# the fault_severity depends on every variable except fault_severity, id, and location
frmla <- as.formula(paste("as.factor(fault_severity) ~ ", paste(colnames(mergedTraining)[4:ncol(mergedTraining)], collapse="+"), sep=""))

# try one tree...?
ct <- ctree(frmla, data=mergedTraining)
#png("ct.png")
#plot(ct)
#dev.off()
tmp <- predict(ct, newdata=mergedTest)

# try a forest of trees...?
cf <- cforest(frmla, data=mergedTraining)
tmpF <- predict(cf, newdata=mergedTest)
