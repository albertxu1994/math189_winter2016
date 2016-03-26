library(party)

# read in test data, and merge it with the data from the other files
test <- read.csv("test.csv", header=T, stringsAsFactors=F)
test$location <- as.numeric(gsub("location ", "", test$location))
mergedTest <- merge(test, mergedCols, id="id", all.x=T)

# remove cols that are only NAs
mergedTest <- mergedTest[,colSums(is.na(mergedTest)) < nrow(mergedTest)]

# set all present values to 1, missing values to 0
mergedTest[,4:ncol(mergedTest)][!is.na(mergedTest[,4:ncol(mergedTest)])] <- 1
mergedTest[,4:ncol(mergedTest)][is.na(mergedTest[,4:ncol(mergedTest)])] <- 0

# only keep columns that are in the training set and the test set
commonCols <- colnames(mergedTraining)[colnames(mergedTraining) %in% colnames(mergedTest)]
mergedTraining <- mergedTraining[,c("fault_severity", commonCols)]
mergedTest <- mergedTest[,commonCols]

# the fault_severity depends on every variable except fault_severity, id, and location
frmla <- as.formula(paste("as.factor(fault_severity) ~ ", paste(colnames(mergedTraining)[4:ncol(mergedTraining)], collapse="+"), sep=""))

# get an example of one tree
ct <- ctree(frmla, data=mergedTraining, controls=cforest_control(mincriterion=0, mtry=NULL)) # takes ~10 mins
png("ct_volume.png", width=3600, height=1000, units="px")
plot(ct)
dev.off()

# use a forest of trees to make the predictions
# mincriterion=0 forces large trees to be grown
# mtry=NULL forces bagging
cf <- cforest(frmla, data=mergedTraining, controls=cforest_control(mincriterion=0, mtry=NULL))
mergedTest$pred_fault_severity <- predict(cf, newdata=mergedTest) # takes ~12 mins
write.table(mergedTest, file="results.csv", sep=",", quote=F, col.names=T, row.names=F)

