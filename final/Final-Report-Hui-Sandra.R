# Math 189, Winter 16, Final Project

# Sherry Diep, sadiep@ucsd.edu
# Sandra Hui, s3hui@ucsd.edu
# David Lee, dal002@ucsd.edu
# Irving Valles, ivalles@ucsd.edu
# Albert Xu, a8xu@uscd.edu
# Mark Yee, mjyee@ucsd.edu

setwd("Data")
library(reshape2)
library(party)
library(caret)
library(randomForest)

########################
# set up input data sets
########################

# read in training data and clean it up
event_type <- read.csv("event_type.csv", header=T, stringsAsFactors=F)
event_type$event_type <- as.numeric(gsub("event_type ", "", event_type$event_type))
event_type <- event_type[order(event_type$id),]
event_type <- dcast(event_type, id ~ as.character(event_type))
colnames(event_type) <- c("id", paste("event_type_", colnames(event_type)[2:length(colnames(event_type))], sep=""))

log_feature <- read.csv("log_feature.csv", header=T, stringsAsFactors=F)
log_feature$log_feature<- as.numeric(gsub("feature ", "", log_feature$log_feature))
log_feature <- log_feature[order(log_feature$id),]
log_feature <- dcast(log_feature, id ~ as.character(log_feature) + as.character(volume)) # count (volume field + log_feature) field as one feature
colnames(log_feature) <- c("id", paste("log_feature_", colnames(log_feature)[2:length(colnames(log_feature))], sep=""))

resource_type <- read.csv("resource_type.csv", header=T, stringsAsFactors=F)
resource_type$resource_type <- as.numeric(gsub("resource_type ", "", resource_type$resource_type))
resource_type <- resource_type[order(resource_type$id),]
resource_type <- dcast(resource_type, id ~ as.character(resource_type))
colnames(resource_type) <- c("id", paste("resource_type_", colnames(resource_type)[2:length(colnames(resource_type))], sep=""))

severity_type <- read.csv("severity_type.csv", header=T, stringsAsFactors=F)
severity_type$severity_type <- as.numeric(gsub("severity_type ", "", severity_type$severity_type))
severity_type <- severity_type[order(severity_type$id),]
severity_type <- dcast(severity_type, id ~ as.character(severity_type))
colnames(severity_type) <- c("id", paste("severity_type_", colnames(severity_type)[2:length(colnames(severity_type))], sep=""))

train <- read.csv("train.csv", header=T, stringsAsFactors=F)
train$location <- as.numeric(gsub("location ", "", train$location))
train <- train[order(train$id),]

# merge by id
mergedCols <- merge(merge(merge(event_type, log_feature, by="id", all=T), resource_type, by="id", all=T), severity_type, by="id", all=T)
mergedTraining <- merge(train, mergedCols, id="id", all.x=T)

# remove cols that are all NA
mergedTraining <- mergedTraining[,colSums(is.na(mergedTraining)) < nrow(mergedTraining)]

# set all present values to 1, missing values to 0
mergedTraining[,4:ncol(mergedTraining)][!is.na(mergedTraining[,4:ncol(mergedTraining)])] <- 1
mergedTraining[,4:ncol(mergedTraining)][is.na(mergedTraining[,4:ncol(mergedTraining)])] <- 0
write.table(mergedTraining, file="merged_training.csv", sep=",", row.names=F, col.names=T, quote=F)


#######################################
# predict fault severity from test data
#######################################

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

# get an example of one tree using all variables
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


###############
# check results
###############

# check results with out of bag error
oobError <- caret:::cforestStats(cf) # takes ~5 mins
print(oobError)
# Accuracy     Kappa
# 0.6963826 0.3436285

# check against another randomForest package
randForRes <- randomForest(frmla, data=mergedTraining) # takes 2.5 hrs
randForRes_pred <- predict(randForRes, newdata=mergedTest)
summary(randForRes_pred)
#   0     1     2  NA's
#   1     2     0 11168

# sanity check: compare fault severity fractions between training and test datasets
comp <- NULL
comp$test <- summary(mergedTest$pred_fault_severity)
comp$train <- summary(as.factor(train$fault_severity))
comp <- as.data.frame(comp)
comp$level <- rownames(comp)
comp_frac <- NULL
comp_frac$test <- comp$test / sum(comp$test)
comp_frac$train <- comp$train / sum(comp$train)
comp_frac$level <- comp$level
comp_frac <- as.data.frame(comp_frac)
comp_frac_melt <- melt(comp_frac)
comp_frac_melt$variable <- c("Test", "Test", "Test", "Train", "Train", "Train")
p_comp_frac <- ggplot(comp_frac_melt, aes(x=level, y=value, fill=variable)) + geom_bar(stat="identity", position="dodge") + theme_bw() + labs(x="Fault Severity", y="Fraction of Cases", fill="Data set", title="Fault Severity Between Data Sets")
ggsave("p_comp_frac.png")

