library(reshape2)

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
#log_feature <- dcast(log_feature, id ~ as.character(log_feature)) # ignore volume field
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

