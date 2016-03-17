event_type <- read.csv("event_type.csv", header=T, stringsAsFactors=F)
event_type$event_type <- as.numeric(gsub("event_type ", "", event_type$event_type))

log_feature <- read.csv("log_feature.csv", header=T, stringsAsFactors=F)
log_feature$log_feature<- as.numeric(gsub("feature ", "", log_feature$log_feature))

resource_type <- read.csv("resource_type.csv", header=T, stringsAsFactors=F)
resource_type$resource_type <- as.numeric(gsub("resource_type ", "", resource_type$resource_type))

severity_type <- read.csv("severity_type.csv", header=T, stringsAsFactors=F)

severity_type$severity_type <- as.numeric(gsub("severity_type ", "", severity_type$severity_type))

test <- read.csv("test.csv", header=T, stringsAsFactors=F)
test$location <- as.numeric(gsub("location ", "", test$location))

