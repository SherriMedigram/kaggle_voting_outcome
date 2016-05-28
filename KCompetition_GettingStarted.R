finalOutput = FALSE

################################################################################
# Feature engineering                                                          #
################################################################################

# Read data from files
data = read.csv("train2016.csv")
evaluation = read.csv("test2016.csv")

### Remove all NA values
data[is.na(data)] <- 1980

### Creating train and test set
if (finalOutput) {
	train = data
	test = evaluation
} else {
	library(caTools)
	set.seed(42)
	spl = sample.split(data$Party, SplitRatio = 0.7)
	train = subset(data, spl==TRUE)
	test = subset(data, spl==FALSE)
}

################################################################################
# Creation model                                                               #
################################################################################

# We will just create a simple logistic regression model, to predict Party using all other variables in the dataset, except for the user ID:
model = glm(Party ~ . - USER_ID, data=train, family=binomial)

# And then make predictions on the evaluation set:
predictions = predict(model, newdata=test)

###############################################################################
# Output                                                                       #
################################################################################
threshold = 0.5

if (finalOutput) {
	PredTestLabels = as.factor(ifelse(predictions<threshold, "Democrat", "Republican"))

	# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):
	MySubmission = data.frame(USER_ID = evaluation$USER_ID, PREDICTION = PredTestLabels)
	colnames(MySubmission) <- c("USER_ID","Predictions")
	write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE, quote=FALSE)
} else {
	confusionMatrix = table(test$Party, predictions > threshold)
	cat("accuracy", sum(diag(confusionMatrix))/sum(confusionMatrix), "\n")
}
