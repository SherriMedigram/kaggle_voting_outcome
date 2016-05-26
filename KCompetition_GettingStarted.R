# KAGGLE COMPETITION - GETTING STARTED

# This script file is intended to help you get started on the Kaggle platform, and to show you how to make a submission to the competition.


# Let's start by reading the data into R
# Make sure you have downloaded these files from the Kaggle website, and have navigated to the directory where you saved the files on your computer

train = read.csv("train2016.csv")
test = read.csv("test2016.csv")

# We will just create a simple logistic regression model, to predict Party using all other variables in the dataset, except for the user ID:
SimpleMod = glm(Party ~ . -USER_ID, data=train, family=binomial)

# And then make predictions on the test set:
PredTest = predict(SimpleMod, newdata=test, type="response")

threshold = 0.5

PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):
MySubmission = data.frame(USER_ID = test$USER_ID, PREDICTION = PredTestLabels)
colnames(MySubmission) <- c("USER_ID","Predictions")
write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE, quote=FALSE)
