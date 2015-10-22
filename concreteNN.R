require(gdata)
require(neuralnet)

# Loading data in the memory
concrete <- read.xls("data/Concrete_Data.xls", sheet=1, header=TRUE)

str(concrete)

# Typically, the solution to this problem is to rescale the data with a normalizing or
# standardization function. If the data follow a bell-shaped curve (a normal distribution
# as described in Chapter 2, Managing and Understanding Data), then it may make sense
# to use standardization via R's built-in scale() function. On the other hand, if the data
# follow a uniform distribution or are severely non-normal, then normalization to a 0-1
# range may be more appropriate. In this case, we'll use the latter.
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

concrete_norm <- as.data.frame(lapply(concrete, normalize))

# To confirm that the normalization worked, we can see that the minimum and
# maximum strength are now 0 and 1, respectively:
summary(concrete_norm$strength)

# In comparison, the original minimum and maximum values were 2.33 and 82.6:
summary(concrete$strength)


# Train set with 75% and Test set with 25%  
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

# We'll begin by training the simplest multilayer feedforward network with only
# a single hidden node:
concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data=concrete_train)
#plot(concrete_model)

# Evaluating model performance
model_resuls <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_resuls$net.result

# Because this is a numeric prediction problem rather than a classification problem,
# we cannot use a confusion matrix to examine model accuracy. Instead, we must
# measure the correlation between our predicted concrete strength and the true
# value. This provides an insight into the strength of the linear association between
# the two variables.
cor(predicted_strength, concrete_test$strength)

# Improving model performance
concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data=concrete_train, hidden=5)
#plot(concrete_model2)
model_results2 <- compute(concrete_model2, concrete_test[1:8])

# Evaluating model performance
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)
