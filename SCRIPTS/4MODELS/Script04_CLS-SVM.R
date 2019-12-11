#### SVM ###############################################

# Support vector machine takes data points and outputs the hyperplane. that best separates the tags. 
# This line is the decision boundary: anything that falls to one side of it we will classify as blue, 
# and anything that falls to the other as red.

# --- Process
# 1. Split data into training and test sets
      # This has been done in the general fCV function in Script03_1_ModelDataSet

# 2. Split the data into training (75%) and test set (25%)
      # This has been done in the general fCV function in Script03_1_ModelDataSet

#### BLD --------------------------------------------------------------
# Call the function
fCV(df_model)

# 3. Fit the model
fit.svmL_bld <- train(fact_BUILDINGID ~.,
                      data=modelData.bld, 
                      method="svmLinear",
                      trControl=fitControl,
                      preProcess = c("center")
)

# Accuracy and Kappa values
print(fit.svmL_bld)

# Model Accuracy
#  Accuracy   Kappa    
#  0.9995546  0.9993091

# 6. Make predictions
predictions.svmL_bld <-predict(fit.svmL_bld, newdata=testing_df, na.action = na.pass)
predictions.svmL_bld

# 8. Confusion Matrix to see how well the prediction went
confusionMatrix(predictions.svmL_bld,testing_df$fact_BUILDINGID)

# Accuracy : 1
# Kappa : 1

# Print the best tuning parameter cp that
# maximizes the model accuracy
fit.svmL_bld$bestTune # C => 1

# Plot final Tree Model
# par(xpd = NA) # Avoid clipping the text in some device
# plot(fit.svmP_bld$finalModel)
# text(fit.svmP_bld$finalModel,  digits = 3)

#### FLOOR --------------------------------------------------------------
fCV(df_model)

# 3. Fit the model
fit.svmP_bld <- train(fact_BUILDINGID ~.,
                       data=modelData.bld, 
                       method="svmPoly",
                       trControl=fitControl,
                       preProcess = c("scale","center")
)

# varImp(fit.svmP_bld)
# plot(varImp(fit.svmP_bld),top=5)
plot(fit.svmP_bld, grid = 50, slice = list())
plot(fit.svmP_bld)


# Accuracy and Kappa values
print(fit.svmP_bld)

# Model Accuracy
#   degree  scale   C     Accuracy     Kappa    
#     1       0.001  0.25  0.9931845  0.9894122

# 6. Make predictions
predictions.svmP_bld <-predict(fit.svmP_bld, newdata=testing_df, na.action = na.pass)
predictions.svmP_bld

# 8. Confusion Matrix to see how well the prediction went
confusionMatrix(predictions.svmP_bld,testing_df$fact_BUILDINGID)

# Accuracy : 1
# Kappa : 1

# Print the best tuning parameter cp that
# maximizes the model accuracy
fit.svmP_bld$bestTune 

# degree  scale    C
#   1     0.001   0.25

# Plot final Tree Model
par(xpd = NA) # Avoid clipping the text in some device
fit.svmP_bld$finalModel

# plot(fit.svmP_bld$finalModel)
# text(fit.svmP_bld$finalModel,  digits = 3)

#########################################################################
#########################################################################
# dev.off()