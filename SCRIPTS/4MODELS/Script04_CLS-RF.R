#### RANDOM FOREST ######################################################


# --- Process
# 1. Split data into training and test sets
# This has been done in the general fCV function in Script03_1_ModelDataSet

# 2. Split the data into training (75%) and test set (25%)
# This has been done in the general fCV function in Script03_1_ModelDataSet

#### BLD --------------------------------------------------------------
# Call the function
fCV(df_model)

# 3. Fit the model
fit.rf_bld <- train(fact_BUILDINGID ~.,
                      data=modelData.bld, 
                      method="rf",
                      trControl=fitControl,
                      preProcess = c("center")
)

# Accuracy and Kappa values
print(fit.rf_bld)

# Model Accuracy
#  Accuracy   Kappa    
#  0.9995546  0.9993091

# 6. Make predictions
predictions.rf_bld <-predict(fit.rf_bld, newdata=testing_df, na.action = na.pass)
predictions.rf_bld

# 8. Confusion Matrix to see how well the prediction went
confusionMatrix(predictions.rf_bld,testing_df$fact_BUILDINGID)

# Accuracy : 1
# Kappa : 1

# Print the best tuning parameter cp that
# maximizes the model accuracy
fit.rf_bld$bestTune # C => 1

# Plot final Tree Model
par(xpd = NA) # Avoid clipping the text in some device
plot(fit.rf_bld$finalModel)
text(fit.rf_bld$finalModel,  digits = 3)

#### FLOOR --------------------------------------------------------------
fCV(df_model)

# 3. Fit the model
fit.bst_bld <- train(fact_BUILDINGID ~.,
                      data=modelData.bld, 
                      method="bstTree",
                      trControl=fitControl,
                      preProcess = c("scale","center")
)

varImp(fit.bst_bld)
plot(varImp(fit.bst_bld),top=5)
plot(fit.bst_bld, grid = 50, slice = list())
plot(fit.bst_bld)


# Accuracy and Kappa values
print(fit.rf_bld)

# Model Accuracy


# 6. Make predictions
predictions.bst_bld <-predict(fit.bst_bld, newdata=testing_df, na.action = na.pass)
predictions.bst_bld

# 8. Confusion Matrix to see how well the prediction went
confusionMatrix(predictions.bst_bld,testing_df$fact_BUILDINGID)

# Accuracy : 
# Kappa : 

# Print the best tuning parameter cp that
# maximizes the model accuracy
fit.bst_bld$bestTune 



# Plot final Tree Model
par(xpd = NA) # Avoid clipping the text in some device
fit.bst_bld$finalModel

plot(fit.bst_bld$finalModel)
text(fit.bst_bld$finalModel,  digits = 3)

#########################################################################
#########################################################################
# dev.off()