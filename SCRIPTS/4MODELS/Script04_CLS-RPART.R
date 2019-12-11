#### Classification and Regression Trees (CART) #######################################
# The prediction error is measured by the RMSE, 
# which corresponds to the average difference between the observed known values 
# of the outcome and the predicted value by the model. 
# RMSE is computed as RMSE = mean((observeds - predicteds)^2) %>% sqrt(). 
# The lower the RMSE, the better the model.

# Choose the best cp value:

# The p-value indicates the association between a given predictor variable and the outcome variable. 
# For example, the first decision node at the top shows the variable that is most strongly associated with the response variable, 
# and thus is selected as the first node.

library(rpart)

# --- Process
# 1. Split data into training and test sets
      # This has been done in the general fCV function in Script03_1_ModelDataSet

# 2. Split the data into training (75%) and test set (25%)
      # This has been done in the general fCV function in Script03_1_ModelDataSet

#### BLD --------------------------------------------------------------
# 3. Call the function
fCV(df_model)

# 4. Fit the model
fit.rpart_Bld <- train(fact_BUILDINGID ~.,
                      data=modelData.bld, 
                      method="rpart",
                      trControl=fitControl,
                      preProcess = c("scale", "center")
)

# 5. View the model Results
varImp(fit.rpart_Bld) #
plot(varImp(fit.rpart_Bld),top=5)
plot(fit.rpart_Bld)

# 6. Print Accuracy and Kappa values
print(fit.rpart_Bld)

# Model Accuracy
# cp         Accuracy   Kappa    
# 0.1932094  0.8282569  0.7381460

# 7. Print the best tuning parameter cp that
# maximizes the model accuracy
fit.rpart_Bld$bestTune # 0.1932094

# 8. Plot final Tree Model
par(xpd = NA) # Avoid clipping the text in some device
plot(fit.rpart_Bld$finalModel)
text(fit.rpart_Bld$finalModel,  digits = 3)

# 9. Make predictions
predictions.rpart_Bld <-predict(fit.rpart_Bld, newdata=testing_df, na.action = na.pass)
predictions.rpart_Bld

# 10. Confusion Matrix to see how well the prediction went
confusionMatrix(predictions.rpart_Bld,testing_df$fact_BUILDINGID)

# 11. Print the Prediction Accuracy and Kappa Values
# Accuracy  Kappa 
#  0.7757   0.6559
   
# 12. Compute the prediction error RMSE
RMSE(predictions.rpart_Bld, testing_df$fact_BUILDINGID)

#### FLOOR   --------------------------------------------------------------

#### SPACEID --------------------------------------------------------------
fCV(df_model)

# 3. Fit the model
fit.rpart_spc <- train(fact_SPACEID ~.,
                       data=modelData.spc, 
                       method="rpart",
                       trControl=fitControl,
                       preProcess = c("center")
)

varImp(fit.rpart_spc) #
plot(varImp(fit.rpart_spc),top=5)
plot(fit.rpart_spc)

# Accuracy and Kappa values
print(fit.rpart_spc)

# Model Accuracy
# cp         Accuracy   Kappa    
# 0.01657459  0.09198044  0.06912538

# 6. Make predictions
predictions.rpart_spc <-predict(fit.rpart_spc, newdata=testing_df, na.action = na.pass)
predictions.rpart_spc

# 8. Confusion Matrix to see how well the prediction went
confusionMatrix(predictions.rpart_spc,testing_df$fact_SPACEID)


# Accuracy : 0.0841  
# Kappa : 0.0579   

# Print the best tuning parameter cp that
# maximizes the model accuracy
fit.rpart_spc$bestTune # 0.01657459

# Plot final Tree Model
par(xpd = NA) # Avoid clipping the text in some device
plot(fit.rpart_spc$finalModel)
text(fit.rpart_spc$finalModel,  digits = 3)

#########################################################################
#########################################################################
# dev.off()