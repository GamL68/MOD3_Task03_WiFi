#### K- Nearest Neighbourgh #######################################



# --- Process
# 1. Split data into training and test sets
      # This has been done in the general fCV function in Script03_1_ModelDataSet

# 2. Split the data into training (75%) and test set (25%)
      # This has been done in the general fCV function in Script03_1_ModelDataSet

#### BLD --------------------------------------------------------------
# 3. Call the function
fCV(df_model)

# 4. Fit the model
fit.kN_Bld <- train(fact_BUILDINGID ~.,
                      data=modelData.bld, 
                      method="kknn",
                      trControl=fitControl,
                      preProcess = c("scale", "center")
)

# 5. View the model Results
varImp(fit.kN_Bld ) #
plot(varImp(fit.kN_Bld ),top=5)
plot(fit.kN_Bld)

# 6. Print Accuracy and Kappa values
print(fit.kN_Bld)

# Model Accuracy
# Kmax   Accuracy   Kappa    
#  9    0.9992583  0.9988491

# 7. Print the best tuning parameter cp that
# maximizes the model accuracy
fit.kN_Bld$bestTune 

# kmax distance  kernel
#  3      9     2 optimal

# 8. Plot final Tree Model
par(xpd = NA) # Avoid clipping the text in some device
plot(fit.kN_Bld$finalModel)
text(fit.kN_Bld$finalModel,  digits = 3)

# 9. Make predictions
predictions.kN_Bld <-predict(fit.kN_Bld, newdata=testing_df, na.action = na.pass)
predictions.kN_Bld

# 10. Confusion Matrix to see how well the prediction went
confusionMatrix(predictions.kN_Bld,testing_df$fact_BUILDINGID)

# 11. Print the Prediction Accuracy and Kappa Values
# Accuracy  Kappa 
#  1          1
   
# 12. Compute the prediction error RMSE
RMSE(predictions.kN_Bld, testing_df$fact_BUILDINGID)

#### FLOOR   --------------------------------------------------------------

#### SPACEID --------------------------------------------------------------
fCV(df_model)

# 3. Fit the model
fit.kn_spc <- train(fact_SPACEID ~.,
                       data=modelData.spc, 
                       method="kknn",
                       trControl=fitControl,
                       preProcess = c("center")
)

plot(fit.kn_spc)

#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)
plot(fit.kn_spc, print.thres = 0.5, type="S")

# Accuracy and Kappa values
print(fit.kn_spc)

# Model Accuracy
# kmax   Accuracy     Kappa     
#  9     0.7343167  0.7302867

# 6. Make predictions
predictions.kn_spc <-predict(fit.kn_spc, newdata=testing_df, na.action = na.pass)
predictions.kn_spc

# 8. Confusion Matrix to see how well the prediction went
confusionMatrix(predictions.kn_spc,testing_df$fact_SPACEID)


# Accuracy : 0.757
# Kappa : 0.753 

mean(predictions.kn_spc == testing_df$fact_SPACEID) # 0.7570093

# Print the best tuning parameter cp that
# maximizes the model accuracy
fit.kn_spc$bestTune 

# kmax distance  kernel
#   3     9     2 optimal

# Plot final Tree Model
par(xpd = NA) # Avoid clipping the text in some device
plot(fit.kn_spc$finalModel)

#########################################################################
#########################################################################
# dev.off()