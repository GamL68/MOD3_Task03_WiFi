library(rpart)

# --- FILTER df to give model base dataset
df_model<-df %>% 
  filter(FLOOR==1)

df_model$fact_BUILDINGID<-as.factor(df_model$char_BUILDINGID)
df_model$fact_FLOOR<-as.factor(df_model$char_FLOOR)
df_model$fact_SPACEID<-as.factor(df_model$char_SPACEID)
df_model$fact_POSITION<-as.factor(df_model$char_POSITION)
df_model$fact_ZoneID<-as.factor(df_model$ZoneID)

df_model<-df_model %>% 
          dplyr::select(starts_with("WAP"),
                        starts_with("fact_"),
                        LONGITUDE, LATITUDE,ZoneID,LocID,Tot_Bld_WAPS,
                        -(starts_with("char_")),
                        -ZoneID)
########################################################################
# --- CREATE FUNCTION to be called from all models
########################################################################
fCV <- function(df_model) {
  
  # Set Seed
  set.seed(998)
  
  # Split the data into a smaller subset
  df_split<-df_model[sample(1:nrow(df_model),3000,replace=FALSE),]
  
  # Split the data into training (75%) and test set (25%)
  inTrain <- caret::createDataPartition(df_split$fact_BUILDINGID, 
                                        p = .75, 
                                        list = FALSE)
  training_df <<- df_split[inTrain,]
  testing_df  <<- df_split[-inTrain,]
  
  # Create a 5 fold cross validation(Could also be 10 fold, but due to big data we prefer to use 5)
  fitControl <<- caret::trainControl(method = "repeatedcv", 
                                     number = 5, # Run algorithms using 5-fold cross validation
                                     repeats = 3,
                                     verbose = FALSE
  )
}

fCV(df_model)

nrow(training_df)
#########################################################################

### Model Data Variables
# --- BLD
modelData.bld<-training_df %>% 
  dplyr::select(starts_with("WAP"), 
                fact_BUILDINGID)

# --- SPACEID
modelData.spc<-training_df %>% 
  dplyr::select(starts_with("WAP"), 
                fact_SPACEID)
