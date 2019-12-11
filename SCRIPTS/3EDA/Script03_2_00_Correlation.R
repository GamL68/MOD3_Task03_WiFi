#################################################################################

# PLAN OF ATTACK:
# Test Correlation between eda_qnt_df
# Test Correlation between eda_accessed_df
# Test Multicollinearity between final_wap_df

#################################################################################
#### CORRELATION of QUANTATIVE data
# Data needs to be normaly distributed to decide the corrolation

num_qnt_df<-eda_qnt_df %>% select_if(is.numeric)
corrplot(cor(num_qnt_df))

# View covariance
cov(num_qnt_df$LATITUDE,num_qnt_df$STRENGTH, use = "complete.obs") # 8.007241

# Testing the correlation strength
set.seed(10)
cor.test(num_qnt_df$LONGITUDE,num_qnt_df$STRENGTH)
# t = -16.56, df = 19218, p-value < 2.2e-16
# cor = -0.1186145 

set.seed(10)
cor.test(num_qnt_df$LATITUDE,num_qnt_df$STRENGTH)
# t = 9.0671, df = 19218, p-value < 2.2e-16
# 0.06526583 

# Reject the null hypothesis
# The p-value is below 0.05 therefore the null hypo is rejected, 
# the BUILDINGID is not normally distributed 

# Returns a vector of integers corresponding to columns to remove 
# to reduce pair-wise correlations.
high_corr<-findCorrelation(cor(num_qnt_df), cutoff = 0.7, verbose = FALSE)
# 1 6 8 4 9

high_corr_colnames<-colnames(num_qnt_df)[high_corr]

# df to use for training, testing and Moddelling
uncorr_qnt_df = subset(num_qnt_df, select = -c(1,4,6,8,9))
model_qnt_df<-num_qnt_df[,!(names(num_qnt_df) %in% high_corr_colnames)]

rm(high_corr,high_corr_colnames)

#################################################################################
#### CORRELATION of QUANTATIVE data
# Data needs to be normaly distributed to decide the corrolation

num_accessed_df<-eda_accessed_df %>% select_if(is.numeric)
corrplot(cor(num_accessed_df))

# View covariance
cov(num_accessed_df$LATITUDE,num_accessed_df$MAX, use = "complete.obs") # -24.7

# Testing the correlation strength
set.seed(10)
cor.test(num_accessed_df$LONGITUDE,num_accessed_df$MAX)
# t = -4, df = 19218, p-value = 2e-05
# cor = -0.0304

set.seed(10)
cor.test(num_accessed_df$LATITUDE,num_accessed_df$MAX)
# t = -4, df = 19218, p-value = 9e-05
# -0.0283

# Reject the null hypothesis
# The p-value is below 0.05 therefore the null hypo is rejected, 
# the BUILDINGID is not normally distributed 

# Returns a vector of integers corresponding to columns to remove 
# to reduce pair-wise correlations.
high_corr<-findCorrelation(cor(num_accessed_df), cutoff = 0.7, verbose = FALSE)
# 1 2 7

high_corr_colnames<-colnames(num_accessed_df)[high_corr]

# df to use for training, testing and Moddelling
uncorr_accessed_df = subset(num_accessed_df, select = -c(1,2,7))
model_accessed_df<-num_accessed_df[,!(names(num_accessed_df) %in% high_corr_colnames)]

rm(high_corr,high_corr_colnames)
