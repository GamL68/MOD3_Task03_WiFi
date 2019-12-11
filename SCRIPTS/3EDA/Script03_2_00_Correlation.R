# Data needs to be normaly distributed to decide the corrolation

corr_bld1<-bld1_df %>%
  select_if(is.numeric)

# View relation
library(corrplot)
cov(corr_bld1$ZoneID,corr_bld1$LocID, use = "complete.obs") # -0.626
# corrplot()

# Testing the correlation strength
cor.test(corr_bld1$BUILDINGID,corr_bld1$LONGITUDE) # 0.95 indicates very strong correlation

# Reject the null hypothesis
# The p-value is below 0.05 therefore the null hypo is rejected, 
# the BUILDINGID is not normally distributed 

# Spearman Method - Used for computing on a non- normally distributed correlation
cor(corr_bld1$BUILDINGID,corr_bld1$LONGITUDE, method="spearman") # 0.92

# Kendall Method - Not widely used, prefferable to use spearman for non normal distribution 
cor(corr_bld1$BUILDINGID,corr_bld1$LONGITUDE, method="kendall") # 0.799

#### Select df
# temp<-cbind(wap_df,corr_bld1)

corr_df<- bld1_df %>% 
  select_if(is.numeric) %>% 
  filter(MAX_WAP!= -110)
  
# corr_df<-temp %>% 
#   select(starts_with("WAP"),LONGITUDE, LATITUDE, FLOOR, MAX_WAP, MIN_WAP, SIGNAL_QUALITY) %>%
#   filter(MAX_WAP!= -110)

# Remove temp df
rm("temp")

# Collinearity only applies to continuous numerical variables and not categorical vars.
corr_numeric_df<-corr_df[sapply(corr_df, is.numeric)]

# We remove vars who have a correlation greater than 0.7
high_corr<-findCorrelation(corr_numeric_df, cutoff = 0.7)
# Identify var Names of Highly correlated vars
colnames(corr_numeric_df)[high_corr]

