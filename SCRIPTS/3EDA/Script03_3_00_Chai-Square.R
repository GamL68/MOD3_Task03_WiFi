# Test indipendance used to determine if there is a significant relationship 
# between 2 nominal vars.

#library(MASS)

# My NULL hypothesis is that the Signal_quality and the selected vars
# are independant

chai_ZONE<-table(df$SIGNAL_QUALITY,df$ZoneID)
chisq.test(chai_ZONE) 
# X-squared = 3000, df = 60, p-value <2e-16

chai_BLD<-table(as.factor(df$SIGNAL_QUALITY),df$char_BUILDINGID)
chisq.test(chai_BLD) 
# X-squared = 2000, df = 10, p-value <2e-16

chai_FLR<-table(as.factor(df$SIGNAL_QUALITY),df$char_FLOOR)
chisq.test(chai_FLR) 
# X-squared = 500, df = 20, p-value <2e-16

chai_COORD<-table(as.factor(df$SIGNAL_QUALITY),df$COORD_POINT)
chisq.test(chai_COORD) 
# X-squared = 40000, df = 5000, p-value <2e-16

# the p-value is below 0.05 therefore we reject the NULL Hypothesis, 
# the vars are not independent

#### Test the strength of the 2 nominal vars association

# Cramer's coeff

assocstats(chai_ZONE) # Cramer's V: 0.19 
assocstats(chai_BLD) # Cramer's V: 0.2 
assocstats(chai_FLR) # Cramer's V: 0.082 
assocstats(chai_COORD) # Cramer's V: 0.671 


