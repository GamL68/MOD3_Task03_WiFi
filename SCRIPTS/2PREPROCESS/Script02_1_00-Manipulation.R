########################################################################
# Once the ID is created we can build new dataframes containing any new
# field for exploration purposes
# I have created:

# a) Headers List
# b) eda_wap_df: contains only wap fields
# c) eda_accessed_df
# d) eda_qnt_df
# e) eda_qlt_df

# The ID field is present in all dataframes 
# All Manipulation will be recorded in the base_df.
# Final output df for EDA will be wifi_df.

########################################################################
# PLAN OF ATTACK:
# Create Specialized dfs
# Drop any unnecessary field
# Categorical data: create count tables to understand different categories.
# Failed access imputation (gather and spread, calculate Max, Min, Valid and Invalid accesses)
########################################################################

#### GENERATE ID
# Set ID to join the various tables to the main table.###############

# --- ID Field 

if("ID" %in% colnames(df)){
  df<-df
}else{
  df$ID <- seq.int(nrow(df))
  df$ID<- as.character(df$ID)
}
########################################################################
# --- Headers: Transposed list of column names
headers_lst<- list(colnames(df))
headers_df <- melt(headers_lst, id="ID")
headers_df<-headers_df[-c(2)]
headers_df %>% rename(WAP=value)

rm(headers_lst)

################################################################
# --- WAP: Complete Set of WAPS
eda_wap_df<-df %>% select("ID")

eda_wap_df<-cbind(eda_wap_df,df %>% select(starts_with("WAP")))

# Calculating Column Max Values
# Get rid of the ID column
wap_df.max<-eda_wap_df[,-1]

# Calculate the colMax
wap_col_max<-sapply(wap_df.max,max)

# Change Min value of -110 to 1 to calculate true min value
WAPS<-grep("WAP", names(eda_wap_df), value=T)
eda_wap_df[,WAPS] <- sapply(eda_wap_df[,WAPS],function(x) ifelse(x==-110,1,x))

# Calculating Column Max Values
# Get rid of the ID column
wap_df.min<-eda_wap_df[,-1]

# Calculate the colMin
wap_col_min<-sapply(wap_df.min, min)

# Transfor once again the min value to -110
eda_wap_df[,WAPS] <- sapply(eda_wap_df[,WAPS],function(x) ifelse(x==1,-110,x))

# Build matrix with row bind
wap_col_matrix<-rbind(wap_col_max,wap_col_min)

# Transpose the matrix
wap_trnsp_df<-as.data.frame(t(wap_col_matrix))
colnames(wap_trnsp_df) <- c("MAX","MIN")

# save Valid row names in Matrix
wap_row_names<-rownames(wap_trnsp_df)
wap_details_df<-as.data.frame(wap_row_names)
colnames(wap_details_df) <-c("WAP")

wap_details_df<-cbind(wap_details_df,wap_trnsp_df$MAX,wap_trnsp_df$MIN)
colnames(wap_details_df) <- c("WAP","MAX","MIN")

# Total Tries
wap_details_df$Tot_Tries<-apply(df[1:465], 2, function(x) sum(x<0))

# Total Valid
wap_details_df$Tot_Valid<-apply(df[1:465],2,function(x)sum(x != -110))

# Total Invalid
wap_details_df$Tot_Invalid<-apply(df[1:465],2,function(x)sum(x == -110))

# Total Within range -95
wap_details_df$Tot_Upper_Range<-apply(df[1:465],2,function(x)sum(x>-95))

colnames(wap_details_df) <-c("WAP","MAX", "MIN","TOT","VALID","INVALID","URANGE")

rm(wap_trnsp_df,wap_row_names,wap_col_matrix,wap_col_min,WAPS,wap_df.max,wap_df.min,wap_col_max)

# Test for infinite values
# A tibble of 0 x 2 is produced
wap_details_df %>% select(VALID) %>% group_by(VALID) %>% summarise(n=n()) %>% filter(is.infinite(VALID))

#################################################################
# --- Accesses: Contains info on accesses, MAX, MIN, VALID, INVALID, TOT TRIES ...
eda_accessed_df<-df %>% select(ID)
# Total WAP access tries by Row
eda_accessed_df$Accesses<-apply(df[1:465], 1, function(x) sum(x>-110))
# Total valid WAP access by Row
eda_accessed_df$Valid<-apply(df[1:465],1,function(x)sum(x!=-110))
# rowSums(eda_wap_df!=-110)

# Total invalid WAP access by Row
eda_accessed_df$Invalid<-apply(df[1:465],1,function(x)sum(x==-110))
# --- MAX and MIN Values
# MAX WAP
# 1 indicates manipulation done in rows
# 2 indicates manipulation done in columns
# c(1,2) indicated manipulation done in both rows and columns
eda_accessed_df$MAX<-apply(eda_wap_df[,-1], 1, FUN = function(x) {max(x[x])})
# MIN WAP
# Find minimum value per row excluding -110. These are the WAPS that have picked up a signal.
# The value of -95 was changed after insight activity

eda_accessed_df$MIN<-apply(eda_wap_df[,-1], 1, FUN = function(x) {min(x[x>-95])})
# Replace Infinite values
is.na(eda_accessed_df)<-sapply(eda_accessed_df, is.infinite)
eda_accessed_df[is.na(eda_accessed_df)]<- -110

# Take out any row that has both Max and Min values -110
# eda_accessed_df<-eda_accessed_df %>% filter(MAX!=-110) # 19227 accesses
eda_accessed_df<-eda_accessed_df %>% filter(MIN>=(-95)) # 19220 accesses
# eda_accessed_df2<-eda_accessed_df %>% filter(MIN>-94) # 14677 accesses
# The value to be kept is -95
# I would not set just yet an upper limit

# Add the Long and LAT columns
eda_accessed_df<-cbind(eda_accessed_df,df$LONGITUDE,df$LATITUDE)
names(eda_accessed_df)[names(eda_accessed_df) == "df$LONGITUDE"] <- "LONGITUDE"
names(eda_accessed_df)[names(eda_accessed_df) == "df$LATITUDE"] <- "LATITUDE"

num_accessed_df<-eda_accessed_df %>% select_if(is.numeric)

# Testing the correlation strength
cor.test(num_accessed_df$LONGITUDE,num_accessed_df$Valid)
# t = 33.711, df = 19218, p-value < 2.2e-16
# cor = 0.2362883 

cor.test(num_accessed_df$LATITUDE,num_accessed_df$Invalid)
# t = 51.118, df = 19218, p-value < 2.2e-16
# cor = 0.3459687 

# Create a new dataset with only the valid WAPS
base_df<-df %>%
  filter(ID %in% eda_accessed_df$ID)

#################################################################################
#### QUANTATIVE DF
# New reference df is base_df
# --- QNT: Contains all quantative vars
eda_qnt_df<-base_df %>% select(ID)
eda_qnt_df$BUILDINGID<-base_df$BUILDINGID
eda_qnt_df$FLOORID<-base_df$FLOORID
eda_qnt_df$POSITIONID<-base_df$POSITIONID
eda_qnt_df$SPACEID<-base_df$SPACEID
eda_qnt_df$USERID<-base_df$USERID
eda_qnt_df$PHONEID<-base_df$PHONEID
eda_qnt_df$LONGITUDE <-base_df$LONGITUDE
eda_qnt_df$LATITUDE <-base_df$LATITUDE

# Combine to form useful groups using group_indices() function of dplyr:
# ZoneId -> BuildingID, FLOORID
# LocID -> PositionID, SpaceID
# ItemID -> USERID, PHONEID

# ZoneID
eda_qnt_df$ZoneID<-as.numeric(base_df %>%
                  group_indices(BUILDINGID,FLOORID))

# LocID
eda_qnt_df$LocID<-as.numeric(base_df %>% 
                  group_indices(POSITIONID,SPACEID))
# ItemID
eda_qnt_df$ItemID<-as.numeric(base_df%>% 
                  group_indices(USERID,PHONEID))

# Signal Strength
# > -50dbm Excellent
# < -50 > -60 dBm Good
# < -60 > -70 dBm Fair
# < -70 dBm Weak
# < -90 dbM Poor
# >= -100 dBm No signal

# Strength Signal
# -30dBm is the maximum strength desirable. 
# Higher dBm are not real world readings???

eda_qnt_df$STRENGTH<-1

signal=eda_accessed_df$MAX
EoF<-nrow(eda_qnt_df)

eda_qnt_df$STRENGTH<- 1
for (i in 1:EoF) {
if (eda_accessed_df$MAX[i] <= -1 & eda_accessed_df$MAX[i] >=-50) {
eda_qnt_df$STRENGTH[i]<-10  # Excellent
} else if (eda_accessed_df$MAX[i] <= -51 & eda_accessed_df$MAX[i] >= -60) {
eda_qnt_df$STRENGTH[i]<-8   # Good
} else if (eda_accessed_df$MAX[i] <= -61 & eda_accessed_df$MAX[i] >= -70) {
eda_qnt_df$STRENGTH[i]<-6   # Fair
} else if (eda_accessed_df$MAX[i] <= -71 & eda_accessed_df$MAX[i] >= -90) {
eda_qnt_df$STRENGTH[i]<-5   # Poor
} else if (eda_accessed_df$MAX[i] <= -91 & eda_accessed_df$MAX[i] >= -104) {
eda_qnt_df$STRENGTH[i]<-3   # No signal
} else {eda_qnt_df$STRENGTH[i]<-1 # No access
}
}

rm(i, EoF,signal)

#################################################################################
#### QUANLITATIVE DF
# New reference df is base_df
# --- QLT: Contains all qualitative vars used for EDA

eda_qlt_df<-base_df %>% select(ID)
eda_qlt_df$char_BUILDINGID<-as.character(base_df$BUILDINGID)
eda_qlt_df$char_FLOORID<-as.character(base_df$FLOORID)
eda_qlt_df$char_POSITIONID<-as.character(base_df$POSITIONID)
eda_qlt_df$char_SPACEID<-as.character(base_df$SPACEID)
eda_qlt_df$char_USERID<-as.character(base_df$USERID)
eda_qlt_df$char_PHONEID<-as.character(base_df$PHONEID)

# Combine to form useful groups using group_indices() function of dplyr:
# ZoneId -> BuildingID, FLOORID
# LocID -> PositionID, SpaceID
# ItemID -> USERID, PHONEID

# ZoneID
eda_qlt_df$char_ZoneID<-as.character(base_df %>%
                                group_indices(BUILDINGID,FLOORID))

# LocID
eda_qlt_df$char_LocID<-as.character(base_df %>% 
                               group_indices(POSITIONID,SPACEID))
# ItemID
eda_qlt_df$char_ItemID<-as.character(base_df%>% 
                                group_indices(USERID,PHONEID))

# Signal Strength
# > -50dbm Excellent
# < -50 > -60 dBm Good
# < -60 > -70 dBm Fair
# < -70 dBm Weak
# < -90 dbM Poor
# >= -100 dBm No signal

# Strength Signal
# -30dBm is the maximum strength desirable. 
# Higher dBm are not real world readings???

eda_qlt_df$QUALITY<-1

signal=eda_accessed_df$MAX
EoF<-nrow(eda_qlt_df)

eda_qlt_df$QUALITY<- 1
for (i in 1:EoF) {
  if (eda_accessed_df$MAX[i] <= -1 & eda_accessed_df$MAX[i] >=-50) {
    eda_qlt_df$QUALITY[i]<-"Excellent"
  } else if (eda_accessed_df$MAX[i] <= -51 & eda_accessed_df$MAX[i] >= -60) {
    eda_qlt_df$QUALITY[i]<-"Good"
  } else if (eda_accessed_df$MAX[i] <= -61 & eda_accessed_df$MAX[i] >= -70) {
    eda_qlt_df$QUALITY[i]<-"Fair"
  } else if (eda_accessed_df$MAX[i] <= -71 & eda_accessed_df$MAX[i] >= -90) {
    eda_qlt_df$QUALITY[i]<-"Poor"
  } else if (eda_accessed_df$MAX[i] <= -91 & eda_accessed_df$MAX[i] >= -104) {
    eda_qlt_df$QUALITY[i]<-"No_signal"
  } else {eda_qlt_df$QUALITY[i]<-"No_access"
  }
}

rm(i, EoF,signal)

######################################################################
# Make a restricted df only with fields that are within the boundries of -95 -30

# --- Check if any column has only -95 or greater than -30
drop<-eda_wap_df[sapply(eda_wap_df, function(x) max(x) <= -95)] # 22 columns
drop1<-eda_wap_df[sapply(eda_wap_df, function(x) max(x) >= -30)] # 78 columns

# Creating a character vector named drop in which we are storing
# column names.
drop_names<-names(drop)
drop1_names<-names(drop1)

# Later we are telling R to select all the variables except the column names specified in the vector drop.
# The function names() returns all the column names and the '!' sign indicates negation.
res<-eda_wap_df[ ,!names(eda_wap_df) %in% drop_names]
final_wap_df<-res[ ,!names(res) %in% drop1_names]
tail(names(final_wap_df),27)
dim(final_wap_df)# 19220   366

rm(drop, drop1, drop_names, drop1_names)
