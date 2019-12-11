#### Set ID to join the various tables to the main table.###############

# --- ID Field 
df$ID <- seq.int(nrow(df))
df$ID<- as.character(df$ID)

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
# Check Data Types, missing values
# Drop any unnecessary field
# Categorical data: create count tables to understand different categories.
# Failed access imputation (gather and spread, calculate Max, min valid and invalid accesses)
########################################################################
# --- Headers: Transposed list of column names
headers_lst<- list(colnames(df))
headers_df <- melt(headers_lst, id="ID")
headers_df<-headers_df[-c(2)]
headers_df %>% rename(WAP=value)

rm(headers_lst)

################################################################
# --- WAP: Complete Set of WAPS
eda_wap_df<-df %>% select(ID)
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
wap_details_df$Tot_Tries<-apply(df[1:466], 2, function(x) sum(x<0))

# Total Valid
wap_details_df$Tot_Valid<-apply(df[1:466],2,function(x)sum(x != -110))

# Total Invalid
wap_details_df$Tot_Invalid<-apply(df[1:466],2,function(x)sum(x == -110))

# Total Within range -95
wap_details_df$Tot_Upper_Range<-apply(df[1:466],2,function(x)sum(x>-95))

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
eda_accessed_df$MAX_WAP<-apply(eda_wap_df[,-1], 1, FUN = function(x) {max(x[x])})
# MIN WAP
# Find minimum value per row excluding -110. These are the WAPS that have picked up a signal.
# The value of -95 was changed after insight activity



eda_accessed_df$MIN_WAP<-apply(eda_wap_df[,-1], 1, FUN = function(x) {min(x[x>-95])})
# Replace Infinite values
is.na(eda_accessed_df)<-sapply(eda_accessed_df, is.infinite)
eda_accessed_df[is.na(eda_accessed_df)]<- -110

# Take out any row that has both Max and Min values -110
# eda_accessed_df<-eda_accessed_df %>% filter(MAX_WAP!=-110) # 19227 accesses
eda_accessed_df<-eda_accessed_df %>% filter(MIN_WAP>=(-95)) # 19220 accesses
# eda_accessed_df2<-eda_accessed_df %>% filter(MIN_WAP>-94) # 14677 accesses
# The value to be kept is -95
# I would not set just yet an upper limit

# (MIN_WAP !=-110 & MAX_WAP!=-110)

# Create a new dataset with only the valid WAPS
base_df<-df %>%
  filter(ID %in% eda_accessed_df$ID)

#################################################################################

# --- QNT: Contains all quantative vars
eda_qnt_df<-base_df %>% select(ID)
eda_qnt_df$BUILDINGID<-base_df$BUILDINGID
eda_qnt_df$FLOOR<-base_df$FLOOR
eda_qnt_df$POSITIONID<-base_df$POSITIONID
eda_qnt_df$SPACEID<-base_df$SPACEID
eda_qnt_df$USERID<-base_df$USERID
eda_qnt_df$PHONEID<-base_df$PHONEID
eda_qnt_df$LONGITUDE <-base_df$LONGITUDE
eda_qnt_df$LATITUDE <-base_df$LATITUDE

# Combine to form useful groups using group_indices() function of dplyr:
# ZoneId -> BuildingID, FLOOR 
# LocID -> PositionID, SpaceID
# ItemID -> USERID, PHONEID

# ZoneID
eda_qnt_df$ZoneID<-as.numeric(base_df %>%
                  group_indices(BUILDINGID,FLOOR))

# LocID
eda_qnt_df$LocID<-as.numeric(base_df %>% 
                  group_indices(POSITIONID,SPACEID))
# ItemID
eda_qnt_df$ItemID<-as.numeric(base_df%>% 
                  group_indices(USERID,PHONEID))

# Signal Quality
# > -50dbm Excellent
# < -50 > -60 dBm Good
# < -60 > -70 dBm Fair
# < -70 dBm Weak
# < -90 dbM Poor
# >= -100 dBm No signal

# Quality Signal
eda_qnt_df$SIGNAL_QUALITY<-1

signal=eda_accessed_df$MAX_WA