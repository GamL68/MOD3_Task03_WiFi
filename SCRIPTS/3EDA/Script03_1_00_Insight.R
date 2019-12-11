# --- Find the Col index of LocID and ZoneID
# grep("LocID", colnames(wifi_df))
# grep("ZoneID", colnames(wifi_df))

# --- Identify levels of possible response vars
# This is a multi-class or a multinomial classification problem. 
# If the levels were 2 it would have been a binomial problem.

nlevels(factor(eda_qnt_df$BUILDINGID)) # 3 levels
nlevels(factor(eda_qnt_df$FLOORID)) # 5 levels
nlevels(factor(eda_qnt_df$ZoneID)) # 13 levels

nlevels(factor(eda_qnt_df$SPACEID)) # 123 levels
nlevels(factor(eda_qnt_df$POSITIONID)) # 2 levels
nlevels(factor(eda_qnt_df$LocID))  # 213 levels

nlevels(factor(eda_qnt_df$PHONEID)) # 16 levels
nlevels(factor(eda_qnt_df$USERID)) # 18 levels
nlevels(factor(eda_qnt_df$ItemID))  # 18 levels

nlevels(factor(eda_qnt_df$STRENGTH)) # 6 levels

#### Percentage of STRENGTH of valid accesses -----------------
### -> Target Vars ###
# Let's look at the number of instances (rows) that belong to each Zone class. 
# perc_BUILDINGID <- round(prop.table(table(eda_qnt_df$BUILDINGID)) * 100,2) # 25.59 47.23
# perc_FLOORID <- round(prop.table(table(eda_qnt_df$FLOORID)) * 100,2) # 3.77 26.12
# perc_ZoneID<- round(prop.table(table(eda_qnt_df$ZoneID)) * 100,2) # 3.77 14.03
# perc_SPACEID <- round(prop.table(table(eda_qnt_df$SPACEID)) * 100,2) # 0.02 2.51
# perc_POSITIONID <- round(prop.table(table(eda_qnt_df$POSITIONID)) * 100,2) # 17.15 82.85
# perc_LocID <- round(prop.table(table(eda_qnt_df$LocID)) * 100,2) # 0.02 1.85
# perc_PHONEID <- round(prop.table(table(eda_qnt_df$PHONEID)) * 100,2) # 0.99 25.01
# perc_USERID <- round(prop.table(table(eda_qnt_df$USERID)) * 100,2) # 0.99 23.40
# perc_ItemID <- round(prop.table(table(eda_qnt_df$ItemID)) * 100,2) # 0.99 23.40
# perc_STRENGTH <- round(prop.table(table(eda_qnt_df$STRENGTH)) * 100,2) # 0.41 35.49

# range(perc_BUILDINGID)
  
# --- Frequncy of records
# hist(cbind(freq=table(wifi_df$BUILDINGID), percentage=perc_BUILDINGID))
# hist(cbind(freq=table(wifi_df$FLOORID), percentage=perc_FLOORID))
# hist(cbind(freq=table(wifi_df$ZoneID), percentage=perc_ZoneID))
# hist(cbind(freq=table(wifi_df$SPACEID), percentage=perc_SPACEID))
# hist(cbind(freq=table(wifi_df$POSITIONID), percentage=perc_POSITIONID))
# hist(cbind(freq=table(wifi_df$LocID), percentage=perc_LocID))
# hist(cbind(freq=table(wifi_df$PHONEID), percentage=perc_PHONEID))
# hist(cbind(freq=table(wifi_df$USERID), percentage=perc_USERID))
# hist(cbind(freq=table(wifi_df$ItemID), percentage=perc_ItemID))
# hist(cbind(freq=table(wifi_df$STRENGTH), percentage=perc_STRENGTH))
# hist(cbind(freq=table(wifi_df$COORD_POINT), percentage=perc_COORD_POINT))

# --- See Plots available in Script02_Plots
# 1. Examine the data distribution of a quantative data variable

# The frequency of the data
# hist(eda_qnt_df$FLOORID)
# hist(eda_qnt_df$BUILDINGID)
# hist(eda_qnt_df$SPACEID)
# hist(eda_qnt_df$PHONEID)
# hist(eda_qnt_df$USERID)
# hist(eda_qnt_df$STRENGTH)
# hist(eda_qnt_df$ZoneID)
# hist(eda_qnt_df$LocID)
# hist(eda_qnt_df$ItemID)

# UserID 10 has the highest frequency, followed by USERID 1
# RelativePosition 2 is predominant
# Building 3 has the highest frequency
# FLOORID 4 and 2 are have more access points

# boxplot(eda_qnt_df$USERID, main="USERID \n Formalized Descriptive Statistics")
# boxplot(eda_qnt_df$FLOORID, main="FLOORID \n Formalized Descriptive Statistics")
# boxplot(eda_qnt_df$BUILDINGID, main="BUILDINGID \n Formalized Descriptive Statistics")
# boxplot(eda_qnt_df$SPACEID, main="SPACEID \n Formalized Descriptive Statistics")
# boxplot(eda_qnt_df$PHONEID, main="PHONEID \n Formalized Descriptive Statistics")
# boxplot(eda_qnt_df$POSITIONID, main="POSITIONID \n Formalized Descriptive Statistics")
# boxplot(eda_qnt_df$STRENGTH, main="STRENGTH \n Formalized Descriptive Statistics")
# boxplot(eda_qnt_df$ZoneID, main="ZoneID \n Formalized Descriptive Statistics")
# boxplot(eda_qnt_df$LocID, main="LocID \n Formalized Descriptive Statistics")
# boxplot(eda_qnt_df$ItemID, main="ItemID \n Formalized Descriptive Statistics")

# --- Explore the relationship between 2 quantative vars
# plot(eda_qnt_df$LONGITUDE,eda_qnt_df$LATITUDE, main="LONG vs LAT")
# plot(merge(base_df, eda_accessed_df, by = "ID") %>% select(FLOORID,Valid),main="VALID per FLOORID")
# plot(merge(eda_qnt_df, eda_accessed_df, by = "ID") %>% select(ZoneID,Valid),main="VALID per Zone")
# 
# plot_bld1_valid<-merge(eda_qnt_df, eda_accessed_df, by = "ID") %>%
#   select(BUILDINGID,ZoneID,LocID,Valid) %>%
#   filter(BUILDINGID==1) %>% 
#   group_by(ZoneID,LocID) %>% 
#   summarise(num=sum(Valid)) %>% 
#     ggplot(aes(LocID,num))+
#     geom_line()+
#   facet_grid(~ZoneID)
#         
# plot_bld2_valid<-merge(eda_qnt_df, eda_accessed_df, by = "ID") %>%
#   select(BUILDINGID,ZoneID,LocID,Valid) %>%
#   filter(BUILDINGID==2) %>% 
#   group_by(ZoneID,LocID) %>% 
#   summarise(num=sum(Valid)) %>% 
#   ggplot(aes(LocID,num))+
#   geom_line()+
#   facet_grid(~ZoneID)
# 
# plot_bld3_valid<-merge(eda_qnt_df, eda_accessed_df, by = "ID") %>%
#   select(BUILDINGID,ZoneID,LocID,Valid) %>%
#   filter(BUILDINGID==3) %>% 
#   group_by(ZoneID,LocID) %>% 
#   summarise(num=sum(Valid)) %>% 
#   ggplot(aes(LocID,num))+
#   geom_line()+
#   facet_grid(~ZoneID)
# 
# grid.arrange(plot_bld1_valid,plot_bld2_valid,plot_bld3_valid,
#              nrow = 3,
#              top = textGrob("Valid Accesses by Zone",gp=gpar(fontsize=20,font=3)))
# 
# 
# plot(eda_qnt_df$ZoneID,eda_qnt_df$LocID, main="ZONEID vs LOCID")
# plot(eda_qnt_df$LocID,eda_qnt_df$ItemID, main="LOCID vs ITEMID")
# plot(eda_qnt_df$PHONEID,eda_qnt_df$USERID, main="PHONEID vs USERID")
# plot(eda_qnt_df$SPACEID, eda_qnt_df$USERID, main="SPACEID vs USERID")
# plot(eda_qnt_df$FLOORID,eda_qnt_df$USERID, main="FLOORID vs USERID")

#### PLOTTING CATEGORICAL COUNT VARS ------------------------------------

# --- Count num of Accesses per ZoneId per level
#count_ZoneID<-table(eda_qlt_df$char_ZoneID)
# count_ZoneID<- table(sprintf("%02d", as.numeric(eda_qlt_df$ZoneID)))
# barplot(count_ZoneID, main="Counts Number of Accesses per Zone", 
#         xlab="Zone-ID", 
#         ylab="Totals", 
#         col="#C6DEFF")
# 
# count_BUILDINGID<- table(sprintf("%02d", as.numeric(eda_qlt_df$char_BUILDINGID)))
# barplot(count_BUILDINGID, main="Counts Number of Accesses per BUILDING", 
#         xlab="BUILDING-ID", 
#         ylab="Totals", 
#         col="#C6DEFF")
# 
# count_FLOORID<- table(sprintf("%02d", as.numeric(eda_qlt_df$char_FLOORID)))
# barplot(count_FLOORID, main="Counts Number of Accesses per FLOORID", 
#         xlab="FLOORID", 
#         ylab="Totals", 
#         col="#C6DEFF")
# 
# count_USERID<- table(sprintf("%02d", as.numeric(eda_qlt_df$char_USERID)))
# barplot(count_USERID, main="Counts Number of Accesses per USERID", 
#         xlab="USERID", 
#         ylab="Totals", 
#         col="#C6DEFF")
# 
# count_PHONEID<- table(sprintf("%02d", as.numeric(eda_qlt_df$char_PHONEID)))
# barplot(count_PHONEID, main="Counts Number of Accesses per PHONEID", 
#         xlab="PHONEID", 
#         ylab="Totals", 
#         col="#C6DEFF")
# 
# count_char_LocID<- table(sprintf("%03d", as.numeric(eda_qnt_df$LocID)))
# barplot(count_char_LocID, main="Counts of Accesses per LocID", 
#         xlab="Loc-ID", 
#         ylab="Totals", 
#         col="#C6DEFF")

# --- Caluclating Quantiles and Whiskers

# lower_quantile_MAX<-quantile(wifi_df$MAX,0.25) # -65
# upper_quantile_MAX<-quantile(wifi_df$MAX,0.75) # -51
# 
# lower_whisker_MAX <- max(min(wifi_df$MAX), lower_quantile_MAX - 1.5 * IQR(wifi_df$MAX)) # -86 -30
# boxplot(wifi_df$MAX)$stats[c(1, 5), ]

library(devtools)
# install_github("ujjwalkarn/xda")
library(xda)

# --- Summarize the quantative data
numSummary(eda_qnt_df)
numSummary(eda_wap_df)
numSummary(eda_accessed_df)
numSummary((wap_details_df))
# --- Summarize the qualitative data
charSummary(eda_qlt_df)

# --- Assign QUALITY_DESC within specific Area
bivariate(eda_qlt_df,"char_BUILDINGID","QUALITY")
bivariate(eda_qlt_df,"char_FLOORID","QUALITY")
bivariate(eda_qlt_df,"char_ZoneID","QUALITY")
bivariate(eda_qlt_df,"char_ItemID","QUALITY")

#### COUNT WAP VALID ACCESS POINTS --------------------------------------
# --- Create df
lWAP_col_wifi<- which(colnames(wifi_df)=="WAP519")

# Set NA where value is -110
na_df<-na_if(wifi_df, -110)

# Substitute in wifi_df
na_df<-na_df[ ,c(1:lWAP_col_wifi)]

# Transform into long form and filter removing NAs for valid
valid_WAP_df<-gather(na_df,key="WAPS",value="Num_Valid_Access",na.rm=TRUE)

# filter all NAs for invalid
nonValid_WAP_df<-gather(na_df,key="WAPS",value="Num_Valid_Access",na.rm=FALSE)
invalid_WAP_df<-nonValid_WAP_df[is.na(nonValid_WAP_df$Num_Valid_Access),]

# --- Count number of valid Accesses by WAP Point
# Same info found in wap_details_df
vw_df<-valid_WAP_df %>% 
  group_by(WAPS) %>% 
  summarise(valid_access=n())

# --- Transform into dataframe
vw_df<-as.data.frame(vw_df)

# Range of Valid access
range(vw_df$valid_access)
numSummary(vw_df)

# --- Visualize Valid WAPS
vw_df %>%
  arrange(desc(vw_df$valid_access)) %>% 
  top_n(10) %>% 
  ggplot(aes(x=WAPS, 
             y=valid_access))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1))+
  ggtitle("Top 10 Accesses WAPS")

hist(vw_df$valid_access, 
     main="Distribution of Valid Accesses",
     xlab="WAPs",
     ylab="Num Accesses")
# shows a distinct left skeweness

boxplot(vw_df$valid_access, 
        main="Statistical Distribution of Valid Accesses")

  
#### INVALID ACCESSES --------------------------------------------------
# Same info found in wap_details_df

# --- Count number of invalid Accesses by WAP Point
nvw_df <-invalid_WAP_df %>% 
  group_by(WAPS) %>% 
  summarise(invalid_access=n())

nvw_df<-as.data.frame(nvw_df)

# Range of Valid access
range(nvw_df$invalid_access)
numSummary(nvw_df)

# Plot
hist(nvw_df$invalid_access,
     main="Distribution of Non Valid Accesses",
     xlab="WAP",
     ylab="Num Accesses")
# shows a distinct right skeweness

#### MERGE Valid and non Valid DF -------------------------------------
# Same info found in wap_details_df
merge(vw_df,nvw_df)
access_validity_df<-merge(vw_df,nvw_df,by="WAPS",all=T)[,c("WAPS","valid_access","invalid_access")]

access_validity_df %>% 
  dplyr::select(WAPS,valid_access,invalid_access) %>% 
  group_by(WAPS) %>% 
  dplyr::summarise(Perc=round((valid_access/invalid_access)*100,2)) %>%
  filter(Perc>10) %>% 
  arrange(desc(Perc)) %>%
  ggplot(aes(WAPS,Perc))+
  geom_point(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Percentage over 30 of valid Access \nover respective invalid ones per Wap")

# SIGNAL QUALITY
which(colnames(eda_qnt_df)=="STRENGTH")
which(colnames(wifi_df)=="MAX")
which(colnames(wifi_df)=="LONGITUDE")
which(colnames(wifi_df)=="LATITUDE")
which(colnames(wifi_df)=="FLOORID")

# Exclude ID column from wap_df
temp<-cbind(eda_qnt_df[,11],wifi_df[,c(2:465)],wifi_df[,c(466,467,468,475)])

# Gather all WAP Accesses in long form
signal_WAP_df<-gather(temp,key=WAP,value=MAX,c(-STRENGTH,-LATITUDE,-LONGITUDE,-FLOORID),na.rm=TRUE)
# nrow = 9013100

# Remove the temporary dataframe
rm("temp")

#### CHOSING THE MIN MAX VALUES BASED ON MAX VALID ACCESSES ------------------------

# --- Filter only Valid Access Signals
valid_signal_WAP_df <-signal_WAP_df %>% 
  filter(MAX > -100)

# --- View the Values
valid_signal_WAP_df %>%
  dplyr::select(WAP, MAX) %>% 
  group_by(MAX) %>% 
  dplyr::summarise(num=n()) %>% 
  ggplot(aes(MAX, num))+
  geom_histogram(stat="identity")

# 363973 Total valid accesses

# --- BOXPLOT STATS
# boxplot.stats(valid_signal_WAP_df$MAX, coef = 1.5, do.conf = TRUE, do.out = TRUE)
# boxplot(valid_signal_WAP_df$MAX)

# --- Find upper and lower Whisker values
bp<-boxplot(valid_signal_WAP_df$MAX)$stats[c(1, 5), ] # -99 -43
# Access lower whisker
upw<-bp[1]
# Access upper whisker
lww<-bp[2]
# --- Total Number of valid string access above upper whisker
valid_signal_WAP_df %>%
  dplyr::select(WAP, MAX) %>% 
  group_by(MAX) %>%
  filter(between(MAX,lww,-1)) %>% 
  dplyr::summarise(num=n())%>% 
  ggplot(aes(MAX, num))+
  geom_bar(stat="identity")

# 6151 Count of valid access values that are above the upper whisker

# --- Plot adjusting the value of the lower limit
#From this we see that the consistent limit is around -34
obs_upw = -33
obs_lww = -95

valid_signal_WAP_df %>%
  dplyr::select(WAP, MAX) %>% 
  group_by(MAX) %>%
  filter(between(MAX,obs_upw,-1)) %>% 
  dplyr::summarise(num=n())

# 1167 Count of valid access values excluded

# --- Detect Q1 and Q3 values
quantile(valid_signal_WAP_df$MAX, c(0.25, .75)) 
#  25% 75% 
# -88  -70

# View how many valid accesses are within this range and adapt based on the graphs
# Final values are -95 and -33 range in which 354531 valid accesses 