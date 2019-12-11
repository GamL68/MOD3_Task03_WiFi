1575646367152:signal=final_wap_df$MAX_WAP
1575646367152:EoF<-nrow(wifi_df)
1575646367152:final_wap_df$SIGNAL_QUALITY<- 1
1575646367153:for (i in  1:EoF) {
  1575646367153:if (signal[i] <= -1 & signal[i] >= -50) {
    1575646367153:final_wap_df$SIGNAL_QUALITY[i]<-10  # Excellent
    1575646367154:} else if (signal[i] <= -51 & signal[i] >= -60) {
      1575646367154:final_wap_df$SIGNAL_QUALITY[i]<-8   # Good
      1575646367155:} else if (signal[i] <= -61 & signal[i] >= -70) {
        1575646367155:final_wap_df$SIGNAL_QUALITY[i]<-6   # Fair
        1575646367155:} else if (signal[i] <= -71 & signal[i] >= -90) {
          1575646367156:final_wap_df$SIGNAL_QUALITY[i]<-5   # Poor
          1575646367156:} else if (signal[i] <= -91 & signal[i] >= -104) {
            1575646367156:final_wap_df$SIGNAL_QUALITY[i]<-3   # No signal
            1575646367157:} else {
              1575646367157:final_wap_df$SIGNAL_QUALITY[i] <- 1 # No access
              1575646367157:}
  1575646367158:}
1575646367180:final_wap_df$QUALITY_DESC<- "Failed_Access"
1575646444354:for (i in 1:EoF) {
  1575646444355:if (signal[i] <=-1 & signal[i] >=-50) {
    1575646444355:final_wap_df$SIGNAL_QUALITY[i]<-10  # Excellent
    1575646444356:} else if (signal[i] <= -51 & signal[i] >= -60) {
      1575646444356:final_wap_df$SIGNAL_QUALITY[i]<-8   # Good
      1575646444357:} else if (signal[i] <= -61 & signal[i] >= -70) {
        1575646444357:final_wap_df$SIGNAL_QUALITY[i]<-6   # Fair
        1575646444358:} else if (signal[i] <= -71 & signal[i] >= -90) {
          1575646444358:final_wap_df$SIGNAL_QUALITY[i]<-5   # Poor
          1575646444358:} else if (signal[i] <= -91 & signal[i] >= -104) {
            1575646444359:final_wap_df$SIGNAL_QUALITY[i]<-3   # No signal
            1575646444359:} else {
              1575646444360:final_wap_df$SIGNAL_QUALITY[i]<-1 # No access
              1575646444360:}
  1575646444360:}
1575646450304:final_wap_df$SIGNAL_QUALITY<- 1
1575646450804:for (i in 1:EoF) {
  1575646450805:if (signal[i] <=-1 & signal[i] >=-50) {
    1575646450805:final_wap_df$SIGNAL_QUALITY[i]<-10  # Excellent
    1575646450806:} else if (signal[i] <= -51 & signal[i] >= -60) {
      1575646450806:final_wap_df$SIGNAL_QUALITY[i]<-8   # Good
      1575646450807:} else if (signal[i] <= -61 & signal[i] >= -70) {
        1575646450808:final_wap_df$SIGNAL_QUALITY[i]<-6   # Fair
        1575646450808:} else if (signal[i] <= -71 & signal[i] >= -90) {
          1575646450809:final_wap_df$SIGNAL_QUALITY[i]<-5   # Poor
          1575646450810:} else if (signal[i] <= -91 & signal[i] >= -104) {
            1575646450811:final_wap_df$SIGNAL_QUALITY[i]<-3   # No signal
            1575646450811:} else {
              1575646450812:final_wap_df$SIGNAL_QUALITY[i]<-1 # No access
              1575646450813:}
  1575646450813:}
1575646471161:signal
1575646481587:# Recreate the columns MAX_WAP and SIGNAL_QUALITY with new data WAP range
  1575646481587:final_wap_df$MAX_WAP<-apply(final_wap_df, 1, FUN = function(x) {max(x[x])})
1575646487519:final_wap_df
1575646564585:final_wap_df$MAX_WAP[i]
1575646611106:EoF<-nrow(final_wap_df)
1575646615638:EoF
1575646664420:# New Signal_Quality
  1575646664420:signal=apply(final_wap_df$MAX_WAP)
1575646712423:final_wap_df$MAX_WAP[1]
1575646720528:final_wap_df$MAX_WAP
1575646732281:final_wap_df$MAX_WAP<-apply(final_wap_df, 1, FUN = function(x) {max(x[x])})
1575646737903:final_wap_df$MAX_WAP
1575646769296:final_wap_df<-res[ ,!names(res) %in% drop1]
1575646776202:# --- Check if any column has only -95
  1575646776202:drop<-wap_df[sapply(wap_df, function(x) max(x) <= -96)] # 22 columns
1575646776825:drop1<-wap_df[sapply(wap_df, function(x) max(x) >= -32)] # 78 columns
1575646777450:# Creating a character vector named drop in which we are storing
  1575646777450:# column names.
  1575646777451:drop<-names(drop)
1575646778008:drop1<-names(drop1)
1575646778522:# Later we are telling R to select all the variables except the column names specified in the vector drop.
  1575646778522:# The function names() returns all the column names and the '!' sign indicates negation.
  1575646778523:res<-wap_df[ ,!names(wap_df) %in% drop]
1575646779129:final_wap_df<-res[ ,!names(res) %in% drop1]
1575646784903:final_wap_df
1575646844351:tail(names(final_wap_df),27)
1575646887332:# Remove temporary dataframes
  1575646887332:rm(drop,drop1,res)
1575646898783:tail(final_wap_df)
1575646905329:dup_col <- duplicated(sapply(final_wap_df, digest))
1575646906241:dup_col_df<- final_wap_df[,which(!dup_col)]
1575646910888:dup_col_df
1575646925296:dup_col
1575646975035:# Recreate the columns MAX_WAP and SIGNAL_QUALITY with new data WAP range
  1575646975036:final_wap_df$MAX_WAP<-apply(final_wap_df, 1, FUN = function(x) {max(x[x])})
1575646979201:final_wap_df$MAX_WAP
1575646982792:final_wap_df$MIN_WAP<-apply(wap_df[,-1], 1, FUN = function(x) {min(x[x<-104])})
1575646987634:final_wap_df$MIN_WAP
1575647162963:for (i in 1:EoF) {
  1575647162963:if (final_wap_df$MAX_WAP[i] <= -1 & final_wap_df$MAX_WAP[i] >=-50) {
    1575647162964:final_wap_df$SIGNAL_QUALITY[i]<-10  # Excellent
    1575647162964:} else if (final_wap_df$MAX_WAP[i] <= -51 & final_wap_df$MAX_WAP[i] >= -60) {
      1575647162964:final_wap_df$SIGNAL_QUALITY[i]<-8   # Good
      1575647162965:} else if (final_wap_df$MAX_WAP[i] <= -61 & final_wap_df$MAX_WAP[i] >= -70) {
        1575647162965:final_wap_df$SIGNAL_QUALITY[i]<-6   # Fair
        1575647162965:} else if (final_wap_df$MAX_WAP[i] <= -71 & final_wap_df$MAX_WAP[i] >= -90) {
          1575647162966:final_wap_df$SIGNAL_QUALITY[i]<-5   # Poor
          1575647162966:} else if (final_wap_df$MAX_WAP[i] <= -91 & final_wap_df$MAX_WAP[i] >= -104) {
            1575647162967:final_wap_df$SIGNAL_QUALITY[i]<-3   # No signal
            1575647162967:} else {
              1575647162968:final_wap_df$SIGNAL_QUALITY[i]<-1 # No access
              1575647162968:}
  1575647162968:}
1575647172247:warnings()
1575647216439:final_wap_df
1575647287281:for (i in 1:EoF) {
  1575647287282:if (final_wap_df$MAX_WAP[i] <= -1 & final_wap_df$MAX_WAP[i] >=-50) {
    1575647287282:final_wap_df$SIGNAL_QUALITY[i]<-10  # Excellent
    1575647287283:} else if (final_wap_df$MAX_WAP[i] <= -51 & final_wap_df$MAX_WAP[i] >= -60) {
      1575647287283:final_wap_df$SIGNAL_QUALITY[i]<-8   # Good
      1575647287283:} else if (final_wap_df$MAX_WAP[i] <= -61 & final_wap_df$MAX_WAP[i] >= -70) {
        1575647287284:final_wap_df$SIGNAL_QUALITY[i]<-6   # Fair
        1575647287284:} else if (final_wap_df$MAX_WAP[i] <= -71 & final_wap_df$MAX_WAP[i] >= -90) {
          1575647287285:final_wap_df$SIGNAL_QUALITY[i]<-5   # Poor
          1575647287285:} else if (final_wap_df$MAX_WAP[i] <= -91 & final_wap_df$MAX_WAP[i] >= -104) {
            1575647287286:final_wap_df$SIGNAL_QUALITY[i]<-3   # No signal
            1575647287286:} else {
              1575647287287:final_wap_df$SIGNAL_QUALITY[i]<-1 # No access
              1575647287287:}
  1575647287287:}
1575647401745:# Creating a character vector named drop in which we are storing
  1575647401745:# column names.
  1575647401746:drop<-names(drop)
1575647402415:drop1<-names(drop1)
1575647403091:# Later we are telling R to select all the variables except the column names specified in the vector drop.
  1575647403091:# The function names() returns all the column names and the '!' sign indicates negation.
  1575647403092:res<-wap_df[ ,!names(wap_df) %in% drop]
1575647404256:final_wap_df<-res[ ,!names(res) %in% drop1]
1575647405900:# Add dummy columns
  1575647405901:final_wap_df$SIGNAL_QUALITY<-1
1575647406745:final_wap_df$QUALITY_DESC<- "Failed_Access"
1575647408849:# Remove temporary dataframes
  1575647408850:rm(drop,drop1,res)
1575647410337:# A faster way is to use the digest library, transposing is expensive
  1575647410338:library(digest)
1575647410895:dup_col <- duplicated(sapply(final_wap_df, digest))
1575647411280:dup_col_df<- final_wap_df[,which(!dup_col)]
1575647414339:# Recreate the columns MAX_WAP and SIGNAL_QUALITY with new data WAP range
  1575647414340:final_wap_df$MAX_WAP<-apply(final_wap_df, 1, FUN = function(x) {max(x[x])})
1575647422568:final_wap_df$MIN_WAP<-apply(wap_df[,-1], 1, FUN = function(x) {min(x[x<-104])})
1575647432664:# New Signal_Quality
  1575647432664:signal=final_wap_df$MAX_WAP
1575647435967:EoF<-nrow(final_wap_df)
1575647436464:final_wap_df$SIGNAL_QUALITY<- 1
1575647437339:for (i in 1:EoF) {
  1575647437339:if (final_wap_df$MAX_WAP[i] <= -1 & final_wap_df$MAX_WAP[i] >=-50) {
    1575647437340:final_wap_df$SIGNAL_QUALITY[i]<-10  # Excellent
    1575647437341:} else if (final_wap_df$MAX_WAP[i] <= -51 & final_wap_df$MAX_WAP[i] >= -60) {
      1575647437342:final_wap_df$SIGNAL_QUALITY[i]<-8   # Good
      1575647437343:} else if (final_wap_df$MAX_WAP[i] <= -61 & final_wap_df$MAX_WAP[i] >= -70) {
        1575647437343:final_wap_df$SIGNAL_QUALITY[i]<-6   # Fair
        1575647437344:} else if (final_wap_df$MAX_WAP[i] <= -71 & final_wap_df$MAX_WAP[i] >= -90) {
          1575647437345:final_wap_df$SIGNAL_QUALITY[i]<-5   # Poor
          1575647437346:} else if (final_wap_df$MAX_WAP[i] <= -91 & final_wap_df$MAX_WAP[i] >= -104) {
            1575647437346:final_wap_df$SIGNAL_QUALITY[i]<-3   # No signal
            1575647437347:} else {
              1575647437347:final_wap_df$SIGNAL_QUALITY[i]<-1 # No access
              1575647437348:}
  1575647437348:}
1575647468361:for(i in 1:EoF) {
  1575647468362:if (final_wap_df$MAX_WAP[i] <= -1 & final_wap_df$MAX_WAP[i] >=-50) {
    1575647468362:final_wap_df$SIGNAL_QUALITY[i]<-10  # Excellent
    1575647468362:} else if (final_wap_df$MAX_WAP[i] <= -51 & final_wap_df$MAX_WAP[i] >= -60) {
      1575647468363:final_wap_df$SIGNAL_QUALITY[i]<-8   # Good
      1575647468363:} else if (final_wap_df$MAX_WAP[i] <= -61 & final_wap_df$MAX_WAP[i] >= -70) {
        1575647468363:final_wap_df$SIGNAL_QUALITY[i]<-6   # Fair
        1575647468364:} else if (final_wap_df$MAX_WAP[i] <= -71 & final_wap_df$MAX_WAP[i] >= -90) {
          1575647468364:final_wap_df$SIGNAL_QUALITY[i]<-5   # Poor
          1575647468365:} else if (final_wap_df$MAX_WAP[i] <= -91 & final_wap_df$MAX_WAP[i] >= -104) {
            1575647468365:final_wap_df$SIGNAL_QUALITY[i]<-3   # No signal
            1575647468365:} else {
              1575647468366:final_wap_df$SIGNAL_QUALITY[i]<-1 # No access
              1575647468366:}
  1575647468367:}
1575647486040:final_wap_df$MAX_WAP[i]
1575647492639:final_wap_df$MAX_WAP
1575647692662:which(colnames(final_wap_df)=="MAX_WAP")
1575647744153:for(i in 1:EoF) {
  1575647744154:if (final_wap_df[i,469] <= -1 & final_wap_df[i,469] >=-50) {
    1575647744154:final_wap_df$SIGNAL_QUALITY[i]<-10  # Excellent
    1575647744155:} else if (final_wap_df$MAX_WAP[i] <= -51 & final_wap_df$MAX_WAP[i] >= -60) {
      1575647744155:final_wap_df$SIGNAL_QUALITY[i]<-8   # Good
      1575647744155:} else if (final_wap_df$MAX_WAP[i] <= -61 & final_wap_df$MAX_WAP[i] >= -70) {
        1575647744156:final_wap_df$SIGNAL_QUALITY[i]<-6   # Fair
        1575647744156:} else if (final_wap_df$MAX_WAP[i] <= -71 & final_wap_df$MAX_WAP[i] >= -90) {
          1575647744157:final_wap_df$SIGNAL_QUALITY[i]<-5   # Poor
          1575647744157:} else if (final_wap_df$MAX_WAP[i] <= -91 & final_wap_df$MAX_WAP[i] >= -104) {
            1575647744157:final_wap_df$SIGNAL_QUALITY[i]<-3   # No signal
            1575647744158:} else {
              1575647744158:final_wap_df$SIGNAL_QUALITY[i]<-1 # No access
              1575647744159:}
  1575647744159:}
1575648030470:c<-which(colnames(final_wap_df)=="MAX_WAP")
1575648160160:s<-which(colnames(final_wap_df)=="SIGNAL_QUALITY")
1575648164566:s
1575648214295:m <- "intialize"
1575648215313:for(i in 1:EoF) {
  1575648215314:if (final_wap_df[i,c] <= -1 & final_wap_df[i,c] >=-50) {
    1575648215315:final_wap_df[i,s]<-10  # Excellent
    1575648215315:} else if (final_wap_df[i,c] <= -51 & final_wap_df[i,c] >= -60) {
      1575648215316:final_wap_df[i,s]<-8   # Good
      1575648215316:} else if (final_wap_df[i,c] <= -61 & final_wap_df[i,c] >= -70) {
        1575648215317:final_wap_df[i,s]<-6   # Fair
        1575648215318:} else if (final_wap_df[i,c] <= -71 & final_wap_df[i,c] >= -90) {
          1575648215318:final_wap_df[i,s]<-5   # Poor
          1575648215319:} else if (final_wap_df[i,c] <= -91 & final_wap_df[i,c] >= -104) {
            1575648215319:final_wap_df[i,s]<-3   # No signal
            1575648215320:} else {
              1575648215321:final_wap_df[i,s]<-1 # No access
              1575648215321:}
  1575648215322:}
1575648216479:m
1575648511633:apply(ifelse(x == 1, x + 1,
                           1575648511633:ifelse( x == 2,  x,
                                                 1575648511634:ifelse( x == 2,  x,
                                                                       1575648511634:ifelse( x == 2,  x,
                                                                                             1575648511635:ifelse( x == 2,  x,
                                                                                                                   1575648511635:ifelse( x == 2,  x,
                                                                                                                                         1575648511635:final_wap_df[i,s]<-1))))))
                    1575648511636:)
1575648754869:apply(ifelse(final_wap_df[,c] <= -1 & final_wap_df[,c] >=-50, final_wap_df[,s]<-10,
                           1575648754870:ifelse(final_wap_df[,c] <= -51 & final_wap_df[,c] >= -60,final_wap_df[,s]<-8,
                                                1575648754870:ifelse(final_wap_df[,c] <= -61 & final_wap_df[,c] >= -70,final_wap_df[,s]<-6,
                                                                     1575648754871:ifelse(final_wap_df[,c] <= -71 & final_wap_df[,c] >= -90,final_wap_df[,s]<-5,
                                                                                          1575648754872:ifelse(final_wap_df[,c] <= -91 & final_wap_df[,c] >= -104,final_wap_df[,s]<-3,
                                                                                                               1575648754872:final_wap_df[i,s]<-1)
                                                                                          1575648754873:)
                                                                     1575648754873:)
                                                1575648754873:)
                           1575648754874:)
                    1575648754874:)
1575648768038:ifelse(final_wap_df[,c] <= -1 & final_wap_df[,c] >=-50, final_wap_df[,s]<-10,
                     1575648768039:ifelse(final_wap_df[,c] <= -51 & final_wap_df[,c] >= -60,final_wap_df[,s]<-8,
                                          1575648768040:ifelse(final_wap_df[,c] <= -61 & final_wap_df[,c] >= -70,final_wap_df[,s]<-6,
                                                               1575648768040:ifelse(final_wap_df[,c] <= -71 & final_wap_df[,c] >= -90,final_wap_df[,s]<-5,
                                                                                    1575648768040:ifelse(final_wap_df[,c] <= -91 & final_wap_df[,c] >= -104,final_wap_df[,s]<-3,
                                                                                                         1575648768041:final_wap_df[i,s]<-1)
                                                                                    1575648768041:)
                                                               1575648768041:)
                                          1575648768042:)
                     1575648768042:)
1575648831670:final_wap_df[,c]
1575648850750:c
1575648883853:final_wap_df %>%
  1575648922629:ifelse([,c] <= -1 & [,c] >=-50, [,s]<-10,[,s]<-1
                       1575648942461:final_wap_df %>%
                         1575648942462:+ ifelse([,c] <= -1 & [,c] >=-50, [,s]<-10,[,s]<-1)
                       1575649028201:# --- New Signal_Quality
                         1575649028201:w<-final_wap_df
                       1575649028616:EoF<-nrow(w)
                       1575649028886:c<-which(colnames(final_wap_df)=="MAX_WAP")
                       1575649029190:s<-which(colnames(final_wap_df)=="SIGNAL_QUALITY")
                       1575649033605:w
                       1575649038767:EoF
                       1575649041749:c
                       1575649044405:s
                       1575649103544:for(i in 1:EoF) {
                         1575649103544:if (w[i,c] <= -1 & w[i,c] >=-50) {
                           1575649103545:w[i,s]<-10  # Excellent
                           1575649103545:} else if (w[i,c] <= -51 & w[i,c] >= -60) {
                             1575649103546:w[i,s]<-8   # Good
                             1575649103546:} else if (w[i,c] <= -61 & w[i,c] >= -70) {
                               1575649103546:w[i,s]<-6   # Fair
                               1575649103547:} else if (w[i,c] <= -71 & w[i,c] >= -90) {
                                 1575649103547:w[i,s]<-5   # Poor
                                 1575649103548:} else if (w[i,c] <= -91 & w[i,c] >= -104) {
                                   1575649103548:w[i,s]<-3   # No signal
                                   1575649103549:} else {
                                     1575649103549:w[i,s]<-1 # No access
                                     1575649103550:}
                         1575649113601:for(i in 1:EoF) {
                           1575649113601:if (w[i,c] <= -1 & w[i,c] >=-50) {
                             1575649113602:w[i,s]<-10  # Excellent
                             1575649113602:} else if (w[i,c] <= -51 & w[i,c] >= -60) {
                               1575649113603:w[i,s]<-8   # Good
                               1575649113603:} else if (w[i,c] <= -61 & w[i,c] >= -70) {
                                 1575649113604:w[i,s]<-6   # Fair
                                 1575649113604:} else if (w[i,c] <= -71 & w[i,c] >= -90) {
                                   1575649113604:w[i,s]<-5   # Poor
                                   1575649113605:} else if (w[i,c] <= -91 & w[i,c] >= -104) {
                                     1575649113605:w[i,s]<-3   # No signal
                                     1575649113606:} else {
                                       1575649113606:w[i,s]<-1 # No access
                                       1575649113606:}
                           1575649113607:}
                         1575649127005:for (i in 1:EoF) {
                           1575649127006:if (signal[i] <= -1 & signal[i] >= -50) {
                             1575649127006:final_wap_df$QUALITY_DESC[i]<-"Excellent"
                             1575649127007:} else if (signal[i] <= -51 & signal[i] >= -60) {
                               1575649127007:final_wap_df$QUALITY_DESC[i]<-"Good"
                               1575649127008:} else if (signal[i] <= -61 & signal[i] >= -70) {
                                 1575649127008:final_wap_df$QUALITY_DESC[i]<-"Fair"
                                 1575649127009:} else if (signal[i] <= -71 & signal[i] >= -90) {
                                   1575649127009:final_wap_df$QUALITY_DESC[i]<-"Poor"
                                   1575649127010:} else if (signal[i] <= -91 & signal[i] >= -104) {
                                     1575649127010:final_wap_df$QUALITY_DESC[i]<-"No_Signal"
                                     1575649127011:} else {
                                       1575649127011:final_wap_df$QUALITY_DESC[i] <-"Failed_Access"
                                       1575649127012:}
                           1575649127012:}
                         1575649152145:#### CHOSING THE MIN MAX VALUES BASED ON MAX_WAP VALID ACCESSES ------------------------
                         1575649152146:# --- Filter only Valid Access Signals
                           1575649152146:valid_signal_WAP_df <-signal_WAP_df %>%
                           1575649152147:filter(MAX_WAP > -100)
                         1575649152147:# --- View the Values
                           1575649152148:valid_signal_WAP_df %>%
                           1575649152149:dplyr::select(WAP, MAX_WAP) %>%
                           1575649152150:group_by(MAX_WAP) %>%
                           1575649152151:dplyr::summarise(num=n()) %>%
                           1575649152151:ggplot(aes(MAX_WAP, num))+
                           1575649152152:geom_histogram(stat="identity")
                         1575649152153:# 363973 Total valid accesses
                           1575649152154:# --- BOXPLOT STATS
                           1575649152154:# boxplot.stats(valid_signal_WAP_df$MAX_WAP, coef = 1.5, do.conf = TRUE, do.out = TRUE)
                           1575649152155:# boxplot(valid_signal_WAP_df$MAX_WAP)
                           1575649152155:# --- Find upper and lower Whisker values
                           1575649152156:bp<-boxplot(valid_signal_WAP_df$MAX_WAP)$stats[c(1, 5), ] # -99 -43
                         1575649152157:# Access lower whisker
                           1575649152157:upw<-bp[1]
                         1575649152158:# Access upper whisker
                           1575649152158:lww<-bp[2]
                         1575649152159:# --- Total Number of valid string access above upper whisker
                           1575649152159:valid_signal_WAP_df %>%
                           1575649152160:dplyr::select(WAP, MAX_WAP) %>%
                           1575649152160:group_by(MAX_WAP) %>%
                           1575649152160:filter(between(MAX_WAP,lww,-1)) %>%
                           1575649152161:dplyr::summarise(num=n())%>%
                           1575649152161:ggplot(aes(MAX_WAP, num))+
                           1575649152162:geom_bar(stat="identity")
                         1575649152163:# 6151 Count of valid access values that are above the upper whisker
                           1575649152164:# --- Plot adjusting the value of the lower limit
                           1575649152164:#From this we see that the consistent limit is around -34
                           1575649152165:obs_upw = -33
                         1575649152166:obs_lww = -95
                         1575649152167:valid_signal_WAP_df %>%
                           1575649152167:dplyr::select(WAP, MAX_WAP) %>%
                           1575649152167:group_by(MAX_WAP) %>%
                           1575649152168:filter(between(MAX_WAP,obs_upw,-1)) %>%
                           1575649152168:dplyr::summarise(num=n())
                         1575649152169:# 1167 Count of valid access values excluded
                           1575649152170:# --- Detect Q1 and Q3 values
                           1575649152170:quantile(valid_signal_WAP_df$MAX_WAP, c(0.25, .75))
                         1575649152170:#  25% 75%
                           1575649152171:# -88  -70
                           1575649152172:# View how many valid accesses are within this range and adapt based on the graphs
                           1575649152172:# Final values are -95 and -33 range in which 354531 valid accesses are detected
                           1575649152173:valid_signal_WAP_df %>%
                           1575649152173:dplyr::select(WAP, MAX_WAP) %>%
                           1575649152174:group_by(MAX_WAP) %>%
                           1575649152174:filter(between(MAX_WAP,obs_lww,obs_upw)) %>%
                           1575649152174:dplyr::summarise(num=n())%>%
                           1575649152175:ggplot(aes(MAX_WAP, num))+
                           1575649152176:geom_bar(stat="identity")
                         1575649152176:# 354531
                           1575649152177:# --- Check if any column has only -95
                           1575649152177:drop<-wap_df[sapply(wap_df, function(x) max(x) <= -96)] # 22 columns
                         1575649152178:drop1<-wap_df[sapply(wap_df, function(x) max(x) >= -32)] # 78 columns
                         1575649152179:# Creating a character vector named drop in which we are storing
                           1575649152179:# column names.
                           1575649152180:drop<-names(drop)
                         1575649152180:drop1<-names(drop1)
                         1575649152181:# Later we are telling R to select all the variables except the column names specified in the vector drop.
                           1575649152182:# The function names() returns all the column names and the '!' sign indicates negation.
                           1575649152183:res<-wap_df[ ,!names(wap_df) %in% drop]
                         1575649152183:final_wap_df<-res[ ,!names(res) %in% drop1]
                         1575649152184:# Add dummy columns
                           1575649152185:final_wap_df$SIGNAL_QUALITY<-1
                         1575649152185:final_wap_df$QUALITY_DESC<- "Failed_Access"
                         1575649152186:# Dataset has now 443 col
                           1575649152187:# Remove temporary dataframes
                           1575649152187:rm(drop,drop1,res)
                         1575649152188:# A faster way is to use the digest library, transposing is expensive
                           1575649152189:library(digest)
                         1575649152189:dup_col <- duplicated(sapply(final_wap_df, digest))
                         1575649152190:dup_col_df<- final_wap_df[,which(!dup_col)]
                         1575649152191:# Recreate the columns MAX_WAP and SIGNAL_QUALITY with new data WAP range
                           1575649152192:final_wap_df$MAX_WAP<-apply(final_wap_df, 1, FUN = function(x) {max(x[x])})
                         1575649152193:final_wap_df$MIN_WAP<-apply(wap_df[,-1], 1, FUN = function(x) {min(x[x<-104])})
                         1575649152193:# --- New Signal_Quality
                           1575649152194:w<-final_wap_df
                         1575649152194:EoF<-nrow(w)
                         1575649152194:c<-which(colnames(final_wap_df)=="MAX_WAP")
                         1575649152196:s<-which(colnames(final_wap_df)=="SIGNAL_QUALITY")
                         1575649152196:for(i in 1:EoF) {
                           1575649152196:if (w[i,c] <= -1 & w[i,c] >=-50) {
                             1575649152197:w[i,s]<-10  # Excellent
                             1575649152199:} else if (w[i,c] <= -51 & w[i,c] >= -60) {
                               1575649152199:w[i,s]<-8   # Good
                               1575649152200:} else if (w[i,c] <= -61 & w[i,c] >= -70) {
                                 1575649152200:w[i,s]<-6   # Fair
                                 1575649152201:} else if (w[i,c] <= -71 & w[i,c] >= -90) {
                                   1575649152201:w[i,s]<-5   # Poor
                                   1575649152202:} else if (w[i,c] <= -91 & w[i,c] >= -104) {
                                     1575649152202:w[i,s]<-3   # No signal
                                     1575649152203:} else {
                                       1575649152203:w[i,s]<-1 # No access
                                       1575649152204:}
                           1575649152204:}
                         1575649152205:# --- New QUALITY DESCRIPTION
                           1575649152206:for (i in 1:EoF) {
                             1575649152206:if (signal[i] <= -1 & signal[i] >= -50) {
                               1575649152207:final_wap_df$QUALITY_DESC[i]<-"Excellent"
                               1575649152207:} else if (signal[i] <= -51 & signal[i] >= -60) {
                                 1575649152208:final_wap_df$QUALITY_DESC[i]<-"Good"
                                 1575649152208:} else if (signal[i] <= -61 & signal[i] >= -70) {
                                   1575649152209:final_wap_df$QUALITY_DESC[i]<-"Fair"
                                   1575649152209:} else if (signal[i] <= -71 & signal[i] >= -90) {
                                     1575649152210:final_wap_df$QUALITY_DESC[i]<-"Poor"
                                     1575649152210:} else if (signal[i] <= -91 & signal[i] >= -104) {
                                       1575649152211:final_wap_df$QUALITY_DESC[i]<-"No_Signal"
                                       1575649152211:} else {
                                         1575649152212:final_wap_df$QUALITY_DESC[i] <-"Failed_Access"
                                         1575649152213:}
                             1575649152213:}
                         1575649152214:which(colnames(wifi_df)=="LONGITUDE")
                         1575649152214:which(colnames(wifi_df)=="LATITUDE")
                         1575649152215:which(colnames(wifi_df)=="FLOOR")
                         1575649152216:which(colnames(wifi_df)=="BUILDINGID")
                         1575649152216:which(colnames(wifi_df)=="SPACEID")
                         1575649152217:which(colnames(wifi_df)=="POSITIONID")
                         1575649152217:which(colnames(wifi_df)=="USERID")
                         1575649152218:which(colnames(wifi_df)=="PHONEID")
                         1575649152218:which(colnames(wifi_df)=="ID")
                         1575649152219:which(colnames(wifi_df)=="Tot_Tries")
                         1575649152219:which(colnames(wifi_df)=="Tot_Valid")
                         1575649152220:which(colnames(wifi_df)=="Tot_Invalid")
                         1575649152221:which(colnames(wifi_df)=="char_FLOOR")
                         1575649152221:which(colnames(wifi_df)=="char_FLOOR")
                         1575649152222:which(colnames(wifi_df)=="char_BUILDINGID")
                         1575649152222:which(colnames(wifi_df)=="char_POSITIONID")
                         1575649152223:which(colnames(wifi_df)=="char_SPACEID")
                         1575649152223:which(colnames(wifi_df)=="ZoneID")
                         1575649152224:which(colnames(wifi_df)=="char_ZoneID")
                         1575649152225:which(colnames(wifi_df)=="LocID")
                         1575649152225:which(colnames(wifi_df)=="char_LocID")
                         1575649152226:which(colnames(wifi_df)=="ItemID")
                         1575649152226:which(colnames(wifi_df)=="char_ItemID")
                         1575649152227:which(colnames(wifi_df)=="COORD_POINT")
                         1575649152228:# Add Longitude, Lat and Floor from the df dataset
                           1575649152229:base_df<-cbind(final_wap_df, wifi_df[,c(466:477,480:489,492)])
                         1575649152229:base_df<-base_df%>%
                           1575649152230:filter(between(MAX_WAP, -96,-30))
                         1575649152230:range(base_df$MAX_WAP)
                         1575649152231:hist(base_df$MAX_WAP, freq=FALSE)
                         1575649152232:plot(density(base_df$MAX_WAP))
                         1575649219137:for(i in 1:EoF) {
                           1575649219137:if (w[i,c] <= -1 & w[i,c] >=-50) {
                             1575649219138:w[i,s]<-10  # Excellent
                             1575649219138:} else if (w[i,c] <= -51 & w[i,c] >= -60) {
                               1575649219138:w[i,s]<-8   # Good
                               1575649219139:} else if (w[i,c] <= -61 & w[i,c] >= -70) {
                                 1575649219139:w[i,s]<-6   # Fair
                                 1575649219140:} else if (w[i,c] <= -71 & w[i,c] >= -90) {
                                   1575649219140:w[i,s]<-5   # Poor
                                   1575649219140:} else if (w[i,c] <= -91 & w[i,c] >= -104) {
                                     1575649219141:w[i,s]<-3   # No signal
                                     1575649219141:} else {
                                       1575649219142:w[i,s]<-1 # No access
                                       1575649219142:}
                           1575649219142:}
                         1575649280829:is.na(final_wap_df)
                         1575649302790:final_wap_df$MAX_WAP
                         1575649339512:# Creating a character vector named drop in which we are storing
                           1575649339513:# column names.
                           1575649339513:drop<-names(drop)
                         1575649340277:drop1<-names(drop1)
                         1575649341377:# Later we are telling R to select all the variables except the column names specified in the vector drop.
                           1575649341377:# The function names() returns all the column names and the '!' sign indicates negation.
                           1575649341378:res<-wap_df[ ,!names(wap_df) %in% drop]
                         1575649342284:final_wap_df<-res[ ,!names(res) %in% drop1]
                         1575649351940:is.na(final_wap_df)
                         1575649366054:anyNA(final_wap_df)
                         1575649378825:# Add dummy columns
                           1575649378825:final_wap_df$SIGNAL_QUALITY<-1
                         1575649379462:final_wap_df$QUALITY_DESC<- "Failed_Access"
                         1575649388476:anyNA(final_wap_df)
                         1575649399550:ncol(final_wap_df)
                         1575649408695:# Remove temporary dataframes
                           1575649408695:rm(drop,drop1,res)
                         1575649415894:# A faster way is to use the digest library, transposing is expensive
                           1575649415895:library(digest)
                         1575649416454:dup_col <- duplicated(sapply(final_wap_df, digest))
                         1575649416885:dup_col_df<- final_wap_df[,which(!dup_col)]
                         1575649427678:ncol(final_wap_df)
                         1575649440836:anyNA(final_wap_df)
                         1575649480573:names(final_wap_df)
                         1575649563924:which(colnames(final_wap_df)=="SIGNAL_QUALITY")
                         1575649587330:# Recreate the columns MAX_WAP and SIGNAL_QUALITY with new data WAP range
                           1575649587330:final_wap_df$MAX_WAP<-apply(final_wap_df[,2:465], 1, FUN = function(x) {max(x[x])})
                         1575649588144:final_wap_df$MIN_WAP<-apply(final_wap_df[,2:465], 1, FUN = function(x) {min(x[x<-104])})
                         1575649589798:# --- New Signal_Quality
                           1575649589799:w<-final_wap_df
                         1575649590494:EoF<-nrow(w)
                         1575649590958:c<-which(colnames(final_wap_df)=="MAX_WAP")
                         1575649591342:s<-which(colnames(final_wap_df)=="SIGNAL_QUALITY")
                         1575649592161:for(i in 1:EoF) {
                           1575649592162:if (w[i,c] <= -1 & w[i,c] >=-50) {
                             1575649592162:w[i,s]<-10  # Excellent
                             1575649592163:} else if (w[i,c] <= -51 & w[i,c] >= -60) {
                               1575649592163:w[i,s]<-8   # Good
                               1575649592163:} else if (w[i,c] <= -61 & w[i,c] >= -70) {
                                 1575649592164:w[i,s]<-6   # Fair
                                 1575649592164:} else if (w[i,c] <= -71 & w[i,c] >= -90) {
                                   1575649592165:w[i,s]<-5   # Poor
                                   1575649592165:} else if (w[i,c] <= -91 & w[i,c] >= -104) {
                                     1575649592165:w[i,s]<-3   # No signal
                                     1575649592166:} else {
                                       1575649592166:w[i,s]<-1 # No access
                                       1575649592167:}
                           1575649592167:}
                         1575649614333:final_wap_df$MAX_WAP# --- New QUALITY DESCRIPTION
                         1575649635470:final_wap_df$SIGNAL_QUALITY
                         1575649687900:nlevels(actor(final_wap_df$SIGNAL_QUALITY))
                         1575649704756:nlevels(factor(final_wap_df$SIGNAL_QUALITY))
                         1575649827494:signal<-final_wap_df$MAX_WAP
                         1575649851128:for (i in 1:EoF) {
                           1575649851129:if (signal[i] <= -1 & signal[i] >= -50) {
                             1575649851129:final_wap_df$QUALITY_DESC[i]<-"Excellent"
                             1575649851129:} else if (signal[i] <= -51 & signal[i] >= -60) {
                               1575649851130:final_wap_df$QUALITY_DESC[i]<-"Good"
                               1575649851130:} else if (signal[i] <= -61 & signal[i] >= -70) {
                                 1575649851131:final_wap_df$QUALITY_DESC[i]<-"Fair"
                                 1575649851131:} else if (signal[i] <= -71 & signal[i] >= -90) {
                                   1575649851131:final_wap_df$QUALITY_DESC[i]<-"Poor"
                                   1575649851132:} else if (signal[i] <= -91 & signal[i] >= -104) {
                                     1575649851132:final_wap_df$QUALITY_DESC[i]<-"No_Signal"
                                     1575649851132:} else {
                                       1575649851133:final_wap_df$QUALITY_DESC[i] <-"Failed_Access"
                                       1575649851133:}
                           1575649851134:}
                         1575649865213:final_wap_df$QUALITY_DESC
                         1575649938759:for (i in 1:EoF) {
                           1575649938760:if (signal[i] <= -1 & signal[i] >= -50) {
                             1575649938760:final_wap_df$SIGNAL_QUALITY[i]<-10
                             1575649938760:} else if (signal[i] <= -51 & signal[i] >= -60) {
                               1575649938761:final_wap_df$SIGNAL_QUALITY[i]<-8
                               1575649938761:} else if (signal[i] <= -61 & signal[i] >= -70) {
                                 1575649938762:final_wap_df$SIGNAL_QUALITY[i]<-6
                                 1575649938762:} else if (signal[i] <= -71 & signal[i] >= -90) {
                                   1575649938762:final_wap_df$SIGNAL_QUALITY[i]<-5
                                   1575649938763:} else if (signal[i] <= -91 & signal[i] >= -104) {
                                     1575649938763:final_wap_df$SIGNAL_QUALITY[i]<-3
                                     1575649938764:} else {
                                       1575649938764:final_wap_df$SIGNAL_QUALITY[i]<-1
                                       1575649938765:}
                           1575649938765:}
                         1575649966816:for (i in 1:EoF) {
                           1575649966817:if (signal[i] <= -1 & signal[i] >= -50) {
                             1575649966817:final_wap_df$SIGNAL_QUALITY[i]<-10
                             1575649966817:} else if (signal[i] <= -51 & signal[i] >= -60) {
                               1575649966818:final_wap_df$SIGNAL_QUALITY[i]<-8
                               1575649966818:} else if (signal[i] <= -61 & signal[i] >= -70) {
                                 1575649966819:final_wap_df$SIGNAL_QUALITY[i]<-6
                                 1575649966819:} else if (signal[i] <= -71 & signal[i] >= -90) {
                                   1575649966819:final_wap_df$SIGNAL_QUALITY[i]<-5
                                   1575649966820:} else if (signal[i] <= -91 & signal[i] >= -104) {
                                     1575649966820:final_wap_df$SIGNAL_QUALITY[i]<-3
                                     1575649966821:} else {
                                       1575649966821:final_wap_df$SIGNAL_QUALITY[i]<-1
                                       1575649966821:}
                           1575649966822:}
                         1575649982005:final_wap_df$SIGNAL_QUALITY
                         1575650058261:rm(EoF, signal)
                         1575650074104:# Add Longitude, Lat and Floor from the df dataset
                           1575650074105:base_df<-cbind(final_wap_df, wifi_df[,c(466:477,480:489,492)])
                         1575650075544:base_df<-base_df%>%
                           1575650075545:filter(between(MAX_WAP, -96,-30))
                         1575650116844:which(colnames(base_df)=="ID")
                         1575650146204:base_df<-cbind(final_wap_df, wifi_df[,c(466:477,480:489,492)])
                         1575650152458:base_df<-base_df%>%
                           1575650152459:filter(between(MAX_WAP, -96,-30))
                         1575650184812:tail(names(base_df),27)
                         1575650198279:# which(colnames(wifi_df)=="LONGITUDE")
                           1575650198279:# which(colnames(wifi_df)=="LATITUDE")
                           1575650198280:# which(colnames(wifi_df)=="FLOOR")
                           1575650198280:# which(colnames(wifi_df)=="BUILDINGID")
                           1575650198280:# which(colnames(wifi_df)=="SPACEID")
                           1575650198281:# which(colnames(wifi_df)=="POSITIONID")
                           1575650198281:# which(colnames(wifi_df)=="USERID")
                           1575650198281:# which(colnames(wifi_df)=="PHONEID")
                           1575650198282:which(colnames(wifi_df)=="ID")
                         1575650221039:# Add Longitude, Lat and Floor from the df dataset
                           1575650221039:base_df<-cbind(final_wap_df, wifi_df[,c(466:473,474:489,492)])
                         1575650221550:base_df<-base_df%>%
                           1575650221550:filter(between(MAX_WAP, -96,-30))
                         1575650245334:which(colnames(wifi_df)=="LONGITUDE")
                         1575650246117:which(colnames(wifi_df)=="LATITUDE")
                         1575650246797:which(colnames(wifi_df)=="FLOOR")
                         1575650247469:which(colnames(wifi_df)=="BUILDINGID")
                         1575650248301:which(colnames(wifi_df)=="SPACEID")
                         1575650250398:which(colnames(wifi_df)=="POSITIONID")
                         1575650251900:which(colnames(wifi_df)=="USERID")
                         1575650305894:# Add Longitude, Lat and Floor from the df dataset
                           1575650305895:base_df<-cbind(final_wap_df, wifi_df[,c(466:472)])
                         1575650306325:base_df<-base_df%>%
                           1575650306326:filter(between(MAX_WAP, -96,-30))
                         1575650308205:range(base_df$MAX_WAP)
                         1575650318365:which(colnames(wifi_df)=="PHONEID")
                         1575650327799:# which(colnames(wifi_df)=="ID")
                           1575650327799:which(colnames(wifi_df)=="Tot_Tries")
                         1575650330638:which(colnames(wifi_df)=="Tot_Valid")
                         1575650332221:which(colnames(wifi_df)=="Tot_Invalid")
                         1575650333381:which(colnames(wifi_df)=="char_FLOOR")
                         1575650355822:which(colnames(wifi_df)=="char_BUILDINGID")
                         1575650356492:which(colnames(wifi_df)=="char_POSITIONID")
                         1575650357036:which(colnames(wifi_df)=="char_SPACEID")
                         1575650357613:which(colnames(wifi_df)=="ZoneID")
                         1575650358317:which(colnames(wifi_df)=="char_ZoneID")
                         1575650358988:which(colnames(wifi_df)=="LocID")
                         1575650359629:which(colnames(wifi_df)=="char_LocID")
                         1575650362308:which(colnames(wifi_df)=="ItemID")
                         1575650363030:which(colnames(wifi_df)=="char_ItemID")
                         1575650364853:which(colnames(wifi_df)=="COORD_POINT")
                         1575650374127:# Add Longitude, Lat and Floor from the df dataset
                           1575650374127:base_df<-cbind(final_wap_df, wifi_df[,c(466:473, 475:477,480:489,492)])
                         1575650375197:base_df<-base_df%>%
                           1575650375197:filter(between(MAX_WAP, -96,-30))
                         1575650376589:range(base_df$MAX_WAP)
                         1575650395060:hist(base_df$MAX_WAP, freq=FALSE)
                         1575650396997:plot(density(base_df$MAX_WAP))
                         1575650436500:which(colnames(final_wap_df)=="ID")
                         1575650632687:#### PRESETATION OF ASSUMPTION -----------------
                         1575650632688:# --- Divide the area into small segments BLD-FLR; POSITION-SPACE; USER-PHONE;
                           1575650632688:bld1_df<-base_df %>%
                           1575650632689:group_by(BUILDINGID==1)
                         1575650643043:nrow(bld1_df)
                         1575650697672:#### PRESETATION OF ASSUMPTION -----------------
                         1575650697673:# --- Divide the area into small segments BLD-FLR; POSITION-SPACE; USER-PHONE;
                           1575650697673:bld1_df<-base_df %>%
                           1575650697674:dplyr::select_all() %>%
                           1575650697675:filter(BUILDINGID==1) %>%
                           1575650697676:nrow(bld1_df)
                         1575650700925:#### PRESETATION OF ASSUMPTION -----------------
                         1575650700926:# --- Divide the area into small segments BLD-FLR; POSITION-SPACE; USER-PHONE;
                           1575650700926:bld1_df<-base_df %>%
                           1575650700927:dplyr::select_all() %>%
                           1575650700927:filter(BUILDINGID==1) %>%
                           1575650700928:nrow(bld1_df)
                         1575650730176:#### PRESETATION OF ASSUMPTION -----------------
                         1575650730178:# --- Divide the area into small segments BLD-FLR; POSITION-SPACE; USER-PHONE;
                           1575650730179:bld1_df<-base_df %>%
                           1575650730179:dplyr::select_all() %>%
                           1575650730180:filter(BUILDINGID==1) %>%
                           1575650730181:nrow(bld1_df)
                         1575650784167:#### PRESETATION OF ASSUMPTION -----------------
                         1575650784167:# --- Divide the area into small segments BLD-FLR; POSITION-SPACE; USER-PHONE;
                           1575650784168:bld1_df<-base_df %>%
                           1575650784168:select(everything()) %>%
                           1575650784168:filter(BUILDINGID==1) %>%
                           1575650784169:nrow(bld1_df)
                         1575650803075:#### PRESETATION OF ASSUMPTION -----------------
                         1575650803075:# --- Divide the area into small segments BLD-FLR; POSITION-SPACE; USER-PHONE;
                           1575650803076:base_df %>%
                           1575650803076:select(everything()) %>%
                           1575650803077:filter(BUILDINGID==1) %>%
                           1575650803077:nrows(bld1_df)
                         1575650808949:#### PRESETATION OF ASSUMPTION -----------------
                         1575650808950:# --- Divide the area into small segments BLD-FLR; POSITION-SPACE; USER-PHONE;
                           1575650808950:base_df %>%
                           1575650808950:select(everything()) %>%
                           1575650808951:filter(BUILDINGID==1) %>%
                           1575650815037:#### PRESETATION OF ASSUMPTION -----------------
                         1575650815037:# --- Divide the area into small segments BLD-FLR; POSITION-SPACE; USER-PHONE;
                           1575650815038:base_df %>%
                           1575650815038:select(everything()) %>%
                           1575650815038:filter(BUILDINGID==1)
                         1575650872246:# Add Longitude, Lat and Floor from the df dataset
                           1575650872246:base_df<-cbind(final_wap_df, wifi_df[,c(466:473, 475:477,480:489,492)])
                         1575650874728:base_df<-base_df%>%
                           1575650874729:filter(between(MAX_WAP, -96,-30))
                         1575650875692:range(base_df$MAX_WAP) # -96 ~ -30
                         1575650876198:hist(base_df$MAX_WAP, freq=FALSE)
                         1575650876596:plot(density(base_df$MAX_WAP))
                         1575650882694:base_df %>%
                           1575650882695:select(everything()) %>%
                           1575650882695:filter(BUILDINGID==1)
                         1575650894903:bld1_df<-base_df %>%
                           1575650894904:select(everything()) %>%
                           1575650894905:filter(BUILDINGID==1)
                         1575650902490:nrow(bld1_df)
                         1575650938461:ncol(bld1_df)
                         1575650969029:bld2_df<-base_df %>%
                           1575650969030:select(everything()) %>%
                           1575650969030:filter(BUILDINGID==2)
                         1575650983053:bld3_df<-base_df %>%
                           1575650983054:select(everything()) %>%
                           1575650983054:filter(BUILDINGID==3)
                         1575651007234:nrow(bld2_df)
                         1575651030059:nrow(bld3_df)
                         1575651284015:corr_df<- bld1_df %>%
                           1575651284016:select_if(is.numeric) %>%
                           1575651284016:filter(MAX_WAP!= -110)
                         1575651315228:str(corr_df)
                         1575651341636:tail(str(corr_df))
                         1575651368131:summary(corr_df)
                         1575651392275:numSummary(corr_df)
                         1575651597821:corr_bld1<-bld1_df %>%
                           1575651597822:select_if(is.numeric) %>%
                           1575651597822:filter(MAX_WAP!= -110)
                         1575651660542:# View relation
                           1575651660543:qplot(Tot_Invalid, Tot_Valid, data=corr_bld1) # Inverse
                         1575651662787:hist(corr_bld1$Tot_Invalid)
                         1575651664218:hist(corr_bld1$Tot_Valid)
                         1575651675821:# Compute Correlation
                           1575651675822:cor(as.numeric(corr_bld1$ZoneID),corr_bld1$Tot_Valid) # 0,24
                         1575651687082:cor(corr_bld1$FLOOR,corr_bld1$Tot_Valid) # 0.0496
                         1575651705449:cor(corr_bld1$FLOOR,corr_bld1$Tot_Valid) # 0.0496
                         1575651715230:# Testing for null hypothesis
                           1575651715230:cor.test(as.numeric(corr_bld1$ZoneID),corr_bld1$Tot_Valid)
                         1575651740587:library(corrplot)
                         1575651741195:corr1<-cor(corr_bld1[sapply(corr_bld1,is.numeric)])
                         1575651741889:corrplot(corr1)
                         1575651810467:corr1<-cor(corr_bld1[sapply(corr_bld1)])
                         1575651817586:corr1<-cor(corr_bld1[sapply(corr_bld1)])
                         1575651908436:corr1<-cor(corr_bld1,use="complete.obs", method="pearson")
                         1575651991795:library(Hmisc)
                         1575651992556:rcorr(corr_bld1, type="pearson")
                         1575652053399:# Testing the correlation strength
                           1575652053400:cor.test(as.numeric(corr_bld1$BUILDINGID),corr_bld1$LONGITUDE) # 0.95 indicates very strong correlation
                         1575652115645:corr_bld1<-bld1_df %>%
                           1575652115645:select_if(is.numeric,na.rm=T) %>%
                           1575652115646:filter(MAX_WAP!= -110)
                         1575652130393:nrow(corr_bld1)
                         1575652153546:class(corr_bld1$ZoneID)
                         1575652163924:# Compute Correlation
                           1575652163924:cor(corr_bld1$ZoneID,corr_bld1$Tot_Valid) # 0,141
                         1575652166963:cor(corr_bld1$FLOOR,corr_bld1$Tot_Valid) # 0.141
                         1575652182524:# Testing for null hypothesis
                           1575652182525:cor.test(corr_bld1$ZoneID,corr_bld1$Tot_Valid)
                         1575652196514:corr1<-cor(corr_bld1[sapply(corr_bld1,is.numeric)])
                         1575652214706:corr1<-cor(corr_bld1)
                         1575652229748:corr1<-cor(corr_bld1,na.rm=T)
                         1575652284625:duplicated(corr_bld1)
                         1575652340565:# Correlation 0.24
                           1575652340565:findCorrelation(corr_bld1)
                         1575652434436:# Testing the correlation strength
                           1575652434437:cor.test(corr_bld1$BUILDINGID,corr_bld1$LONGITUDE) # 0.95 indicates very strong correlation
                         1575652452848:anyNA(corr_bld1)
                         1575652521435:corr1<-cor(corr_bld1,factor(ZoneID))
                         1575652540417:glimpse(corr_bld1)
                         1575652576410:corr_bld1<-bld1_df %>%
                           1575652576410:select_if(is.numeric, na.rm=T) %>%
                           1575652576411:filter(MAX_WAP> -100)
                         1575652585881:nrow(bld1_df)
                         1575652610806:corr_bld1<-bld1_df %>%
                           1575652610807:select_if(is.numeric, na.rm=T) %>%
                           1575652610807:filter(MAX_WAP>-95)
                         1575652617993:nrow(bld1_df)
                         1575652632923:corr_bld1<-bld1_df %>%
                           1575652632924:select_if(is.numeric, na.rm=T) %>%
                           1575652632924:#filter(MAX_WAP>-95)
                           1575652632925:# View relation
                           1575652632925:library(corrplot)
                         1575652639284:corr_bld1<-bld1_df %>%
                           1575652639285:select_if(is.numeric, na.rm=T)# %>%
                         1575652645424:nrow(bld1_df)
                         1575652713467:corr1<-cor(corr_bld1, method="pearson")
                         1575652727907:corr1<-cor(corr_bld1, method="pearson"use = "complete.obs")
                         1575652740890:corr1<-cor(corr_bld1, method="pearson", use = "complete.obs")
                         1575652779275:cor(corr1)
                         1575652812027:cor(corr_bld1)
                         1575652867383:c1<-cor(corr_bld1)
                         1575652885543:c1<-cor(corr_bld1)
                         1575652893377:corr_bld1<-bld1_df %>%
                           1575652893378:select_if(is.numeric, na.rm=T)
                         1575652899054:cor(corr_bld1)
                         1575653000059:cor(as.matrix(corr_bld1))
                         1575653025570:cor(corr_bld1,na.omit=T)
                         1575653113855:cor(corr_bld1$ZoneID,corr_bld1$LocID)
                         1575653124777:c1<-cor(corr_bld1$ZoneID,corr_bld1$LocID)
                         1575653125338:corrplot(c1)
                         1575653143274:corrplot(as.matrix(c1))
                         1575653153660:c1
                         1575653168490:corrplot(as.data.frame(c1))
                         1575653246578:c1<-cor(corr_bld1$ZoneID,corr_bld1$LocID, use = "complete.obs")
                         1575653247505:corrplot(c1)
                         1575653262682:corrplot(cor(corr_bld1$ZoneID,corr_bld1$LocID, use = "complete.obs"))
                         1575653277434:corrplot(as.matrix(cor(corr_bld1$ZoneID,corr_bld1$LocID, use = "complete.obs")))
                         1575653337135:cor(corr_bld1$ZoneID,corr_bld1$LocID, use = "complete.obs")
                         1575653422652:corr_bld1<-bld1_df %>%
                           1575653422653:select_if(is.numeric)
                         1575653425787:# View relation
                           1575653425788:library(corrplot)
                         1575653426458:cor(corr_bld1$ZoneID,corr_bld1$LocID, use = "complete.obs") # -0.0219
                         1575653427065:corrplot()
                         1575653478768:cov(corr_bld1)
                         1575653505898:cov(corr_bld1$ZoneID,corr_bld1$LocID, use = "complete.obs") # -0.0219
                         1575653631706:scale(corr_bld1)
                         1575653704586:scaled_corr_bld1<-scale(corr_bld1)
                         1575653713558:distance <- get_dist(scaled_corr_bld1)
                         1575653747118:library(factoextra)
                         1575653842025:if(!require(devtools)) install.packages("devtools")
                         1575653842553:devtools::install_github("kassambara/factoextra")
                         1575653889872:distance <- get_dist(scaled_corr_bld1)
                         1575653911175:install.packages("factoextra")
                         1575653952461:# install.packages("factoextra")
                           1575653952462:library(factoextra)
                         1575653957360:distance <- get_dist(scaled_corr_bld1)
                         1575654007292:fviz_dist(distance, gradient = list(low = "#00AFBB",
                                                                           1575654007292:mid = "white",
                                                                           1575654007293:high = "#FC4E07"))
                         1576050267945:#### SET WORKING DIRECTORY ----------------------------------------------
                         1576050267946:setwd("E:/UBIQUM/GITHUB/MOD3_Task03_WiFi")
                         1576050269426:#### PREPARATION --------------------------------------------------------
                         1576050269426:# --- Libraries
                           1576050269427:source("SCRIPTS/1PREPARATION/Script01_0_00-Libraries.R")
                         1576050273590:# --- Data Collection
                           1576050273590:source("SCRIPTS/1PREPARATION/Script01_1_00-Import.R")
                         1576050276747:# --- Data Inspection
                           1576050276748:source("SCRIPTS/1PREPARATION/Script01_2_00-Inspection.R")
                         1576050277125:# --- Data Cleaning
                           1576050277125:source("SCRIPTS/1PREPARATION/Script01_3_00-Cleaning.R")
                         1576050314367:#### PREPROCESSING ------------------------------------------------------
                         1576050314375:# --- Data Engineering
                           1576050314375:source("SCRIPTS/2PREPROCESS/Script02_1_00-Manipulation.R")
                         1576051564023:#### SET WORKING DIRECTORY ----------------------------------------------
                         1576051564023:setwd("E:/UBIQUM/GITHUB/MOD3_Task03_WiFi")
                         1576051565404:#### PREPARATION --------------------------------------------------------
                         1576051565404:# --- Libraries
                           1576051565405:source("SCRIPTS/1PREPARATION/Script01_0_00-Libraries.R")
                         1576051573531:# --- Data Collection
                           1576051573532:source("SCRIPTS/1PREPARATION/Script01_1_00-Import.R")
                         1576051581130:# --- Data Inspection
                           1576051581130:source("SCRIPTS/1PREPARATION/Script01_2_00-Inspection.R")
                         1576051590586:# --- Data Cleaning
                           1576051590587:source("SCRIPTS/1PREPARATION/Script01_3_00-Cleaning.R")
                         1576051629068:#### PREPROCESSING ------------------------------------------------------
                         1576051629068:# --- Data Engineering
                           1576051629069:source("SCRIPTS/2PREPROCESS/Script02_1_00-Manipulation.R")
                         1576051654074:# --- ID Field
                           1576051654075:df$ID <- seq.int(nrow(df))
                         1576051662251:# --- ID Field
                           1576051662252:df$ID <- seq.int(nrow(df))
                         1576051662729:df$ID<- as.character(df$ID)
                         1576051664061:########################################################################
                         1576051664062:# PLAN OF ATTACK:
                           1576051664063:# Check Data Types, missing values
                           1576051664063:# Drop any unnecessary field
                           1576051664064:# Categorical data: create count tables to understand different categories.
                           1576051664065:# Failed access imputation (gather and spread, calculate Max, min valid and invalid accesses)
                           1576051664065:########################################################################
                         1576051664066:# --- Headers: Transposed list of column names
                           1576051664066:headers_lst<- list(colnames(df))
                         1576051667001:headers_df <- melt(headers_lst, id="ID")
                         1576051667577:headers_df<-headers_df[-c(2)]
                         1576051668023:headers_df %>% rename(WAP=value)
                         1576051673377:rm(headers_lst)
                         1576051674539:################################################################
                         1576051674540:# --- WAP: Complete Set of WAPS
                           1576051674540:eda_wap_df<-df %>% select(ID)
                         1576051675394:eda_wap_df<-cbind(eda_wap_df,df %>% select(starts_with("WAP")))
                         1576051676460:# Calculating Column Max Values
                           1576051676461:# Get rid of the ID column
                           1576051676462:wap_df.max<-eda_wap_df[,-1]
                         1576051677389:# Calculate the colMax
                           1576051677390:wap_col_max<-sapply(wap_df.max,max)
                         1576051679051:# Change Min value of -110 to 1 to calculate true min value
                           1576051679052:WAPS<-grep("WAP", names(eda_wap_df), value=T)
                         1576051679497:eda_wap_df[,WAPS] <- sapply(eda_wap_df[,WAPS],function(x) ifelse(x==-110,1,x))
                         1576051681115:# Calculating Column Max Values
                           1576051681115:# Get rid of the ID column
                           1576051681116:wap_df.min<-eda_wap_df[,-1]
                         1576051682850:# Calculate the colMin
                           1576051682851:wap_col_min<-sapply(wap_df.min, min)
                         1576051684674:# Transfor once again the min value to -110
                           1576051684674:eda_wap_df[,WAPS] <- sapply(eda_wap_df[,WAPS],function(x) ifelse(x==1,-110,x))
                         1576051688995:# Build matrix with row bind
                           1576051688995:wap_col_matrix<-rbind(wap_col_max,wap_col_min)
                         1576051689763:# Transpose the matrix
                           1576051689764:wap_trnsp_df<-as.data.frame(t(wap_col_matrix))
                         1576051690361:colnames(wap_trnsp_df) <- c("MAX","MIN")
                         1576051693667:# save Valid row names in Matrix
                           1576051693668:wap_row_names<-rownames(wap_trnsp_df)
                         1576051694233:wap_details_df<-as.data.frame(wap_row_names)
                         1576051694714:colnames(wap_details_df) <-c("WAP")
                         1576051695355:wap_details_df<-cbind(wap_details_df,wap_trnsp_df$MAX,wap_trnsp_df$MIN)
                         1576051702236:# Total Tries
                           1576051702236:wap_details_df$Tot_Tries<-apply(df[1:465], 2, function(x) sum(x<0))
                         1576051724958:head(wap_details_df)
                         1576051841899:# Transpose the matrix
                           1576051841900:wap_trnsp_df<-as.data.frame(t(wap_col_matrix))
                         1576051842905:colnames(wap_trnsp_df) <- c("MAX","MIN")
                         1576051848222:wap_trnsp_df
                         1576051931575:wap_details_df<-cbind(wap_details_df,wap_trnsp_df$MAX,wap_trnsp_df$MIN)
                         1576051932284:colnames(wap_details_df) <- c("WAP","MAX","MIN")
                         1576051969936:wap_details_df
                         1576051998640:# save Valid row names in Matrix
                           1576051998641:wap_row_names<-rownames(wap_trnsp_df)
                         1576051999312:wap_details_df<-as.data.frame(wap_row_names)
                         1576052000207:colnames(wap_details_df) <-c("WAP")
                         1576052008671:head(wap_details_df)
                         1576052019192:wap_details_df<-as.data.frame(wap_row_names)
                         1576052019641:colnames(wap_details_df) <-c("WAP")
                         1576052020841:wap_details_df<-cbind(wap_details_df,wap_trnsp_df$MAX,wap_trnsp_df$MIN)
                         1576052021705:colnames(wap_details_df) <- c("WAP","MAX","MIN")
                         1576052029624:head(wap_details_df)
                         1576052069243:# Total Tries
                           1576052069243:wap_details_df$Tot_Tries<-apply(df[1:465], 2, function(x) sum(x<0))
                         1576052220738:# Total Tries
                           1576052220738:wap_details_df$Tot_Tries<-apply(df[2:465], 2, function(x) sum(x<0))
                         1576052343902:which(colnames(wap_details_df)=="WAP519")
                         1576052362200:which(colnames(df)=="WAP519")
                         1576052380015:# Total Tries
                           1576052380016:wap_details_df$Tot_Tries<-apply(df[1:466], 2, function(x) sum(x<0))
                         1576052393447:head(wap_details_df$Tot_Tries)
                         1576052404223:head(wap_details_df)
                         1576052411042:# Total Valid
                           1576052411042:wap_details_df$Tot_Valid<-apply(df[1:466],2,function(x)sum(x != -110))
                         1576052411922:# Total Invalid
                           1576052411923:wap_details_df$Tot_Invalid<-apply(df[2:466],2,function(x)sum(x == -110))
                         1576052420108:# Total Invalid
                           1576052420108:wap_details_df$Tot_Invalid<-apply(df[1:466],2,function(x)sum(x == -110))
                         1576052421194:# Total Within range -33 -95
                           1576052421194:wap_details_df$Tot_Upper_Range<-apply(df[1:466],2,function(x)sum(x>-95))
                         1576052425256:colnames(wap_details_df) <-c("WAP","MAX", "MIN","TOT","VALID","INVALID","URANGE")
                         1576052435991:head(wap_details_df)
                         1576052475153:rm(wap_trnsp_df,wap_row_names,wap_col_matrix,wap_col_min,WAPS,wap_df.max,wap_df.min,wap_col_max)
                         1576052477484:# Test for infinite values
                           1576052477485:wap_details_df %>% select(VALID) %>% group_by(VALID) %>% summarise(n=n()) %>% filter(is.infinite(VALID))
                         1576052511156:#################################################################
                         1576052511156:# --- Accesses: Contains info on accesses, MAX, MIN, VALID, INVALID, TOT TRIES ...
                           1576052511156:eda_accessed_df<-df %>% select(ID)
                         1576052512578:# Total WAP access tries by Row
                           1576052512578:eda_accessed_df$Accesses<-apply(df[1:465], 1, function(x) sum(x>-110))
                         1576052514402:# Total valid WAP access by Row
                           1576052514402:eda_accessed_df$Valid<-apply(df[1:465],1,function(x)sum(x!=-110))
                         1576052515961:# Total invalid WAP access by Row
                           1576052515961:eda_accessed_df$Invalid<-apply(df[1:465],1,function(x)sum(x==-110))
                         1576052518106:# --- MAX and MIN Values
                           1576052518107:# MAX WAP
                           1576052518107:# 1 indicates manipulation done in rows
                           1576052518107:# 2 indicates manipulation done in columns
                           1576052518108:# c(1,2) indicated manipulation done in both rows and columns
                           1576052518108:eda_accessed_df$MAX_WAP<-apply(eda_wap_df[,-1], 1, FUN = function(x) {max(x[x])})
                         1576052525281:eda_accessed_df$MIN_WAP<-apply(eda_wap_df[,-1], 1, FUN = function(x) {min(x[x>-95])})
                         1576052525968:# Replace Infinite values
                           1576052525969:is.na(eda_accessed_df)<-sapply(eda_accessed_df, is.infinite)
                         1576052539759:eda_accessed_df[is.na(eda_accessed_df)]<- -110
                         1576052543346:# Take out any row that has both Max and Min values -110
                           1576052543346:# eda_accessed_df<-eda_accessed_df %>% filter(MAX_WAP!=-110) # 19227 accesses
                           1576052543346:eda_accessed_df<-eda_accessed_df %>% filter(MIN_WAP>=(-95)) # 19220 accesses
                         1576052546507:# Create a new dataset with only the valid WAPS
                           1576052546507:base_df<-df %>%
                           1576052546508:filter(ID %in% eda_accessed_df$ID)
                         1576052549618:# --- QNT: Contains all quantative vars
                           1576052549618:eda_qnt_df<-base_df %>% select(ID)
                         1576052550800:eda_qnt_df$BUILDINGID<-base_df$BUILDINGID
                         1576052551408:eda_qnt_df$FLOOR<-base_df$FLOOR
                         1576052551816:eda_qnt_df$POSITIONID<-base_df$POSITIONID
                         1576052552264:eda_qnt_df$SPACEID<-base_df$SPACEID
                         1576052664468:names(base_df)
                         1576053196450:# --- View Near
                           1576053196451:# which(feature_variance$nzv == 'TRUE')
                           1576053196451:which(colname(df)=="WAP519")
                         1576053202642:# --- View Near
                           1576053202643:# which(feature_variance$nzv == 'TRUE')
                           1576053202644:which(colnames(df)=="WAP519")
                         1576053242054:head(names(df))
                         1576053252215:feature_variance[feature_variance$nzv,][1:466,]
                         1576053289808:feature_variance[feature_variance$zeroVar,][1:466,]
                         1576053308975:feature_variance[feature_variance$zeroVar,][1:466,]
                         1576053526806:dim(dfDesc)
                         1576053535440:dim(dfDescr)
                         1576053548118:dim(df$Descr)
                         1576053587687:dim(df)
                         1576053649222:nzv <- nearZeroVar(df)
                         1576053677337:filtered_df <- df[, -nzv]
                         1576053679823:dim(filtered_df)
                         1576053753568:nzv <- ZeroVar(df)
                         1576053803845:nzv <- nearZeroVar(df)
                         1576053836718:nzv
                         1576053948517:remove_cols <- nearZeroVar(df, names = TRUE,freqCut = 2, uniqueCut = 20)
                         1576053977250:remove_cols
                         1576054099102:remove_cols <- nearZeroVar(df, names = TRUE,freqCut = 2, uniqueCut = 20)
                         1576054128116:all_cols <- names(df)
                         1576054128117:nzv_df <- df[ ,setdiff(all_cols,remove_cols)]
                         1576054144420:head(all_cols)
                         1576054154523:head(remove_cols)
                         1576054164914:dim(nzv_df)
                         1576054294070:zv<-feature_variance[feature_variance$zeroVar,][1:466,]
                         1576054299420:zv
                         1576054321686:dim(zv)
                         1576054338205:nzv_df <- df[ ,setdiff(all_cols,zv)]
                         1576054350766:dim(nzv_df)
                         1576054364238:dim(df)
                         1576054426735:zv$zeroVar
                         1576054435349:zv$zeroVar==TRUE
                         1576054445318:head(zv$zeroVar==TRUE)
                         1576054505302:df[,zv]
                         1576054539006:dim(zv)
                         1576054906532:zerovar(df)
                         1576054927822:?zerovar
                         1576054936012:??zerovar
                         1576055129832:which(feature_variance$zeroVar==TRUE)
                         1576055204021:row.names((feature_variance[17,]))
                         1576055286487:# Filter it out
                           1576055286487:df<-df[,feature_variance$zeroVar==FALSE]
                         1576055338245:dim(feature_variance)
                         1576055421963:dim(df)
                         1576055487821:feature_variance[feature_variance$zeroVar,][1:467,]
                         1576055491394:which(feature_variance$zeroVar==TRUE) # Row 3 has zeroVar
                         1576055492883:row.names((feature_variance[17,])) # WAP018
                         1576055494182:# Filter it out
                           1576055494183:df<-df[,feature_variance$zeroVar==FALSE]
                         1576055503045:feature_variance[feature_variance$zeroVar,][1:466,]
                         1576055503478:which(feature_variance$zeroVar==TRUE) # Row 3 has zeroVar
                         1576055503890:row.names((feature_variance[17,])) # WAP018
                         1576055504374:# Filter it out
                           1576055504374:df<-df[,feature_variance$zeroVar==FALSE]
                         1576055630386:zv_names<-row.names((feature_variance[17,])) # WAP018
                         1576055655621:# Filter it out
                           1576055655621:df<-df[ ,-zv_names]
                         1576055663325:# Filter it out
                           1576055663325:df<-df[-zv_names]
                         1576055674204:df[zv_names]
                         1576055844119:# Filter it out
                           1576055844119:df<-df[!zv_names]
                         1576055921485:var.out<-setdiff(names(dd),zv_names)
                         1576055927923:var.out<-setdiff(names(df),zv_names)
                         1576055931460:var.out
                         1576055999061:# Filter it out
                           1576055999062:df<-df[var.out]
                         1576056005555:dim(df)
                         1576056221052:which(colnames(df)=="TIMESTAMP")
                         1576056251835:ts_df<-df$TIMESTAMP
                         1576056277123:head(ts_df)
                         1576056310948:ts_lst<-df$TIMESTAMP
                         1576056319075:lass(ts_lst)
                         1576056332914:class(ts_lst)
                         1576056392142:#### DROP UNUSED COLUMNS
                           1576056392143:# Instead of deleting transfer it to a new df
                           1576056392143:ts_col<-which(colnames(df)=="TIMESTAMP")
                         1576056399863:# Drop from the original df
                           1576056399864:df<-df %>% select(-ts_col)
                         1576056408842:dim(df)
                         1576056470901:df_lwc<-which(colnames(df)=="WAP519")
                         1576056478685:feature_variance[feature_variance$zeroVar,][1:df_lwc,]
                         1576056498085:feature_variance[feature_variance$nzv,][1:df_lwc,]
                         1576056526529:df_lwc
                         1576056600997:# Remove unnecessary vars
                           1576056600998:rm(df_lwc,feature_variance,zv_names,var.out)
                         1576056784211:rm(ts_col)
                         1576056893237:# Drop from the original df
                           1576056893238:df$TIMESTAMP<-NULL
                         1576057016014:#############################################################################
                         1576057016015:# RENAME COLUMNS
                           1576057016015:df <- df %>% rename(RELATIVEPOSITION = POSITIONID, FLOOR = FLOORID )
                         1576057039518:#############################################################################
                         1576057039519:# RENAME COLUMNS
                           1576057039519:df <- df %>% rename(POSITIONID=RELATIVEPOSITION,FLOORID=FLOOR )
                         1576057056341:#############################################################################
                         1576057056341:# RENAME COLUMNS
                           1576057056342:df <- df %>% rename(POSITIONID=RELATIVEPOSITION,
                                                             1576057056342:FLOORID=FLOOR )
                         1576057163605:#############################################################################
                         1576057163606:# MISSING DATA
                           1576057163607:anyNA(df)
                         1576057240998:#############################################################################
                         1576057240999:# MISSING DATA
                           1576057240999:na_test<-anyNA(df)
                         1576057327321:na_test
                         1576057335695:if (na_test == FALSE) {
                           1576057335695:md.pattern(df, plot = TRUE, rotate.names = FALSE)
                           1576057335696:} else {
                             1576057335696:df<-df
                             1576057335697:}
                         1576057374056:#############################################################################
                         1576057374057:# MISSING DATA
                           1576057374057:na_test<-anyNA(df)
                         1576057374420:if (na_test == TRUE) {
                           1576057374421:md.pattern(df, plot = TRUE, rotate.names = FALSE)
                           1576057374421:} else {
                             1576057374422:df<-df
                             1576057374422:}
                         1576057401404:na_test<-NULL
                         1576057407089:na_test
                         1576057410067:rm(na_test)
                         1576057422618:na_test
                         1576057430020:#############################################################################
                         1576057430020:# MISSING DATA
                           1576057430021:na_test<-anyNA(df)
                         1576057432786:rm(na_test)
                         1576057437595:na_test
                         1576057972114:sapply(df[,WAPS],function(x) rowMaxs(df, na.rm = TRUE))
                         1576057985210:rowMaxs(df, na.rm = TRUE))
1576057989680:rowMaxs(df, na.rm = TRUE)
1576058145303:#############################################################################
1576058145304:# Drop any unnecessary field (i.e WAPS containing only -110 values)
  1576058145304:# Rows with only -110 values
  1576058145305:WAPS<-grep("WAP", names(df), value=T)
1576058145979:sapply(df[,WAPS],function(x) max(x))
1576058169275:df$MAX<-sapply(df[,WAPS],function(x) max(x))
1576058217852:df$MAX<-sapply(df[,WAPS],1,function(x) max(x))
1576058308914:apply(df[1:466],2,function(x) max(x))
1576058348841:apply(df[1:466],1,function(x) max(x))
1576058513564:#############################################################################
1576058513564:# Drop any unnecessary field (i.e WAPS containing only -110 values)
  1576058513565:# Rows with only -110 values
  1576058513565:df$rMAX<-apply(df[1:466],1,function(x) max(x))
1576058514465:df$rMIN<-apply(df[1:466],1,function(x) min(x > -110))
1576058609447:df$rMAX
1576058624349:df$rMIN
1576058665073:df$rMIN<-apply(df[1:466],1,function(x) {min(x(x> -110))})
1576058681834:df$rMIN<-apply(df[1:466],1,function(x) {(min(x> -110))})
1576058690544:head(df$rMIN)
1576061751181:df$rMIN<-apply(df[1:466],1,function(x) {min(x>-95)})
1576061778426:#### SET WORKING DIRECTORY ----------------------------------------------
1576061778426:setwd("E:/UBIQUM/GITHUB/MOD3_Task03_WiFi")
1576061779299:#### PREPARATION --------------------------------------------------------
1576061779300:# --- Libraries
  1576061779300:source("SCRIPTS/1PREPARATION/Script01_0_00-Libraries.R")
1576061780843:# --- Data Collection
  1576061780843:source("SCRIPTS/1PREPARATION/Script01_1_00-Import.R")
1576061781971:# --- Data Inspection
  1576061781972:source("SCRIPTS/1PREPARATION/Script01_2_00-Inspection.R")
1576061783618:# --- Data Cleaning
  1576061783619:source("SCRIPTS/1PREPARATION/Script01_3_00-Cleaning.R")
1576061862216:# --- Data Cleaning
  1576061862217:source("SCRIPTS/1PREPARATION/Script01_3_00-Cleaning.R")
1576061949667:names(df)
1576061996025:#############################################################################
1576061996026:# Drop any unnecessary field (i.e WAPS containing only -110 values)
  1576061996026:# Rows with only -110 values
  1576061996027:df$MAX<-apply(df[1:466],1,function(x) {max(x)})
1576061998136:df$MIN<-apply(df[1:466],1,function(x) {min(x>-95)})
1576062009005:head(df$MIN)
1576062027446:df$MIN<-apply(df[1:466],1,function(x) {min(x)(x>-95)})
1576062051190:df$MIN<-apply(df[1:466],1,function(x) {min(x(x>-95))})
1576062135313:#############################################################################
1576062135313:# Drop any unnecessary field (i.e WAPS containing only -110 values)
  1576062135314:set.seed(1)
1576062135568:sec.large <- as.vector(apply(df, 1, order, decreasing=T)[2,])
1576062156717:sec.large
1576062211255:#############################################################################
1576062211256:# Drop any unnecessary field (i.e WAPS containing only -110 values)
  1576062211256:ans2 <- apply(df, 1, function(i) sort(i)[ dim(df)[2]-1])
1576062248086:ans2
1576062314314:#############################################################################
1576062314314:# Drop any unnecessary field (i.e WAPS containing only -110 values)
  1576062314314:ans2 <- apply(df, 1, function(i) max(i[-which.max(i)]))
1576062331494:ans2
1576062385849:#############################################################################
1576062385850:# Drop any unnecessary field (i.e WAPS containing only -110 values)
  1576062385850:ans2 <- apply(df[1:465], 1, function(i) max(i[-which.max(i)]))
1576062395133:ans2
1576062443270:min <- apply(df[1:465], 1, function(i) min(i[-which.min(i)]))
1576062448278:min
1576062925757:head(max)
1576062934572:max
1576062942073:#############################################################################
1576062942073:# Drop any unnecessary field (i.e WAPS containing only -110 values)
  1576062942073:max1 <- apply(df[1:465], 1, function(i) max(i[-which.max(i)]))
1576062944668:max1
1576062955108:head(max1)
1576062959824:# Rows with only -110 values
  1576062959824:df$MAX<-apply(df[1:466],1,function(x) {max(x)})
1576062967220:head(df$MAX)
1576062976360:# Rows with only -110 values
  1576062976361:df$MAX<-apply(df[1:465],1,function(x) {max(x)})
1576062983284:head(df$MAX)
1576063109016:# Drop any unnecessary field (i.e WAPS containing only -110 values)
  1576063109017:df1<-df %>%
  1576063109017:filter(MAX!=-110)
1576063113635:nrow(df)
1576063118068:nrow(df1)
1576063142935:# Drop any unnecessary field (i.e WAPS containing only -110 values)
  1576063142936:df<-df %>%
  1576063142936:filter(MAX>-95)
1576063148228:nrow(df)
1576063311321:# --- ID Field
  1576063311321:df$ID <- seq.int(nrow(df))
1576063312805:df$ID<- as.character(df$ID)
1576063316566:# --- Headers: Transposed list of column names
  1576063316567:headers_lst<- list(colnames(df))
1576063317141:headers_df <- melt(headers_lst, id="ID")
1576063317525:headers_df<-headers_df[-c(2)]
1576063317893:headers_df %>% rename(WAP=value)
1576063319372:rm(headers_lst)
1576063325165:headers_df
1576063353948:df$rMAX<-NULL
1576063364725:df$rMIN<-NULL
1576063373891:df$MIN<-NULL
1576063406038:# --- Headers: Transposed list of column names
  1576063406039:headers_lst<- list(colnames(df))
1576063406262:headers_df <- melt(headers_lst, id="ID")
1576063406613:headers_df<-headers_df[-c(2)]
1576063406964:headers_df %>% rename(WAP=value)
1576063407509:rm(headers_lst)
1576063411413:<- melt(headers_lst, id="ID")
1576063417189:headers_df
1576063849861:if("ID" %in% colnames(df))
  1576063850610:{
    1576063850610:df<-df
    1576063850611:}else{
      1576063850612:df$ID <- seq.int(nrow(df))
      1576063850612:df$ID<- as.character(df$ID)
      1576063850613:}
1576063863603:names(df)
1576063875619:df$ID<-NULL
1576063880067:names(df)
1576063912514:if("ID" %in% colnames(df)){
  1576063912515:df<-df
  1576063912516:}else{
    1576063912517:df$ID <- seq.int(nrow(df))
    1576063912517:df$ID<- as.character(df$ID)
    1576063912518:}
1576063922380:names(df)
1576063946473:########################################################################
1576063946474:# --- Headers: Transposed list of column names
  1576063946474:headers_lst<- list(colnames(df))
1576063946760:headers_df <- melt(headers_lst, id="ID")
1576063947140:headers_df<-headers_df[-c(2)]
1576063947524:headers_df %>% rename(WAP=value)
1576063948365:rm(headers_lst)
1576064084216:################################################################
1576064084217:# --- WAP: Complete Set of WAPS
  1576064084217:eda_wap_df<-headers_df %>% select(WAPS)
1576064090007:################################################################
1576064090007:# --- WAP: Complete Set of WAPS
  1576064090008:eda_wap_df<-headers_df %>% select(WAP)
1576064149744:################################################################
1576064149745:# --- WAP: Complete Set of WAPS
  1576064149745:eda_wap_df<-headers_df
1576064154117:eda_wap_df<-cbind(eda_wap_df,df %>% select(starts_with("WAP")))
1576064183727:################################################################
1576064183728:# --- WAP: Complete Set of WAPS
  1576064183728:eda_wap_df<-df %>% select("ID")
1576064184652:eda_wap_df<-cbind(eda_wap_df,df %>% select(starts_with("WAP")))
1576064199615:# Calculating Column Max Values
  1576064199615:# Get rid of the ID column
  1576064199616:wap_df.max<-eda_wap_df[,-1]
1576064200688:# Calculate the colMax
  1576064200689:wap_col_max<-sapply(wap_df.max,max)
1576064204214:# Change Min value of -110 to 1 to calculate true min value
  1576064204215:WAPS<-grep("WAP", names(eda_wap_df), value=T)
1576064204708:eda_wap_df[,WAPS] <- sapply(eda_wap_df[,WAPS],function(x) ifelse(x==-110,1,x))
1576064205530:# Calculating Column Max Values
  1576064205531:# Get rid of the ID column
  1576064205531:wap_df.min<-eda_wap_df[,-1]
1576064206751:# Calculate the colMin
  1576064206752:wap_col_min<-sapply(wap_df.min, min)
1576064208047:# Transfor once again the min value to -110
  1576064208047:eda_wap_df[,WAPS] <- sapply(eda_wap_df[,WAPS],function(x) ifelse(x==1,-110,x))
1576064208783:# Build matrix with row bind
  1576064208783:wap_col_matrix<-rbind(wap_col_max,wap_col_min)
1576064209675:# Transpose the matrix
  1576064209676:wap_trnsp_df<-as.data.frame(t(wap_col_matrix))
1576064210412:colnames(wap_trnsp_df) <- c("MAX","MIN")
1576064211148:# save Valid row names in Matrix
  1576064211149:wap_row_names<-rownames(wap_trnsp_df)
1576064211772:wap_details_df<-as.data.frame(wap_row_names)
1576064212252:colnames(wap_details_df) <-c("WAP")
1576064212727:wap_details_df<-cbind(wap_details_df,wap_trnsp_df$MAX,wap_trnsp_df$MIN)
1576064213172:colnames(wap_details_df) <- c("WAP","MAX","MIN")
1576064216949:# Total Tries
  1576064216950:wap_details_df$Tot_Tries<-apply(df[1:466], 2, function(x) sum(x<0))
1576064238133:# Total Tries
  1576064238134:wap_details_df$Tot_Tries<-apply(df[1:464], 2, function(x) sum(x<0))
1576064239153:# Total Valid
  1576064239154:wap_details_df$Tot_Valid<-apply(df[1:464],2,function(x)sum(x != -110))
1576064244756:# Total Invalid
  1576064244756:wap_details_df$Tot_Invalid<-apply(df[1:464],2,function(x)sum(x == -110))
1576064250558:# Total Within range -95
  1576064250558:wap_details_df$Tot_Upper_Range<-apply(df[1:464],2,function(x)sum(x>-95))
1576064252997:colnames(wap_details_df) <-c("WAP","MAX", "MIN","TOT","VALID","INVALID","URANGE")
1576064256045:rm(wap_trnsp_df,wap_row_names,wap_col_matrix,wap_col_min,WAPS,wap_df.max,wap_df.min,wap_col_max)
1576064259079:# Test for infinite values
  1576064259080:# A tibble of 0 x 2 is produced
  1576064259080:wap_details_df %>% select(VALID) %>% group_by(VALID) %>% summarise(n=n()) %>% filter(is.infinite(VALID))
1576064323528:#################################################################
1576064323528:# --- Accesses: Contains info on accesses, MAX, MIN, VALID, INVALID, TOT TRIES ...
  1576064323529:eda_accessed_df<-headers_df
1576064325721:# Total WAP access tries by Row
  1576064325722:eda_accessed_df$Accesses<-apply(df[1:464], 1, function(x) sum(x>-110))
1576064355091:#################################################################
1576064355091:# --- Accesses: Contains info on accesses, MAX, MIN, VALID, INVALID, TOT TRIES ...
  1576064355091:eda_accessed_df<-df %>% select(ID)
1576064355638:# Total WAP access tries by Row
  1576064355639:eda_accessed_df$Accesses<-apply(df[1:464], 1, function(x) sum(x>-110))
1576064357043:# Total valid WAP access by Row
  1576064357044:eda_accessed_df$Valid<-apply(df[1:464],1,function(x)sum(x!=-110))
1576064359014:# Total invalid WAP access by Row
  1576064359014:eda_accessed_df$Invalid<-apply(df[1:464],1,function(x)sum(x==-110))
1576064361239:# --- MAX and MIN Values
  1576064361240:# MAX WAP
  1576064361240:# 1 indicates manipulation done in rows
  1576064361241:# 2 indicates manipulation done in columns
  1576064361242:# c(1,2) indicated manipulation done in both rows and columns
  1576064361242:eda_accessed_df$MAX_WAP<-apply(eda_wap_df[,-1], 1, FUN = function(x) {max(x[x])})
1576064362683:eda_accessed_df$MIN_WAP<-apply(eda_wap_df[,-1], 1, FUN = function(x) {min(x[x>-95])})
1576064364781:# Replace Infinite values
  1576064364781:is.na(eda_accessed_df)<-sapply(eda_accessed_df, is.infinite)
1576064365683:eda_accessed_df[is.na(eda_accessed_df)]<- -110
1576064366214:# Take out any row that has both Max and Min values -110
  1576064366215:# eda_accessed_df<-eda_accessed_df %>% filter(MAX_WAP!=-110) # 19227 accesses
  1576064366215:eda_accessed_df<-eda_accessed_df %>% filter(MIN_WAP>=(-95)) # 19220 accesses
1576064366806:# Create a new dataset with only the valid WAPS
  1576064366806:base_df<-df %>%
  1576064366807:filter(ID %in% eda_accessed_df$ID)
1576064375321:eda_accessed_df
1576066138445:# Add the Long and LAT columns
  1576066138446:eda_accessed_df<-cbind(eda_accessed_df,df$LONGITUDE,df$LATITUDE)
1576066150321:head(eda_accessed_df)
1576066216180:# Create a new dataset with only the valid WAPS
  1576066216181:base_df<-df %>%
  1576066216181:filter(ID %in% eda_accessed_df$ID)
