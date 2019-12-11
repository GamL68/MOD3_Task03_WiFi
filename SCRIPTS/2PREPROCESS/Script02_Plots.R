basicPlot <- function(...){
  plot(LONGITUDE ~ LATITUDE, data=df, bty="n", lwd=2,
       main="WiFi WAP Visualization by Coordinates within BLDs", col="#00526D", 
       xlab="LONG", 
       ylab="LAT", ...)
  axis(side = 1, col="grey")
  axis(side = 2, col="grey")
}

basicPlot()

library(arm) # for 'display' function only

lsq.mod <- lsfit(df_model$LATITUDE, df_model$LONGITUDE)
basicPlot()
abline(lsq.mod, col="orange", lwd=2)
legend(x="topleft", bty="n", lwd=c(2,2), lty=c(NA,1),
       legend=c("observation", "linear least square"),
       col=c("#00526D","orange"),  pch=c(1,NA))

lin.mod <- glm(LONGITUDE ~ LATITUDE, data=df_model, 
               family=gaussian(link="identity"))

display(lin.mod)
###############################################
# --- Script02_3_00-Manipulaton
###############################################

# --- Longitude and Latitude by Floor/Building
plot_coordinates_by_FlrBld<-df %>% 
  select(char_BUILDINGID,char_FLOOR,ZoneID,LONGITUDE, LATITUDE) %>% 
  group_by(char_FLOOR,char_BUILDINGID, ZoneID, LONGITUDE, LATITUDE) %>% 
  summarise(num=n()) %>% 
  ggplot(aes(x=LATITUDE, y=LONGITUDE))+
  geom_point()+
  geom_jitter(stat="identity", 
              position="jitter", 
              na.rm = TRUE,
              aes(colour = ZoneID))+
  theme(panel.background = element_rect(fill="white", 
                                        color="grey"))+
  ggtitle("View Coordinates by ZoneID")+
  facet_grid(char_FLOOR ~ char_BUILDINGID)

plot_coordinates_by_FlrBld

#########################################################################

# --- By USERID

plot_coordinates_by_UserID<-df %>% 
  select(char_BUILDINGID,char_FLOOR,USERID,LONGITUDE, LATITUDE) %>% 
  group_by(char_FLOOR,char_BUILDINGID, USERID, LONGITUDE, LATITUDE) %>% 
  summarise(num=n()) %>% 
  ggplot(aes(x=LATITUDE, y=LONGITUDE))+
  geom_point()+
  geom_jitter(stat="identity", 
              position="jitter", 
              na.rm = TRUE,
              aes(colour = USERID))+
  theme(panel.background = element_rect(fill="white", 
                                        color="grey"))+
  ggtitle("View Coordinates by UserID")+
  facet_grid(char_FLOOR ~ char_BUILDINGID)

plot_coordinates_by_UserID

############################################################################

# --- POSITION

plot_coordinates_by_RELATIVEPOSITION<-df %>% 
  select(char_BUILDINGID,char_FLOOR,RELATIVEPOSITION,LONGITUDE, LATITUDE) %>% 
  group_by(char_FLOOR,char_BUILDINGID, RELATIVEPOSITION, LONGITUDE, LATITUDE) %>% 
  summarise(num=n()) %>% 
  ggplot(aes(x=LATITUDE, y=LONGITUDE))+
  geom_point()+
  geom_jitter(stat="identity", 
              position="jitter", 
              na.rm = TRUE,
              aes(colour = RELATIVEPOSITION))+
  theme(panel.background = element_rect(fill="white", 
                                        color="grey"))+
  ggtitle("View Coordinates by Position")+
  facet_grid(char_FLOOR ~ char_BUILDINGID)

plot_coordinates_by_RELATIVEPOSITION

############################################################################

# --- WAP accesses by Building/Floor/Position

# Plotting the Total WAPS based on Building, Floor, Position
# POSITION 1->Inside, 2->Outside (corridor)

plot_Tot_Wap_Accessed_by_FlrBld<-df %>% 
  select(BUILDINGID,FLOOR,char_POSITION,Tot_Bld_WAPS) %>% 
  group_by(BUILDINGID,FLOOR,char_POSITION) %>% 
  summarise(num=sum(Tot_Bld_WAPS)) %>% 
  ggplot(aes(x=FLOOR, 
             y=num, 
             fill=char_POSITION))+
  geom_bar(stat="identity")+
  geom_text(aes(label=num),
            angle=90,
            size=3,
            position=position_stack(vjust=.5))+
  scale_x_continuous(trans="reverse") +
  facet_grid(~BUILDINGID)+
  theme(legend.title = element_text(color="black",
                                    size=10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        panel.background = element_rect(fill="white", 
                                        color="grey"))+
  scale_fill_manual(values=c("#8571D8","#E1DEED"),
                    name=" ",
                    labels = c("Inside", "Outside"))+
  ylab("Total Count")+
  ggtitle("Total Number of WAPs \n per Building per Floor sectioned by Position")
  
plot_Tot_Wap_Accessed_by_FlrBld
###############################################