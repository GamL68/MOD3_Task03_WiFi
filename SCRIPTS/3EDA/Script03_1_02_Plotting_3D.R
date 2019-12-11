# --- Understand where the rows where there are only -110 values
line_color<-"blue"

coeff<-mean(wifi_df$MAX_WAP)
lower_quantile<-quantile(wifi_df$MAX_WAP,0.25)

lower_whisker <- max(min(wifi_df$MAX_WAP), lower_quantile - 1.5 * IQR(wifi_df$MAX_WAP))

#library(grid)
plot_Zones_bld1<-wifi_df %>% 
  dplyr::select(BUILDINGID,FLOOR,ZoneID,MAX_WAP,MIN_WAP,COORD_POINT) %>% 
  filter(MAX_WAP!=-110, BUILDINGID==1) %>%
  group_by(BUILDINGID,FLOOR,ZoneID,COORD_POINT,MAX_WAP,MIN_WAP) %>% 
  dplyr::summarize(Num=n()) %>% 
  ggplot(aes(FLOOR,BUILDINGID))+
  geom_line(aes(COORD_POINT,MAX_WAP,color=MAX_WAP))+
  geom_line(aes(COORD_POINT,MIN_WAP,color=MIN_WAP))+
  geom_hline(yintercept=coeff, linetype="dashed", color = "blue")+
  geom_hline(yintercept=lower_whisker, linetype="dashed", color = "red")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none"
  )+
  theme(axis.text.y = element_text(angle = 0, hjust = 1))+
  facet_grid(BUILDINGID~FLOOR)+
  facet_wrap(~ FLOOR+ZoneID, strip.position="top", ncol=5,scales="free")+
  ggtitle("Building 1")

####

plot_Zones_bld2<-wifi_df %>% 
  dplyr::select(BUILDINGID,FLOOR,ZoneID,MAX_WAP,MIN_WAP,COORD_POINT) %>% 
  filter(MAX_WAP!=-110, BUILDINGID==2) %>%
  group_by(BUILDINGID,FLOOR,ZoneID,COORD_POINT,MAX_WAP,MIN_WAP) %>% 
  dplyr::summarize(Num=n()) %>% 
  ggplot(aes(FLOOR,BUILDINGID))+
  geom_line(aes(COORD_POINT,MAX_WAP,color=MAX_WAP))+
  geom_line(aes(COORD_POINT,MIN_WAP,color=MIN_WAP))+
  geom_hline(yintercept=coeff, linetype="dashed", color = "blue")+
  geom_hline(yintercept=lower_whisker, linetype="dashed", color = "red")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none"
  )+
  theme(axis.text.y = element_text(angle = 0, hjust = 1))+
  facet_grid(BUILDINGID~FLOOR)+
  facet_wrap(~ FLOOR+ZoneID, strip.position="top", ncol=5,scales="free")+
  ggtitle("Building 2")

###

plot_Zones_bld3<-wifi_df %>% 
  dplyr::select(BUILDINGID,FLOOR,ZoneID,MAX_WAP,MIN_WAP,COORD_POINT) %>% 
  filter(MAX_WAP!=-110, BUILDINGID==3) %>%
  group_by(BUILDINGID,FLOOR,ZoneID,COORD_POINT,MAX_WAP,MIN_WAP) %>% 
  dplyr::summarize(Num=n()) %>% 
  ggplot(aes(FLOOR,BUILDINGID))+
  geom_line(aes(COORD_POINT,MAX_WAP,color=MAX_WAP))+
  geom_line(aes(COORD_POINT,MIN_WAP,color=MIN_WAP))+
  geom_hline(yintercept=coeff, linetype="dashed", color = "blue")+
  geom_hline(yintercept=lower_whisker, linetype="dashed", color = "red")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none"
  )+
  theme(axis.text.y = element_text(angle = 0, hjust = 1))+
  facet_grid(BUILDINGID~FLOOR)+
  facet_wrap(~ FLOOR+ZoneID, strip.position="top", ncol=5,scales="free")+
  ggtitle("Building 3")


grid.arrange(plot_Zones_bld1,plot_Zones_bld2,plot_Zones_bld3,
             nrow = 3,
             top = textGrob("Max Wap Access Coordinates",gp=gpar(fontsize=20,font=3)))

###################################################################################
# --- Investigate SPACEID, USERID and FLOOR compare with MAX_WAP and SIGNAL_QUALITY
###################################################################################

plot_bld1_spacedistribution<-eda_qnt_df %>% 
  dplyr::select(BUILDINGID,POSITIONID,SPACEID,USERID,FLOOR,MAX_WAP,SIGNAL_QUALITY) %>% 
  group_by(BUILDINGID,FLOOR,POSITIONID,SPACEID,SIGNAL_QUALITY, USERID) %>% 
  filter(between(SPACEID,100,150), FLOOR==4, BUILDINGID==1)%>% 
  dplyr::summarise(num=n(),WAP=max(MAX_WAP)) %>% 
  ggplot(aes(SPACEID,num))+
  geom_point(aes(USERID))+
  facet_grid(POSITIONID~SIGNAL_QUALITY)+
  ggtitle("BUILDING 1")+
  theme(plot.title=element_text(size=8),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90),
    axis.title.y = element_blank())

plot_bld2_spacedistribution<-eda_qnt_df %>% 
  dplyr::select(BUILDINGID,POSITIONID,SPACEID,USERID,FLOOR,MAX_WAP,SIGNAL_QUALITY) %>% 
  group_by(BUILDINGID,FLOOR,POSITIONID,SPACEID,SIGNAL_QUALITY, USERID) %>% 
  filter(between(SPACEID,100,150), FLOOR==4, BUILDINGID==2)%>% 
  dplyr::summarise(num=n(),WAP=max(MAX_WAP)) %>% 
  ggplot(aes(SPACEID,num))+
  geom_point(aes(USERID))+
  facet_grid(POSITIONID~SIGNAL_QUALITY)+
  ggtitle("BUILDING 2")+
  theme(plot.title=element_text(size=8),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_blank())

plot_bld3_spacedistribution<-eda_qnt_df %>% 
  dplyr::select(BUILDINGID,POSITIONID,SPACEID,USERID,FLOOR,MAX_WAP,SIGNAL_QUALITY) %>% 
  group_by(BUILDINGID,FLOOR,POSITIONID,SPACEID,SIGNAL_QUALITY, USERID) %>% 
  filter(between(SPACEID,100,150), FLOOR==4, BUILDINGID==3)%>% 
  dplyr::summarise(num=n(),WAP=max(MAX_WAP)) %>% 
  ggplot(aes(SPACEID,num))+
  geom_point(aes(USERID))+
  facet_grid(POSITIONID~SIGNAL_QUALITY)+
  ggtitle("BUILDING 3")+
  theme(plot.title=element_text(size=8),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_blank())

grid.arrange(plot_bld1_spacedistribution,plot_bld2_spacedistribution,plot_bld3_spacedistribution,
             nrow = 3,
             top = textGrob("Max Wap User Distribution \nBy Position \n4th Floor, Space IDs 100~150",gp=gpar(fontsize=10,font=1)),
                            bottom = textGrob("Space ID (100~150)"), 
                            right = textGrob("Position"), 
                            left = textGrob("Totals"))
