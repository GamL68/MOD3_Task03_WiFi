#### PRESETATION OF ASSUMPTION -----------------
# --- Divide the area into small segments BLD-FLR; POSITION-SPACE; USER-PHONE;

bld1_df<-base_df %>% 
  select(everything()) %>% 
  filter(BUILDINGID==1)
# nrow = 5244

bld2_df<-base_df %>% 
  select(everything()) %>% 
  filter(BUILDINGID==2)
# nrow = 4896

bld3_df<-base_df %>% 
  select(everything()) %>% 
  filter(BUILDINGID==3)
# nrow 8650

# ncol = 492


