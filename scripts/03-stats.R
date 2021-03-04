
#Percentile Ranks By Pitch Type
names(df_select)

#remove some unnecessary variables for this analysis
df_milb_select <- df_milb %>% 
  dplyr::select(-c(PiratesGameID,BatterID,
                   Count,avg_velo,avg_vert_mov,avg_horz_mov,GameEventSequence,ActivityDate,
                   InitPosY))

#make the data analysis dataset
df_final <- df_select %>% mutate(PlayerName="") %>% bind_rows(df_milb) %>% 
  mutate(Perceived_Velocity=Velocity*(60.5-6)/(60.5-Extension)) ##Create Perceived Velocity just to have in the data set

#Make some summary statistics, percentile rank them and join to the 
#correlation and variable importance data
ff <- df_final %>% ungroup() %>% 
  group_by(PitcherID,PitcherHand,PitchType,PlayerName) %>%
  summarise(RV=sum(RunValue),RV_100=(RV/n())*-100,HBreak=mean(HBreak),VBreak=mean(VBreak),Velocity=mean(Velocity),SpinRate=mean(SpinRate),
            RelH=mean(RelH),RelS=mean(RelS),ForeArmAngle=mean(ForeArmAngle),Extension=mean(Extension),
            Perceived_Velo=mean(Perceived_Velocity),
            InitPosX=mean(InitPosX),InitPosZ=mean(InitPosZ),InitVelX=mean(InitVelX),
            InitVelY=mean(InitVelY),InitVelZ=mean(InitVelZ),InitAccelX=mean(InitAccelX),
            InitAccelY=mean(InitAccelY),InitAccelZ=mean(InitAccelZ),PlateX=mean(PlateX),PlateZ=mean(PlateZ),Zone_Pct=sum(IsInZone)/n(),
            FirstPitchStrikePct=sum(FirstPitchStrike)/sum(FirstPitch),
            VeloDiff=mean(velo_diff),HorzMovDiff=mean(horz_mov_diff),VertMovDiff=mean(vert_mov_diff),
            n=n()) %>% 
  filter(n>=45&PlayerName==""|PlayerName!="") %>% 
  group_by(PitchType) %>% 
  mutate_if(is.numeric,percent_rank)  %>%   
  gather(key="variable",value="percentile",-PitcherID,-PitcherHand,-PitchType,-PlayerName) %>% 
  inner_join(all_var) %>% 
  mutate(value=(percentile*importance)*100,percentile=percentile*100)
#View(df_final)

#Do the same but don't percentile rank the data, so we can keep the actual stats
ff_stat <- df_final %>% ungroup() %>% 
  group_by(PitcherID,PitcherHand,PitchType,PlayerName) %>%
  summarise(RV=sum(RunValue),RV_100=(RV/n())*-100,HBreak=mean(HBreak),VBreak=mean(VBreak),Velocity=mean(Velocity),SpinRate=mean(SpinRate),
            RelH=mean(RelH),RelS=mean(RelS),ForeArmAngle=mean(ForeArmAngle),Extension=mean(Extension),
            Perceived_Velo=mean(Perceived_Velocity),#Create Perceived Velocity
            InitPosX=mean(InitPosX),InitPosZ=mean(InitPosZ),InitVelX=mean(InitVelX),
            InitVelY=mean(InitVelY),InitVelZ=mean(InitVelZ),InitAccelX=mean(InitAccelX),
            InitAccelY=mean(InitAccelY),InitAccelZ=mean(InitAccelZ),PlateX=mean(PlateX),PlateZ=mean(PlateZ),Zone_Pct=sum(IsInZone)/n(),
            FirstPitchStrikePct=sum(FirstPitchStrike)/sum(FirstPitch),
            VeloDiff=mean(velo_diff),HorzMovDiff=mean(horz_mov_diff),VertMovDiff=mean(vert_mov_diff),n=n()) %>% 
  filter(n>=45&PlayerName==""|PlayerName!="") %>% 
  ungroup() %>% 
  gather(key="variable",value="stat",-PitcherID,-PitcherHand,-PitchType,-PlayerName) %>% 
  inner_join(all_var) 

#Join the data to make a pitch components dataset
pitch_components <- ff %>% 
  inner_join(ff_stat) %>% 
  filter(PlayerName!="") %>% 
write_csv('data/pitch_components.csv')
#View(pitch_components)
# Run Value By Pitch Type

#Make a dataset that shows stats by level for our 5 pitchers
stats <- df_final %>% ungroup() %>% 
  group_by(PitcherID,PitcherHand,PitchType,PlayerName,LevelofPlayTypeCode) %>%
  summarise(RV=sum(RunValue),RV_100=(RV/n())*-100,n=n()) %>% 
  group_by(PitchType) %>% 
  filter(n>=45&PlayerName==""|PlayerName!="") %>% 
  mutate(RV_100=percent_rank(RV_100)*100) %>% 
  filter(PlayerName!="") %>% 
  write_csv('data/stats.csv')
names(df_final)

#Make a dataset that shows overall stats for our 5 pitchers
overall_stats <- df_final %>% ungroup() %>% 
  group_by(PitcherID,PitcherHand,PlayerName,PitchType) %>%
  summarise(RV=sum(RunValue),RV_100=(RV/n())*-100,n=n(),AVG_MPH=mean(Velocity),MAX_MPH=max(Velocity),RPM=mean(SpinRate),
            Perceived_Velo=mean(Perceived_Velocity)) %>% 
  group_by(PitchType) %>% 
  filter(n>=45&PlayerName==""|PlayerName!="") %>% 
  mutate(RV_100=percent_rank(RV_100)*100) %>% 
  filter(PlayerName!="") %>% 
  write_csv('data/overall_stats.csv')
  

