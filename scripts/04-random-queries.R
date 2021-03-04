
str(test)
test$ActivityDate <- as.Date(test$ActivityDate, "%m/%d/%Y")
test$ActivityDate
#Get Player Age
age <- test %>% 
  group_by(PlayerName) %>% 
  slice(1) %>% 
  mutate(Age=round(as.numeric(PitcherAge+(Sys.Date()-ActivityDate)/365.25),1)) %>% 
  select(PlayerName,Age)

test %>% 
  group_by(PlayerName,LevelofPlayTypeCode) %>% 
  summarise(PitcherAge=mean(PitcherAge)) %>% 
  inner_join(age) %>% 
  write_csv('data/bio.csv')

names
names(df_milb)
df_milb %>% 
  dplyr::select(PlayerName,ActivityDate,LevelofPlayTypeCode,PlateX,PlateZ,FirstPitch,PitchType,BatSide,IsInZone,RelH,RelS) %>% write_csv('data/plate_location.csv')

#Get data by date
df_milb %>% ungroup() %>% 
  group_by(ActivityDate,PlayerName,LevelofPlayTypeCode,PitchType) %>% 
  summarise(mph=mean(Velocity),max=max(Velocity),rpm=mean(SpinRate),n=n(),FirstPitchStrikePct=sum(FirstPitchStrike)/sum(FirstPitch)) %>% 
  arrange(PlayerName,ActivityDate) %>% View() write_csv('data/by_date.csv')

#make some generic summary stats
df_milb %>% ungroup() %>% 
  group_by(PlayerName,PitchType) %>% 
  summarise(mph=mean(Velocity),rpm=mean(SpinRate)) %>% 
  arrange(PlayerName,desc(mph),desc(rpm)) %>% write_csv('data/summary_stats.csv')

#look at the 1st pitch strike data
df_final %>% ungroup() %>% filter(PlayerName!="") %>%  
  group_by(PlayerName,PitcherHand) %>% summarise(FirstPitchStrikePct=sum(FirstPitchStrike)/sum(FirstPitch))


df_final %>% ungroup() %>% filter(PlayerName!="") %>%  
  group_by(PlayerName,PitcherHand,PitchType) %>% 
  count() %>% 
  spread(PitchType,n) 

