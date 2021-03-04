########################################################################### 
#General Process: Use the training data to find correlations and variable importance
#with Run Value/100 to analyze the 5 pitchers in the test set.
#Step 1: Create some metrics from data
#Step 2: Remove correlated predictors
#Step 3: Run random forest models on splits of the data (by pitch type and pitcher arm side)
#        to find most important features for building model -
#Step 4: Make some summary stats and do analysis




#load some packages
library(tidyverse)
library(naniar)

#read the data
d <- read_csv("2018MLPitchData.csv")
e <- read_csv("pitch-type-lookup.csv") #created lookup tool to condense pitch types
test <- read_csv("TestSet.csv")
#Make first pitch strike#
test_strike <- test %>% 
  arrange(PitcherID,ActivityDate,GameEventSequence) %>% 
  mutate(FirstPitch=if_else(Count=='0--0',1,0)) %>% 
  mutate(FirstPitchStrike=if_else(FirstPitch==1&BatterID!=lead(BatterID)|FirstPitch==1&lead(Count)=='0--1',1,0))

d_strike <- d %>% 
  arrange(PitcherID,ActivityDate,GameEventSequence) %>% 
  mutate(FirstPitch=if_else(Count=='0--0',1,0)) %>% 
  mutate(FirstPitchStrike=if_else(FirstPitch==1&BatterID!=lead(BatterID)|FirstPitch==1&lead(Count)=='0--1',1,0))

#Condense the pitch type
d1 <- d_strike %>% inner_join(e) %>% select(-c(PitchTypeCode,Description))
f1 <- test_strike %>% inner_join(e) %>% select(-c(PitchTypeCode,Description))
d1 %>% distinct(PitchType)
f1 %>% distinct(PitchType)

#Create velo and break differences between offspeed pitches based on pitcher's average fb.
pitch_avg <- d1 %>%
  filter(PitchType %in% c("FF", "FT"))%>%
  group_by(PitcherID)%>%
  summarise(avg_velo = mean(Velocity,na.rm=T),
            avg_vert_mov = mean(VBreak,na.rm=T),
            avg_horz_mov = mean(HBreak,na.rm=T))

pitch_avg_test <- f1 %>%
  filter(PitchType %in% c("FF", "FT"))%>%
  group_by(PitcherID,PlayerName)%>%
  summarise(avg_velo = mean(Velocity,na.rm=T),
            avg_vert_mov = mean(VBreak,na.rm=T),
            avg_horz_mov = mean(HBreak,na.rm=T))

#get fb averages
mlb <- left_join(pitch_avg, d1,  by = "PitcherID")
milb <- left_join(pitch_avg_test, f1,  by = c("PitcherID","PlayerName"))

#Do the math to get velo and break differences
mlb2 <- mlb %>% 
  mutate(
    velo_diff = ifelse(
      PitchType %in% c("SL", "CB", "FS", "KC", "CH", "FC"), 
      Velocity - avg_velo, NA),
    horz_mov_diff = ifelse(
      PitchType %in% c("SL", "CB", "FS", "KC", "CH", "FC"), 
      HBreak - avg_horz_mov, NA),
    vert_mov_diff = ifelse(
      PitchType %in% c("SL", "CB", "FS", "KC", "CH", "FC"), 
      VBreak - avg_vert_mov, NA))


milb2 <- milb %>% 
  mutate(
    velo_diff = ifelse(
      PitchType %in% c("SL", "CB", "FS", "KC", "CH", "FC"), 
      Velocity - avg_velo, NA),
    horz_mov_diff = ifelse(
      PitchType %in% c("SL", "CB", "FS", "KC", "CH", "FC"), 
      HBreak - avg_horz_mov, NA),
    vert_mov_diff = ifelse(
      PitchType %in% c("SL", "CB", "FS", "KC", "CH", "FC"), 
      VBreak - avg_vert_mov, NA))

#do some cleanup
mlb2$velo_diff[is.na(mlb2$velo_diff)] = mlb2$Velocity[is.na(mlb2$velo_diff)] - 
  mlb2$avg_velo[is.na(mlb2$velo_diff)]

mlb2$horz_mov_diff[is.na(mlb2$horz_mov_diff)] = mlb2$HBreak[is.na(mlb2$horz_mov_diff)] - 
  mlb2$avg_horz_mov[is.na(mlb2$horz_mov_diff)]

mlb2$vert_mov_diff[is.na(mlb2$vert_mov_diff)] = mlb2$VBreak[is.na(mlb2$vert_mov_diff)] - 
  mlb2$avg_vert_mov[is.na(mlb2$vert_mov_diff)]

milb2$velo_diff[is.na(milb2$velo_diff)] = milb2$Velocity[is.na(milb2$velo_diff)] - 
  milb2$avg_velo[is.na(milb2$velo_diff)]

milb2$horz_mov_diff[is.na(milb2$horz_mov_diff)] = milb2$HBreak[is.na(milb2$horz_mov_diff)] - 
  milb2$avg_horz_mov[is.na(milb2$horz_mov_diff)]

milb2$vert_mov_diff[is.na(milb2$vert_mov_diff)] = milb2$VBreak[is.na(milb2$vert_mov_diff)] - 
  milb2$avg_vert_mov[is.na(milb2$vert_mov_diff)]


mlb2$SpinRate <- as.numeric(mlb2$SpinRate)
milb2$SpinRate <- as.numeric(milb2$SpinRate)

#Imputing Mean Spin Rate by Pitcher/Pitch Type
mlb3 <- mlb2 %>% group_by(PitcherID,PitchType) %>%
  mutate(SpinRate=ifelse(is.na(SpinRate),mean(SpinRate,na.rm=TRUE),SpinRate))
milb3 <- milb2 %>% 
  group_by(PitcherID,PitchType,PlayerName) %>%
  mutate(SpinRate=ifelse(is.na(SpinRate),mean(SpinRate,na.rm=TRUE),SpinRate))

#Imputing Means for missing data by Pitcher/Pitch Type

milb4 <- milb3 %>%
  group_by(PlayerName,PitcherID,PitchType) %>%
  mutate_each(funs(replace(., which(is.na(.)),
                           mean(., na.rm=TRUE)))) 

df <- na.omit(mlb3) #kill any rows with NA probably unnecessary after the clean up above
df_milb <- milb4 # just rename this something better
rm(d,d1,mlb,mlb2,mlb3)

summary(df$Extension)
