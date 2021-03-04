#load some more packages
library(randomForest)
library(caret)
library(tidyverse)
library(corrplot)

#remove data that won't be used for feature selection
df_select <- df %>% 
  dplyr::select(-c(PiratesGameID,BatterID,
                   Count,avg_velo,avg_vert_mov,avg_horz_mov,GameEventSequence,ActivityDate,InitPosY)) 

# Feature Selection for Fastballs - Remove Correlated Predictors
fb <- df_select %>% ungroup() %>% 
  filter(PitchType %in% c("FF")) %>% 
  select(PitcherID,PitcherHand,PitchType,LevelofPlayTypeCode,BatSide,RunValue,HBreak,VBreak,Velocity,SpinRate,RelH,
         RelS,ForeArmAngle,Extension,InitPosX,InitPosZ,InitVelX,InitVelY,InitVelZ,InitAccelX,InitAccelY,InitAccelZ,PlateX,PlateZ,IsInZone,FirstPitch,FirstPitchStrike) %>% 
  group_by(PitcherID,PitcherHand,PitchType,BatSide) %>% 
  summarise(RV=sum(RunValue),RV_100=(RV/n())*100,HBreak=mean(HBreak),VBreak=mean(VBreak),Velocity=mean(Velocity),SpinRate=mean(SpinRate),
            RelH=mean(RelH),RelS=mean(RelS),ForeArmAngle=mean(ForeArmAngle),Extension=mean(Extension),
            InitPosX=mean(InitPosX),InitPosZ=mean(InitPosZ),InitVelX=mean(InitVelX),
            InitVelY=mean(InitVelY),InitVelZ=mean(InitVelZ),InitAccelX=mean(InitAccelX),
            InitAccelY=mean(InitAccelY),InitAccelZ=mean(InitAccelZ),PlateX=mean(PlateX),PlateZ=mean(PlateZ),Zone_Pct=sum(IsInZone)/n(),
            FirstPitchStrikePct=sum(FirstPitchStrike)/sum(FirstPitch),n=n()) %>% 
  arrange(RV) %>% 
  filter(n>=100)

cor_fb <- fb %>% filter(PitcherHand=="R") %>%  ungroup() %>% select(RV_100:FirstPitchStrikePct)
names(fb)
cor_fb 
correlations <- cor(cor_fb)

corrplot(correlations,order="hclust") #show corrleations for fastballs
highCorr <- findCorrelation(correlations,cutoff=.7,names=TRUE) #find highly correlated predictors
highCorr 
#remove and re-run
new_cor <- cor_fb %>% select(-c(InitVelZ,InitAccelZ,InitPosZ,InitVelY,InitVelX,InitPosX,InitAccelY,InitAccelX,RelH))
correlations1 <- cor(new_cor)
highCorr <- findCorrelation(correlations1,cutoff=.7,names=TRUE)
highCorr #all good
dev.off()
corrplot(correlations1,order="hclust")

### Use Random Forest Variable Selection to see what would
### be important if modeling the dataset based on Run Value/100

#Remove the correlated predictors
df_select1 <- df_select %>% 
  select(-c(InitVelZ,InitAccelZ,InitPosZ,InitVelY,InitVelX,InitPosX,InitAccelY,InitAccelX,RelH,velo_diff,horz_mov_diff,vert_mov_diff))
#Select average fastball data
fb <- df_select1 %>% ungroup() %>% 
  filter(PitchType %in% c("FF","FT","FC")) %>% 
  group_by(PitcherID,PitcherHand,PitchType,LevelofPlayTypeCode) %>% 
  summarise(RV=sum(RunValue),RV_100=(RV/n())*100,HBreak=mean(HBreak),VBreak=mean(VBreak),Velocity=mean(Velocity),SpinRate=mean(SpinRate),
            RelS=mean(RelS),ForeArmAngle=mean(ForeArmAngle),Extension=mean(Extension),PlateX=mean(PlateX),
            PlateZ=mean(PlateZ),Zone_Pct=sum(IsInZone)/n(),
            FirstPitchStrikePct=sum(FirstPitchStrike)/sum(FirstPitch),n=n()) %>% 
  arrange(RV) %>% 
  filter(n>=100)

#make a function to grab data on pitch type/p_throws level
f <- function(data,PitchSide,Type){
  data %>% 
    ungroup() %>% 
    filter(PitcherHand == PitchSide & PitchType==Type)}

#Grab some data
ff_l <- f(fb,"L","FF") %>% select(-c(PitcherID,PitchType,PitcherHand,n,RV))
ff_r <- f(fb,"R","FF") %>% select(-c(PitcherID,PitchType,PitcherHand,n,RV))
ft_l <- f(fb,"L","FT") %>% select(-c(PitcherID,PitchType,PitcherHand,n,RV))
ft_r <- f(fb,"R","FT") %>% select(-c(PitcherID,PitchType,PitcherHand,n,RV))
fc_l <- f(fb,"L","FC") %>% select(-c(PitcherID,PitchType,PitcherHand,n,RV))
fc_r <- f(fb,"R","FC") %>% select(-c(PitcherID,PitchType,PitcherHand,n,RV))

#Run Random Forest Model on the Data Splits
ff.rf <- randomForest(RV_100~., data=ff_r,keep.forest=FALSE,importance=TRUE,ntree=5000)
ff.lf <- randomForest(RV_100~., data=ff_l,keep.forest=FALSE,importance=TRUE,ntree=5000)
ft.rf <- randomForest(RV_100~., data=ft_r,keep.forest=FALSE,importance=TRUE,ntree=5000)
ft.lf <- randomForest(RV_100~., data=ft_l,keep.forest=FALSE,importance=TRUE,ntree=5000)
fc.rf <- randomForest(RV_100~., data=ft_r,keep.forest=FALSE,importance=TRUE,ntree=5000)
fc.lf <- randomForest(RV_100~., data=ft_l,keep.forest=FALSE,importance=TRUE,ntree=5000)

#Now get Variable Importance based on these models
var_imp_ff_r=as.data.frame(importance(ff.rf,type=1)) %>% rename(ff_r=`%IncMSE`)
var_imp_ff_l=as.data.frame(importance(ff.lf,type=1)) %>% rename(ff_l=`%IncMSE`)
var_imp_ft_r=as.data.frame(importance(ft.rf,type=1)) %>% rename(ft_r=`%IncMSE`)
var_imp_ft_l=as.data.frame(importance(ft.lf,type=1)) %>% rename(ft_l=`%IncMSE`)
var_imp_fc_r=as.data.frame(importance(fc.rf,type=1)) %>% rename(fc_r=`%IncMSE`)
var_imp_fc_l=as.data.frame(importance(fc.lf,type=1)) %>% rename(fc_l=`%IncMSE`)

#Bind the data
var_imp_fastballs <- round(bind_cols(var_imp_ff_r,var_imp_ff_l,var_imp_ft_r,var_imp_ft_l,var_imp_fc_r,var_imp_fc_l),2)

fb_importance <- var_imp_fastballs %>% mutate(ff_r= round(ff_r / sum(ff_r),2),ff_l=round(ff_l/sum(ff_l),2),
                                              ft_r =round(ft_r/sum(ft_r),2),  ft_l=round(ft_l/sum(ft_l),2),
                                              fc_r =round(fc_r/sum(fc_r),2),  fc_l=round(fc_l/sum(fc_l),2))

# Remove Correlated Predictors for Breaking Balls - Different Function 
# because we are adding the avg break and velo diff compared to fastballs here

cb <- df_select %>% ungroup() %>% 
  filter(PitchType %in% c("CH")) %>% 
  select(PitcherID,PitcherHand,PitchType,BatSide,LevelofPlayTypeCode,RunValue,HBreak,VBreak,Velocity,SpinRate,RelH,
         RelS,ForeArmAngle,Extension,InitPosX,InitPosZ,InitVelX,InitVelY,InitVelZ,InitAccelX,InitAccelY,InitAccelZ,PlateX,PlateZ,IsInZone,
         FirstPitch,FirstPitchStrike,
         velo_diff,horz_mov_diff,vert_mov_diff) %>% 
  group_by(PitcherID,PitcherHand,PitchType,BatSide) %>% 
  summarise(RV=sum(RunValue),RV_100=(RV/n())*100,HBreak=mean(HBreak),VBreak=mean(VBreak),Velocity=mean(Velocity),SpinRate=mean(SpinRate),
            RelH=mean(RelH),RelS=mean(RelS),ForeArmAngle=mean(ForeArmAngle),Extension=mean(Extension),
            InitPosX=mean(InitPosX),InitPosZ=mean(InitPosZ),InitVelX=mean(InitVelX),
            InitVelY=mean(InitVelY),InitVelZ=mean(InitVelZ),InitAccelX=mean(InitAccelX),
            InitAccelY=mean(InitAccelY),InitAccelZ=mean(InitAccelZ),PlateX=mean(PlateX),PlateZ=mean(PlateZ),Zone_Pct=sum(IsInZone)/n(),
            FirstPitchStrikePct=sum(FirstPitchStrike)/sum(FirstPitch),
            VeloDiff=mean(velo_diff),HorzMovDiff=mean(horz_mov_diff),VertMovDiff=mean(vert_mov_diff),n=n()) %>% 
  arrange(RV) %>% 
  filter(n>=100)

##Variable Importance Curves/Sliders/Changeups
cor_cb <- cb %>% filter(PitcherHand=="R") %>%  ungroup() %>% select(RV_100:VertMovDiff)

correlations_c <- cor(cor_cb)
dev.off()
corrplot(correlations_c,order="hclust")
highCorr <- findCorrelation(correlations,cutoff=.7,names=TRUE)
highCorr
new_cor <- cor_cb %>% select(-c(InitVelZ,InitAccelZ,InitPosZ,InitVelY,InitVelX,InitPosX,InitAccelY,InitAccelX,RelH))
correlations1 <- cor(new_cor)
highCorr1 <- findCorrelation(correlations1,cutoff=.7,names=TRUE)
highCorr1
dev.off()
corrplot(correlations1,order="hclust")
#same model as fastballs except with  velo/horz/vert mov diff

#Same as fastballs - now that correlated predictors are removed
#find variable importance for breaking pitches
df_select2 <- df_select %>% 
  select(-c(InitVelZ,InitAccelZ,InitPosZ,InitVelY,InitVelX,InitPosX,InitAccelY,InitAccelX,RelH))

br <- df_select2 %>% ungroup() %>% 
  filter(PitchType %in% c("CU","SL","CH")) %>% 
  group_by(PitcherID,PitcherHand,PitchType,LevelofPlayTypeCode) %>% 
  summarise(RV=sum(RunValue),RV_100=(RV/n())*100,HBreak=mean(HBreak),VBreak=mean(VBreak),Velocity=mean(Velocity),SpinRate=mean(SpinRate),
            RelS=mean(RelS),ForeArmAngle=mean(ForeArmAngle),Extension=mean(Extension),PlateX=mean(PlateX),
            PlateZ=mean(PlateZ),Zone_Pct=sum(IsInZone)/n(),FirstPitchStrikePct=sum(FirstPitchStrike)/sum(FirstPitch),
            VeloDiff=mean(velo_diff),HorzMovDiff=mean(horz_mov_diff),VertMovDiff=mean(vert_mov_diff),
            n=n()) %>% 
  arrange(RV) %>% 
  filter(n>=100)


#Grab some data - change to br

cu_l <- f(br,"L","CU") %>% select(-c(PitcherID,PitchType,PitcherHand,n,RV))
cu_r <- f(br,"R","CU") %>% select(-c(PitcherID,PitchType,PitcherHand,n,RV))
sl_l <- f(br,"L","SL") %>% select(-c(PitcherID,PitchType,PitcherHand,n,RV))
sl_r <- f(br,"R","SL") %>% select(-c(PitcherID,PitchType,PitcherHand,n,RV))
ch_l <- f(br,"L","CH") %>% select(-c(PitcherID,PitchType,PitcherHand,n,RV))
ch_r <- f(br,"R","CH") %>% select(-c(PitcherID,PitchType,PitcherHand,n,RV))

cu.rf <- randomForest(RV_100~., data=cu_r,keep.forest=FALSE,importance=TRUE,ntree=5000)
cu.lf <- randomForest(RV_100~., data=cu_l,keep.forest=FALSE,importance=TRUE,ntree=5000)
sl.rf <- randomForest(RV_100~., data=sl_r,keep.forest=FALSE,importance=TRUE,ntree=5000)
sl.lf <- randomForest(RV_100~., data=sl_l,keep.forest=FALSE,importance=TRUE,ntree=5000)
ch.rf <- randomForest(RV_100~., data=sl_r,keep.forest=FALSE,importance=TRUE,ntree=5000)
ch.lf <- randomForest(RV_100~., data=sl_l,keep.forest=FALSE,importance=TRUE,ntree=5000)

## Variable Importance
var_imp_cu_r=as.data.frame(importance(cu.rf,type=1)) %>% rename(cu_R=`%IncMSE`)
var_imp_cu_l=as.data.frame(importance(cu.lf,type=1)) %>% rename(cu_L=`%IncMSE`)
var_imp_sl_r=as.data.frame(importance(sl.rf,type=1)) %>% rename(sl_R=`%IncMSE`)
var_imp_sl_l=as.data.frame(importance(sl.lf,type=1)) %>% rename(sl_L=`%IncMSE`)
var_imp_ch_r=as.data.frame(importance(ch.rf,type=1)) %>% rename(ch_R=`%IncMSE`)
var_imp_ch_l=as.data.frame(importance(ch.lf,type=1)) %>% rename(ch_L=`%IncMSE`)
var_imp_offspeed <- round(bind_cols(var_imp_cu_r,var_imp_cu_l,var_imp_sl_r,var_imp_sl_l,var_imp_ch_r,var_imp_ch_l),2)

offspeed_importance <- var_imp_offspeed %>% mutate(cu_R= round(cu_R / sum(cu_R),2),cu_L=round(cu_L/sum(cu_L),2),
                                                   sl_R =round(sl_R/sum(sl_R),2),  sl_L=round(sl_L/sum(sl_L),2),
                                                   ch_R =round(ch_R/sum(ch_R),2),  ch_L=round(ch_L/sum(ch_L),2))

fb_importance
offspeed_importance 
#bind the data and clean it up
df <- cbind(variable = rownames(fb_importance), fb_importance)
df1 <- cbind(variable = rownames(offspeed_importance), offspeed_importance)
var_importance <- df1 %>% left_join(df)
var_importance
var_importance[is.na(var_importance)] <- 0
var_importance
### Keep the most "important" variables and scale their importance so
### it equals 1 (scaling to 1 is really useless)

top_5_var <- var_importance %>% 
  gather(key="pitch",value="importance",-variable) %>% 
  arrange(pitch,desc(importance)) %>% 
  group_by(pitch) %>% 
  filter(importance>.08) %>% 
  separate(pitch,c("PitchType","PitcherHand"),sep="_") %>% 
  mutate(PitchType=toupper(PitchType),PitcherHand=toupper(PitcherHand)) %>%
  group_by(PitchType,PitcherHand) %>% 
  mutate(importance=importance/sum(importance)) %>% 
  write_csv("data/variable_importance.csv")


## Do some correlations to explore the data further

cor_func <- function(data,name){data %>% 
  select(-LevelofPlayTypeCode) %>% 
  cor() %>% 
  as.data.frame() %>% 
  select(RV_100) %>% 
  mutate(RV_100=round(RV_100,2))}

cor_ffr <- cor_func(ff_r) %>% rename(FF_R=RV_100)
cor_ffl <- cor_func(ff_l) %>% rename(FF_L=RV_100)
cor_ftl <- cor_func(ft_l) %>% rename(FT_L=RV_100)
cor_ftr <- cor_func(ft_r) %>% rename(FT_R=RV_100)
cor_fcl <- cor_func(fc_l) %>% rename(FC_L=RV_100)
cor_fcr <- cor_func(fc_r) %>% rename(FC_R=RV_100)
cor_cul <- cor_func(cu_l) %>% rename(CU_L=RV_100)
cor_cur <- cor_func(cu_r) %>% rename(CU_R=RV_100)
cor_chl <- cor_func(ch_l) %>% rename(CH_L=RV_100)
cor_chr <- cor_func(ch_r) %>% rename(CH_R=RV_100)
cor_sll <- cor_func(sl_l) %>% rename(SL_L=RV_100)
cor_slr <- cor_func(sl_r) %>% rename(SL_R=RV_100)

correlations_fb <- bind_cols(cor_ffr,cor_ffl,cor_ftr,cor_ftl,cor_fcl,cor_fcr) %>%   
                rownames_to_column(var="variable") %>% 
                gather(key="pitch",value="correlation",-variable) 
             
correlations_offspeed <- bind_cols(cor_cul,cor_cur,cor_chr,cor_chl,cor_sll,cor_slr) %>% 
  rownames_to_column(var="variable") %>% 
  gather(key="pitch",value="correlation",-variable) 

#Keep any correlations where r>.15 - not ideal, but run values are tough
correlations <- correlations_offspeed %>% 
  bind_rows(correlations_fb) %>% 
  filter((correlation>(.15)|correlation<(-.15))&correlation!=1) %>% 
  arrange(pitch,desc(abs(correlation))) %>% 
  select(pitch,variable,correlation) %>% 
  separate(pitch,c("PitchType","PitcherHand")) %>% 
  write_csv("data/correlations.csv")
#View(correlations)
top_5_var

#Join the correlations and variable importance data
all_var <- correlations %>% full_join(top_5_var) 


