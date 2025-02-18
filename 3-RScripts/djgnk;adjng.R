library(lme4) 
library(lmerTest) 


yard_obs <- readRDS("2-Cleaned_Data/yard_data_cleaned.RDS")



yard_obs$Basal.density <- na_if(yard_obs$Basal.density, "No")

yard_obs[is.na(yard_obs) | yard_obs=="Inf"] = NA

behaviour.in.yards.mod <- lm(BehaviourType ~ Basal.density + IsHedge + Yard.code, 
                               data = yard_obs)
