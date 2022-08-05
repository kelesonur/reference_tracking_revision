library(dplyr) # magic
library(magrittr) # piped magic
library(tidyr) # dark magic
library(ggplot2) # plots things
library(readxl) 
library(irr) 
library(stats)
library("Bolstad")
df <- read.csv("demographics.csv", sep = ";")
df$Sex %<>% recode('F' = 0, 'M' = 1)

summary <- df %>% 
  group_by(Status) %>% 
  summarise(M_Age = mean(Age.),
            SD_Age = sd(Age.),
            SD_TID_R = sd(TID_Receptive),
            M_TID_R = mean(TID_Receptive),
            SD_TID_P = sd(TID_Productive),
            M_TID_P = mean(TID_Productive),
            SD_TR_R = sd(Turkish_Reading),
            M_TR_R = mean(Turkish_Reading),
            SD_TR_W = sd(Turkish_Writing),
            M_TR_W = mean(Turkish_Writing),
            SD_TR_S = sd(Turkish_Speaking),
            M_TR_S = mean(Turkish_Speaking),
            M_Use = mean(Use),
            SD_Use = sd(Use),
            M_LOE = mean(LOE),
            SD_LOE = sd(LOE))

bayes.t.test(formula = Sex ~ Status, data = df)
bayes.t.test(Use ~ Status, data = df)
bayes.t.test(LOE ~ Status, data = df)
bayes.t.test(Age.  ~ Status, data = df)
bayes.t.test(TID_Receptive ~ Status, data = df)
bayes.t.test(TID_Productive ~ Status, data = df)
bayes.t.test(Turkish_Writing ~ Status, data = df)
bayes.t.test(Turkish_Speaking ~ Status, data = df)
bayes.t.test(Turkish_Reading ~ Status, data = df)


