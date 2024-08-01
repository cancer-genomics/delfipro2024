library(plyr)
library(dplyr)
library(devtools)
load_all("~/Dropbox/useful.stuff.aa")
library(tidyverse)
library(data.table)
library(here)
library(readxl)
meta<-fread(here("data","metadata.csv")) %>% select(-V1)

model<-fread(here("data/scores","Screening_CV.csv"))
model2<-fread(here("data/scores","Screening_Val.csv"))
delfi<-rbind(model %>% select(id,score),model2 %>%select(id,score)) %>% rename("Screening ZEUS"="score")
meta2<-left_join(meta,delfi,by="id")

model<-fread(here("data/scores","Diagnostic_CV.csv"))
model2<-fread(here("data/scores","Diagnostic_Val.csv"))
delfi<-rbind(model %>% select(id,score),model2 %>%select(id,score)) %>% rename("Diagnostic ZEUS"="score")
meta2<-left_join(meta2,delfi,by="id")


proteins<-fread(here("data","Proteins.csv"))
proteins<-proteins %>% select(id,ca125,he4)

meta2<-left_join(meta2,proteins,by="id")

meta2_CV<-meta2 %>% filter(diag_val=="no" & screen_val=="no")
meta2_val<-meta2 %>% filter(diag_val=="yes"|screen_val=="yes")

write.csv(meta2_CV,"../data/scores/All_Scores_CV.csv")
write.csv(meta2_val,"../data/scores/All_Scores_Val.csv")


