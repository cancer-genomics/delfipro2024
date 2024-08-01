library(plyr)
library(dplyr)
library(devtools)
load_all("~/Dropbox/useful.stuff.aa")
library(tidyverse)
library(data.table)
library(here)
library(readxl)
meta2_CV<-fread(here("data","scores","All_Scores_CV.csv"))
meta2_Val<-fread(here("data","scores","All_Scores_Val.csv"))

meta2_CV<-meta2_CV %>% filter(training_set_include_screening=="yes")
#meta2_Val<-meta2_Val %>% filter(diag_val=="yes"|screen_val=="yes")
meta2_Val<-meta2_Val %>% filter(screen_val=="yes")



library(pROC)

sens<-function(d,dv,spec,full_dat) {

  d3<-roc(d$classifier_type,d$`Screening ZEUS`,levels=c("healthy",'cancer')) %>% coords(transpose=FALSE) %>% filter(specificity>=spec) %>% slice(1)
  d3_t<-roc(full_dat$classifier_type,full_dat$`Screening ZEUS`,levels=c("healthy",'cancer')) %>% coords(transpose=FALSE) %>% filter(specificity>=spec) %>% slice(1)
  d3$threshold<-d3_t$threshold
  d3$model<-"Screening DELFI+CA125+HE4"
  d3$val_sens<-(dv %>% filter(`Screening ZEUS`>=d3$threshold & classifier_type=="cancer") %>% nrow())/(dv %>% filter(classifier_type=="cancer") %>% nrow())
  d3$val_spec<-(dv %>% filter(`Screening ZEUS`<d3$threshold & classifier_type=="healthy") %>% nrow())/(dv %>% filter(classifier_type=="healthy") %>% nrow())
  
  d4<-roc(d$classifier_type,d$ca125,levels=c("healthy",'cancer')) %>% coords(transpose=FALSE) %>% filter(specificity>=spec) %>% slice(1)
  d4_t<-roc(full_dat$classifier_type,full_dat$ca125,levels=c("healthy",'cancer')) %>% coords(transpose=FALSE) %>% filter(specificity>=spec) %>% slice(1)
  d4$threshold<-d4_t$threshold
  d4$model<-"ca125"
  d4$val_sens<-(dv %>% filter(`ca125`>=d4$threshold & classifier_type=="cancer") %>% nrow())/(dv %>% filter(classifier_type=="cancer") %>% nrow())
  d4$val_spec<-(dv %>% filter(`ca125`<d4$threshold & classifier_type=="healthy") %>% nrow())/(dv %>% filter(classifier_type=="healthy") %>% nrow())
  
  d5<-roc(d$classifier_type,d$he4,levels=c("healthy",'cancer')) %>% coords(transpose=FALSE) %>% filter(specificity>=spec) %>% slice(1)
  d5_t<-roc(full_dat$classifier_type,full_dat$he4,levels=c("healthy",'cancer')) %>% coords(transpose=FALSE) %>% filter(specificity>=spec) %>% slice(1)
  d5$threshold<-d5_t$threshold
  d5$model<-"he4"
  d5$val_sens<-(dv %>% filter(`he4`>=d5$threshold & classifier_type=="cancer") %>% nrow())/(dv %>% filter(classifier_type=="cancer") %>% nrow())
  d5$val_spec<-(dv %>% filter(`he4`<d5$threshold & classifier_type=="healthy") %>% nrow())/(dv %>% filter(classifier_type=="healthy") %>% nrow())
  
  dat<-rbind(d3,d4,d5)
  dat$set_specificity<-spec
  
  
  dat
  
}


ref_dat<-meta2_CV
res<-sens(meta2_CV,meta2_Val,1,ref_dat)

for(v in c(.99,.95,.9,.85,.8,.75,.7)) {
  r<-sens(meta2_CV,meta2_Val,v,ref_dat)
  res<-rbind(res,r)
  print(v)
}

res$set<-"All Cancers"
res2<-res

########
res<-sens(meta2_CV %>% filter(classifier_type=="healthy"|subtype_simple=="HGSOC"),meta2_Val %>% filter(classifier_type=="healthy"|subtype_simple=="HGSOC"),1,ref_dat)

for(v in c(.99,.95,.9,.85,.8,.75,.7)) {
  r<-sens(meta2_CV %>% filter(classifier_type=="healthy"|subtype_simple=="HGSOC"),meta2_Val %>% filter(classifier_type=="healthy"|subtype_simple=="HGSOC"),v,ref_dat)
  res<-rbind(res,r)
  print(v)
}

res$set<-"HGSOC"
res2<-rbind(res2,res)

########
res<-sens(meta2_CV %>% filter(classifier_type=="healthy"|stage=="I"),meta2_Val %>% filter(classifier_type=="healthy"|stage=="I"),1,ref_dat)

for(v in c(.99,.95,.9,.85,.8,.75,.7)) {
  r<-sens(meta2_CV %>% filter(classifier_type=="healthy"|stage=="I"),meta2_Val %>% filter(classifier_type=="healthy"|stage=="I"),v,ref_dat)
  res<-rbind(res,r)
  print(v)
}

res$set<-"Stage I"
res2<-rbind(res2,res)

########
res<-sens(meta2_CV %>% filter(classifier_type=="healthy"|stage=="II"),meta2_Val %>% filter(classifier_type=="healthy"|stage=="II"),1,ref_dat)

for(v in c(.99,.95,.9,.85,.8,.75,.7)) {
  r<-sens(meta2_CV %>% filter(classifier_type=="healthy"|stage=="II"),meta2_Val %>% filter(classifier_type=="healthy"|stage=="II"),v,ref_dat)
  res<-rbind(res,r)
  print(v)
}

res$set<-"Stage II"
res2<-rbind(res2,res)

########
res<-sens(meta2_CV %>% filter(classifier_type=="healthy"|stage=="III"),meta2_Val %>% filter(classifier_type=="healthy"|stage=="III"),1,ref_dat)

for(v in c(.99,.95,.9,.85,.8,.75,.7)) {
  r<-sens(meta2_CV %>% filter(classifier_type=="healthy"|stage=="III"),meta2_Val %>% filter(classifier_type=="healthy"|stage=="III"),v,ref_dat)
  res<-rbind(res,r)
  print(v)
}

res$set<-"Stage III"
res2<-rbind(res2,res)

########
########
res<-sens(meta2_CV %>% filter(classifier_type=="healthy"|stage=="IV"),meta2_Val %>% filter(classifier_type=="healthy"|stage=="IV"),1,ref_dat)

for(v in c(.99,.95,.9,.85,.8,.75,.7)) {
  r<-sens(meta2_CV %>% filter(classifier_type=="healthy"|stage=="IV"),meta2_Val %>% filter(classifier_type=="healthy"|stage=="IV"),v,ref_dat)
  res<-rbind(res,r)
  print(v)
}

res$set<-"Stage IV"
res2<-rbind(res2,res)

########

write.csv(res2,"../data/tables/Allcancer_screen.csv")



