library(plyr)
library(dplyr)
library(devtools)
load_all("~/Dropbox/useful.stuff.aa")
library(tidyverse)
library(data.table)
library(here)
library(readxl)
meta<-fread(here("data","metadata.csv")) %>% select(-V1)

diag<-meta %>% filter(training_set_include_diag=="yes")
diag_val<-meta %>% filter(training_set_include_diag=="no")


train<-fread(here("data","diag_train_data.csv")) %>% select(-V1)
test<-fread(here("data","diag_test_data.csv")) %>% select(-V1)

library(caret)
library(recipes)
library(pROC)
#train DELFI in a cross-validated manner
recipe_seq <- recipe(type ~ ., data=train) %>%
	 step_rm(starts_with("cov")) %>%
    update_role(id, new_role = "ID") %>%
    step_pca(starts_with("ratio_"), prefix = "ratio_pc_",  threshold=0.90)     %>%
    step_corr(all_predictors(), threshold=0.95) %>%
    step_nzv(all_predictors())


rocstats <- function(obs, score) {
    roc <- pROC::roc
    roc <- suppressMessages(roc(obs, score,
                                levels=c("healthy", "cancer"),
                                ci=TRUE))
    list(sens = rev(roc[["sensitivities"]]),
         spec = rev(roc[["specificities"]]),
         thresh = rev(roc$threshold),
         auc = roc$ci[2],
         lower = roc$ci[1],
         upper = roc$ci[3])
}

maxSens<-function(data,lev,model) {
  r<-rocstats(data$obs,data$cancer)
  m<-r$sens[length(r$spec[r$spec==1])]
  print(m)
  c(max_sens = m)
}

glmnetGrid <- expand.grid(
    alpha = 1,
    lambda = 10^seq(-5, -1, length.out = 100))
#### Train models
set.seed(1234)
ctrl_all <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 10,
                     verboseIter = TRUE,
                     savePredictions="final",
                     classProbs=TRUE,
                     index=createMultiFolds(train$type, 5, 10),
                     summaryFunction = twoClassSummary)
set.seed(1234)
model_delfi <- caret::train(recipe_seq,
                          data = train,
                          method = "glmnet",
                          tuneGrid = glmnetGrid,
                          trControl = ctrl_all)

res<-get_cv_preds(train,model_delfi)

res<-inner_join(res,diag %>% select(-id2) %>% rename(type3=type),by=c("id"="id"))


roc(res$type,res$score,levels=c("healthy",'cancer'))



roc(res$type,res$score,levels=c("healthy",'cancer'))%>%
coords(transpose=FALSE)
plot(roc(res$type,res$score,ci=T),print.auc=T)

write.csv(res,"../data/scores/Diagnostic_CV.csv")

saveRDS(model_delfi,"../data/models/Diagnostic.rds")

#Add the validation set
score=predict(model_delfi,test,type="prob")$cancer
test<-cbind(test %>% select(id,type),score)
res<-inner_join(test,diag_val %>% select(-id2) %>% rename(type3=type),by=c("id"="id"))
write.csv(res,"../data/scores/Diagnostic_Val.csv")





