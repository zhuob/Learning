##


library(dplyr)
library(tidyr)
library(ggplot2)
library(caret )## package for classification and regression training

setwd("C:/Users/zhuob01/Desktop/Project/")
source("C:/Users/zhuob01/Desktop/Project/functions.R")
setwd("/Users/Bin/Google Drive/Study/WorkProject/GileadProject")
source("functions.R")
## data cleaning and manipulation

# read the data
ae <- read.csv("AE times.csv") %>% rename(SUBJECT = ID)
biomarker <- read.csv("Biomarker Data.csv", stringsAsFactors = F) 

## separate biomarker data into baseline and postdose
baseline <- biomarker %>% filter(WEEK == 0) %>% 
                          rename(BL = MARKER.VALUE) %>% 
                          select(-WEEK)
postdose <- biomarker %>% filter(WEEK != 0)

# merge them and 
# change the biomarker names to consistent format (e.g., M1 - M01), 
# for ease of visualization and analysis

b1 <- left_join(baseline %>% arrange(SUBJECT, MARKER.NAME), 
                postdose %>% arrange(SUBJECT, MARKER.NAME), 
                by = c("SUBJECT","MARKER.NAME")) %>% 
             mutate(MARKER.NAME = ifelse(nchar(MARKER.NAME)==2, 
                              gsub("M", "M0", MARKER.NAME), MARKER.NAME))
## combine with AE data
b2 <- right_join(ae %>% arrange(SUBJECT), b1, by = "SUBJECT") 

## remove measurements collected after AE,
## Calculate change from baseline, FC, and logFC
b3 <- b2 %>% filter(WEEK <= EVENT.TIME | is.na(EVENT.TIME)) %>%
             arrange(SUBJECT, WEEK, MARKER.NAME) %>%
             mutate(FC = (MARKER.VALUE + 0.1)/(BL + 0.1), 
                    CHG = MARKER.VALUE - BL, 
                    logFC = log(FC), 
                    AE = as.factor(ifelse(is.na(EVENT.TIME), 0, 1))) 


#### Q1
# Basic descriptive statistics of the biomarker
# q1_discreptive <- b3 %>% group_by(MARKER.NAME) %>% 
#                     summarize(mean = mean(MARKER.VALUE), 
#                               sd = sd(MARKER.VALUE), 
#                               min = min(MARKER.VALUE), 
#                               median = median(MARKER.VALUE), 
#                               max = max(MARKER.VALUE))

## visualize the biomarkers over time
ggplot(data = b3, aes(x = WEEK, y = FC)) + 
        geom_point(aes(color = AE, shape = AE), alpha = 1/5) +
        facet_wrap(~MARKER.NAME, scale = "free")

## visualize the biomarkers over time, averaged
BiomakerVStime(dat = b3, valuetype = "FC")

## whether there's correlation among biomarkers

get_correlation(b3, valuetype = "FC", plot_cor = T)
d1 <- get_correlation(b3, valuetype = "FC", plot_cor = F)




# simple ANOVA shows that M1-M4 are changing as a result of treatment
# b3 <- within(b3,  MARKER.NAME <- relevel(MARKER.NAME, ref = "M20"))
# m1 <- lm(data = b3 ,FC ~ MARKER.NAME + WEEK + MARKER.NAME*WEEK )
# r1 <- data.frame(residual = resid(m1))
# summary(m1)
# ggplot(data = r1, aes(sample = residual) ) + geom_qq()



# ## use CHG --- basically will give up this option --- poor performance
# get_correlation(b3, valuetype = "CHG", plot_cor = T) # M1-M4 are most correlated
# BiomakerVStime(dat = b3, valuetype = "CHG")  # linear M1-M4 with time
# choose_type(dat = b3, valuetype = "CHG") %>% evaluate_dat() # bad...
# 
# ## use residual of M1-M2
# b0 <- use_resid(dat = b3, valuetype = "CHG", var = "M01")
# choose_type(dat = b0, valuetype = "CHG") %>% evaluate_dat() # bad...
# b0 <- use_resid(dat = b0, valuetype = "CHG", var = "M02")
# choose_type(dat = b0, valuetype = "CHG") %>% evaluate_dat() # bad...
# b0 <- use_resid(dat = b0, valuetype = "CHG", var = "M03")
# choose_type(dat = b0, valuetype = "CHG") %>% evaluate_dat() # bad...
# b0 <- use_resid(dat = b0, valuetype = "CHG", var = "M04")
# choose_type(dat = b0, valuetype = "CHG") %>% evaluate_dat() # bad...
# get_correlation(b0, valuetype = "CHG", plot_cor = T) # M1-M4 are very correlated
# BiomakerVStime(dat = b0, valuetype = "CHG")  # linear M1-M4 with time
# 
# # ------ end of exploration --------------
# 
# ## use FC
# get_correlation(b3, valuetype = "FC", plot_cor = T) # M1-M4 are less correlated
# BiomakerVStime(dat = b3, valuetype = "FC")  # linear M1-M4 with time
# choose_type(dat = b3, valuetype = "FC") %>% evaluate_dat() # better
# 
# ## use residual of M1-M2
# b0 <- use_resid(dat = b3, valuetype = "FC", var = "M01")
# choose_type(dat = b0, valuetype = "FC") %>% evaluate_dat() # bad...
# b0 <- use_resid(dat = b0, valuetype = "FC", var = "M02")
# choose_type(dat = b0, valuetype = "FC") %>% evaluate_dat() # bad...
# b0 <- use_resid(dat = b0, valuetype = "FC", var = "M03")
# choose_type(dat = b0, valuetype = "FC") %>% evaluate_dat() # bad...
# get_correlation(b0, valuetype = "FC", plot_cor = T) # M1-M4 are very correlated
# BiomakerVStime(dat = b0, valuetype = "FC")  # linear M1-M4 with time
# choose_type(dat = b0, valuetype = "FC") %>% evaluate_dat() # bad...
# 
# 
# 
# ## use logFC
# d1 <- get_correlation(b3, valuetype = "logFC", plot_cor = F) # M1-M4 are very correlated
# BiomakerVStime(dat = b3, valuetype = "logFC")  # linear M1-M4 with time
# choose_type(dat = b3, valuetype = "logFC") %>% evaluate_dat() # bad...
# 
# ## use residual of M1-M2
# b0 <- use_resid(dat = b3, valuetype = "logFC", var = "M01")
# choose_type(dat = b0, valuetype = "logFC") %>% evaluate_dat() # bad...
# b0 <- use_resid(dat = b0, valuetype = "logFC", var = "M02")
# choose_type(dat = b0, valuetype = "logFC") %>% evaluate_dat() # bad...
# b0 <- use_resid(dat = b0, valuetype = "logFC", var = "M03")
# choose_type(dat = b0, valuetype = "logFC") %>% evaluate_dat() # bad...
# b0 <- use_resid(dat = b0, valuetype = "logFC", var = "M04")
# choose_type(dat = b0, valuetype = "logFC") %>% evaluate_dat() # bad...
# 
# get_correlation(b0, valuetype = "logFC", plot_cor = T) # M1-M4 are very correlated
# BiomakerVStime(dat = b0, valuetype = "logFC")  # linear M1-M4 with time

## I think I'll choose this model

q2 <- choose_type(b3, valuetype = "logFC")
m2 <- glm(data = q2, AE ~ . - SUBJECT , family = binomial(link = "logit")  )
res2 <- ifelse(predict(m2, type = 'response')>0.5, 1, 0)
confusionMatrix(res2, q2$AE)



# ggplot(data = b7, aes(ave_lgFC, fill = AE)) + geom_density() + 
#      facet_wrap(~MARKER.NAME, scale = "free")


# ggplot(data = b6, aes(x = WEEK, y = logFC)) + geom_point(aes(color = AE), alpha = 1/10) + 
#     facet_wrap(~MARKER.NAME, scale = "free")
      
## see whether there is any sign of AE one week before it happens

# LOGISTIC REGRESSION MODEL SELECTION https://fsu.digital.flvc.org/islandora/object/fsu:253957/datastream/PDF/view
# m3 <- glm(data = q2, outcome ~ M03  + M04 + M07 + M08 + M13 , family = binomial(link = "logit") )
# res2 <- ifelse(predict(m3, type = 'response')>0.5, 1, 0)
# confusionMatrix(res2, q2$outcome)
# 
# 
# q2_2 <- q2 %>% select(-SUBJECT) %>% mutate(AE = as.factor(AE))
# m4 <- randomForest(data = q2_2, AE ~. , importance = TRUE)
# m4
# round(importance(m4), 2)



## http://anson.ucdavis.edu/~mueller/sjsrevised.pdf
## http://online.liebertpub.com/doi/pdf/10.1089/omi.2013.0017


## https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4181610/pdf/nihms629351.pdf



## transpose for PCA 
new_dat <- pca_all(dat = b3, valuetype = "FC")

final <- new_dat %>% select(SUBJECT, AE, EVENT.TIME, MARKER.NAME, SCORE) %>%
                     spread(key = MARKER.NAME, value = SCORE) %>% 
                     select(-SUBJECT, -EVENT.TIME)  


# m4 <- glm(data = final, AE ~ .,family =  binomial(link = 'logit')) 
# res2 <- ifelse(predict(m4, type = 'response')>0.4, 1, 0)
# confusionMatrix(res2, final$AE)
# summary(m4)

## Cross-validation and penalized regression

inTraining <- createDataPartition(final$AE, p = .75, list = FALSE)
training <- final[ inTraining,]
testing  <- final[-inTraining,]


# http://topepo.github.io/caret/index.html

fitControl <- trainControl(## 10-fold CV
                method = "repeatedcv",
                number = 10,
                ## repeated ten times
                repeats = 3)

## tuning parameter is niter
tunegrid <- expand.grid(.nIter = seq(1, 150, by = 5))
set.seed(6)
blr_fit <- train(AE ~ ., data = training, 
                 method = "LogitBoost", 
                 trControl = fitControl,
                 tuneGrid = tunegrid,
                 tuneLength = 30,
                 verbose = FALSE)



## random forest
tunegrid <- expand.grid(.mtry=c(1:15))

set.seed(5)

rf_fit <- train(AE ~ ., data = training, 
              method = "rf", 
              metric = "Accuracy", 
              tuneGrid = tunegrid,
              trControl = fitControl)


## support vector machine
set.seed(5)
grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10))
svm_fit <- train(AE ~ ., data = training, 
              method = "svmLinear",
              trControl = fitControl, 
              tuneGrid = grid,
              tuneLength = 30)


## explains what each parameter means
gbmGrid <-  expand.grid(interaction.depth = 1:5, # 1 implies an additive model, 
                        # 2 implies a model with up to 2-way interactions, etc
                        n.trees = (1:10)*50, 
                        shrinkage = c(0.001,  0.01, 0.1),
                        n.minobsinnode = 10:15)
set.seed(5)

gbm_fit <- train(AE ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)


## what does Kappa mean? 
## https://datascience.stackexchange.com/questions/1108/kappa-near-to-60-in-unbalanced-110-data-set

## model comparison

resamps <- resamples(list(GBM = gbm_fit,
                          SVM_LK = svm_fit,
                          BLR = blr_fit, 
                          RF = rf_fit))

rlst <- summary(resamps)

# get the mean accuracy and mean kappa
train_accuracy <- rlst$statistics$Accuracy
train_kappa <- rlst$statistics$Kappa
ids <- row.names(train_kappa)
train_accuracy1 <- train_accuracy[ids != "SVM_LK", 4]
best_train <- names(train_accuracy1)[train_accuracy1 == max(train_accuracy1)]
train_kappa1 <- train_kappa[ids != "SVM_LK", 4]


# get the data of accuracy and Kappa for suammry and visualization
rl1 <- rlst$values %>% gather(key = Metric, value = Value) 
rl2 <- data.frame(stringr::str_split_fixed(rl1$Metric, "~", 2), rl1$Value) 
names(rl2) <- c("Classifier", "Metric", "Value")

ggplot(data = rl2, aes(x = Classifier , y = Value)) +
      geom_boxplot(aes(color = Classifier)) + 
      facet_grid(~Metric, scale = "free") + 
      theme(legend.position =  "top")
 

## get the parameters that have the best performance
f1 <- gbm_fit$finalModel
params_gbm <- c(f1$interaction.depth, f1$n.trees, f1$shrinkage, f1$n.minobsinnode)
params_svm <- svm_fit$finalModel@param$C # SVM
params_blr <- as.numeric(blr_fit$finalModel$tuneValue) # blr
params_rf <- rf_fit$finalModel$mtry # rf

## prediction accuracy
pred_gbm <- get_prediction(gbm_fit, testing, method = "GBM")
pred_blr <- get_prediction(blr_fit, testing, method = "BLR")
pred_svm <- get_prediction(svm_fit, testing, method = "SVM_LK")
pred_rf <-  get_prediction(rf_fit, testing, method = "RF")
pred_summary <- bind_rows(pred_blr$summary, pred_rf$summary, 
                          pred_svm$summary, pred_gbm$summary)
pred_outcome <- bind_rows(pred_blr$outcome, pred_rf$outcome, 
                          pred_svm$outcome, pred_gbm$outcome) %>%
                    mutate(Truth = as.factor(Reference),
                           Prediction = as.factor(Prediction))


ggplot(data = pred_outcome, aes(x = Truth, y = Prediction)) + 
      geom_point(aes(color = Classifier)) + 
      geom_text(aes(label = Freq, color = Classifier), size = 8) + 
      facet_wrap(~Classifier) 
#    
pred1 <- pred_all %>% filter(Classifier != "SVM_LK")        
max_accuracy <- max(pred1$Accuracy)      
min_accuracy <- min(pred1$Accuracy)
best_pred <- pred1$Classifier[pred1$Accuracy == max_accuracy]

## get the importance of each predictor 

importance_gbm <- get_importance(gbm_fit, method = "GBM")
importance_blr <- get_importance(blr_fit, method = "BLR")
importance_svm <- get_importance(svm_fit, method = "SVM_LK")
importance_rf <- get_importance(rf_fit, method = "RF")
dfs <-list(importance_blr, importance_rf, importance_svm, importance_gbm)
importance_all <- plyr::join_all(dfs, by = "Biomarker")

var_importance <- importance_all %>% select(Biomarker, ends_with("rank")) %>%
                  arrange(Biomarker)

## get average rank
var_importance1 <- var_importance %>% 
              mutate(AveRank = (BLR_rank + RF_rank + SVM_LK_rank + GBM_rank)/4, 
                     rank_new = rank(AveRank,  ties.method= "first"))
biomarker_interest <- var_importance1$Biomarker[var_importance1$rank_new <= 4]

