# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 


# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)

# installing boot package

install.packages('boot')
library(boot)
install.packages('ROCR')
library(ROCR)
install.packages("prediction")
library(prediction)
# Loading packages

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, stringr, tidyr) 
library(pROC)

pcr_data <- import("~/Desktop/pCR data.csv") # Importing pCR data
head(pcr_data)
summary(pcr_data) # all variables are integers
str(pcr_data)

# Lots of missing data in some variables all coded as 99

pcr_data[pcr_data == 99] <- NA

# PCR
pcr_data[pcr_data$PCR == 0,]$PCR <- "no pCR"
pcr_data[pcr_data$PCR == 1,]$PCR <- "pCR"
pcr_data$PCR <- as.factor(pcr_data$PCR)
is.factor(pcr_data$PCR)

# ER Positive
pcr_data[pcr_data$ER_Positive == 0,]$ER_Positive <- "no"
pcr_data[pcr_data$ER_Positive == 1,]$ER_Positive <- "yes"
pcr_data$ER_Positive <- as.factor(pcr_data$ER_Positive)
is.factor(pcr_data$ER_Positive) 

# PR Positive
pcr_data[pcr_data$PR_Positive == 0,]$PR_Positive <- "no"
pcr_data[pcr_data$PR_Positive == 1,]$PR_Positive <- "yes"
pcr_data$PR_Positive <- as.factor(pcr_data$PR_Positive)
is.factor(pcr_data$PR_Positive)

# HER2 Positive 
pcr_data[pcr_data$HER2_Positive == 0,]$HER2_Positive <- "no"
pcr_data[pcr_data$HER2_Positive == 1,]$HER2_Positive <- "yes"
pcr_data$HER2_Positive <- as.factor(pcr_data$HER2_Positive)
is.factor(pcr_data$HER2_Positive)

# Clinical lymph node status at Dx
pcr_data[pcr_data$Clinical_LN_Status_at_Dx == 0,]$Clinical_LN_Status_at_Dx  <- "negative"
pcr_data[pcr_data$Clinical_LN_Status_at_Dx == 1,]$Clinical_LN_Status_at_Dx  <- "positive"
pcr_data$Clinical_LN_Status_at_Dx <- as.factor(pcr_data$Clinical_LN_Status_at_Dx)
is.factor(pcr_data$Clinical_LN_Status_at_Dx)

# Diagnosis Uni
pcr_data$Diagnosis_Uni <- as.integer(pcr_data$Diagnosis_Uni)
pcr_data$Diagnosis_Uni <- factor(pcr_data$Diagnosis_Uni,
                                 levels = c(2),
                                 labels = c("invasive"))
is.factor(pcr_data$Diagnosis_Uni)

# Diagnosis L
pcr_data$Diagnosis_L <- as.integer(pcr_data$Diagnosis_L)
pcr_data$Diagnosis_L <- factor(pcr_data$Diagnosis_L,
                                 levels = c(1, 2, 3),
                                 labels = c("DCIS", "invasive", "other"))
is.factor(pcr_data$Diagnosis_L)

# Diagnosis R
pcr_data$Diagnosis_R <- as.integer(pcr_data$Diagnosis_R)
pcr_data$Diagnosis_R <- factor(pcr_data$Diagnosis_R,
                               levels = c(1, 2, 3),
                               labels = c("DCIS", "invasive", "other"))
is.factor(pcr_data$Diagnosis_R)


# Initial Tumour Size Uni
pcr_data$Initial_T_Size_Uni <- recode(pcr_data$Initial_T_Size_Uni, `1` = 1, `2` = 1, `3` = 1, `4` = 2, `5` = 2, `6` = 2, 
       `7` = 3, `8` = 3,`9` = 3, `10` = 3, `11` = 3, `12` = 3)
pcr_data$Initial_T_Size_Uni <- factor(pcr_data$Initial_T_Size_Uni,
                               levels = c(1, 2, 3),
                               labels = c("≤2", ">2 ≤5", "≥5"))
is.factor(pcr_data$Initial_T_Size_Uni)

# Initial Tumour Size L
pcr_data$Initial_T_Size_L <- recode(pcr_data$Initial_T_Size_L, `3` = 1, `4` = 2, `5` = 2, `6` = 2, 
                                      `7` = 3, `8` = 3,`9` = 3, `10` = 3, `11` = 3, `12` = 3)
pcr_data$Initial_T_Size_L <- factor(pcr_data$Initial_T_Size_L,
                                      levels = c(1, 2, 3),
                                      labels = c("≤2", ">2 ≤5", "≥5"))
is.factor(pcr_data$Initial_T_Size_L)

# Initial Tumour Size R
pcr_data$Initial_T_Size_R <- recode(pcr_data$Initial_T_Size_R, `1` = 1, `3` = 1, `4` = 2, `5` = 2, `6` = 2, 
                                      `7` = 3, `8` = 3,`9` = 3, `11` = 3, `12` = 3)
pcr_data$Initial_T_Size_R <- factor(pcr_data$Initial_T_Size_R,
                                      levels = c(1, 2, 3),
                                      labels = c("≤2", ">2 ≤5", "≥5"))
is.factor(pcr_data$Initial_T_Size_R)

# stage 
pcr_data$stage <- as.integer(pcr_data$stage)
pcr_data$stage <- factor(pcr_data$stage,
                               levels = c(1, 2, 3),
                               labels = c("I", "II", "III"))
is.factor(pcr_data$stage)

# LN Sx Type

pcr_data[pcr_data$LN_Sx_Type == 0,]$LN_Sx_Type <- "no"
pcr_data[pcr_data$LN_Sx_Type == 1,]$LN_Sx_Type <- "ALND"
pcr_data[pcr_data$LN_Sx_Type == 2,]$LN_Sx_Type <- "SND"
pcr_data[pcr_data$LN_Sx_Type == 3,]$LN_Sx_Type <- "SND then ALND"
pcr_data$LN_Sx_Type <- as.factor(pcr_data$LN_Sx_Type)
is.factor(pcr_data$LN_Sx_Type)

# BCS Candidate 
pcr_data[pcr_data$BCS_Candidate == 0,]$BCS_Candidate <- "no"
pcr_data[pcr_data$BCS_Candidate == 1,]$BCS_Candidate <- "yes"
pcr_data$BCS_Candidate <- as.factor(pcr_data$BCS_Candidate)
is.factor(pcr_data$BCS_Candidate)

# Surgery Side 
pcr_data[pcr_data$Surgery_Side == 0,]$Surgery_Side <- "unilateral"
pcr_data[pcr_data$Surgery_Side == 1,]$Surgery_Side <- "bilateral"
pcr_data$Surgery_Side <- as.factor(pcr_data$Surgery_Side)
is.factor(pcr_data$Surgery_Side)

# Specify Surgery
pcr_data[pcr_data$Specify_Surgery == 1,]$Specify_Surgery <- "BCS"
pcr_data[pcr_data$Specify_Surgery == 2,]$Specify_Surgery <- "mastectomy"
pcr_data$Specify_Surgery <- as.factor(pcr_data$Specify_Surgery)
is.factor(pcr_data$Specify_Surgery)

# Specify Surgery Uni
pcr_data$Specify_Surgery_Uni <- as.integer(pcr_data$Specify_Surgery_Uni)
pcr_data$Specify_Surgery_Uni <- factor(pcr_data$Specify_Surgery_Uni,
                               levels = c(1, 2),
                               labels = c("BCS", "mastectomy"))
is.factor(pcr_data$Specify_Surgery_Uni)

# Specify Surgery Bilat
pcr_data$Specify_Surgery_Bilat <- as.integer(pcr_data$Specify_Surgery_Bilat)
pcr_data$Specify_Surgery_Bilat <- factor(pcr_data$Specify_Surgery_Bilat,
                                       levels = c(1, 2),
                                       labels = c("BCS", "mastectomy"))
is.factor(pcr_data$Specify_Surgery_Bilat)

# chemo

pcr_data[pcr_data$chemo == 0,]$chemo <- "no"
pcr_data[pcr_data$chemo == 1,]$chemo <- "yes"
pcr_data$chemo <- as.factor(pcr_data$chemo)
is.factor(pcr_data$chemo)

# rad

pcr_data[pcr_data$rad == 0,]$rad <- "no"
pcr_data[pcr_data$rad == 1,]$rad <- "yes"
pcr_data$rad <- as.factor(pcr_data$rad)
is.factor(pcr_data$rad)

# hormone

pcr_data[pcr_data$hormone == 0,]$hormone <- "no"
pcr_data[pcr_data$hormone == 1,]$hormone <- "yes"
pcr_data$hormone <- as.factor(pcr_data$hormone)
is.factor(pcr_data$hormone)

# Herceptin_Clinical_response

pcr_data[pcr_data$Herceptin_Clinical_response == 0,]$Herceptin_Clinical_response <- "not complete"
pcr_data[pcr_data$Herceptin_Clinical_response == 1,]$Herceptin_Clinical_response <- "complete"
pcr_data$Herceptin_Clinical_response <- as.factor(pcr_data$Herceptin_Clinical_response)
is.factor(pcr_data$Herceptin_Clinical_response)

# Zone of treatment

pcr_data[pcr_data$Zone_Tx == 1,]$Zone_Tx <- "Calgary"
pcr_data[pcr_data$Zone_Tx == 2,]$Zone_Tx <- "Center"
pcr_data[pcr_data$Zone_Tx == 3,]$Zone_Tx <- "Edmonton"
pcr_data[pcr_data$Zone_Tx == 4,]$Zone_Tx <- "North"
pcr_data$Zone_Tx <- as.factor(pcr_data$Zone_Tx)
is.factor(pcr_data$Zone_Tx)

# T Stage Largest

pcr_data[pcr_data$T_Stage__Largest_ == 1,]$T_Stage__Largest_ <- "T1"
pcr_data[pcr_data$T_Stage__Largest_ == 2,]$T_Stage__Largest_ <- "T2"
pcr_data[pcr_data$T_Stage__Largest_ == 3,]$T_Stage__Largest_ <- "T3"
pcr_data[pcr_data$T_Stage__Largest_ == 4,]$T_Stage__Largest_ <- "T4"
pcr_data$T_Stage__Largest_ <- as.factor(pcr_data$T_Stage__Largest_)
is.factor(pcr_data$T_Stage__Largest_)

# Definitive surgery simple

pcr_data[pcr_data$Definitive_Sx_Simple == 1,]$Definitive_Sx_Simple <- "1"
pcr_data[pcr_data$Definitive_Sx_Simple == 2,]$Definitive_Sx_Simple <- "2"
pcr_data$Definitive_Sx_Simple <- as.factor(pcr_data$Definitive_Sx_Simple)
is.factor(pcr_data$Definitive_Sx_Simple)

xtabs(~ PCR + Herceptin_Clinical_response, data=pcr_data, addNA=T)

View(pcr_data)

############################# Lit review logistic model 

logit_LR <- glm(PCR ~ Pt_Age_at_Sx+T_Stage__Largest_+Specify_Surgery+ER_Positive+PR_Positive+HER2_Positive,
                data=pcr_data, family="binomial") #binomial for logistic regression
summary(logit_LR)

exp(cbind(OR = coef(logit_LR), confint(logit_LR))) #exponentiating regression coefficients and 95% CIs

pred_LR <- predict(logit_LR, pcr_data, type='response') #calculating probability predictions
head(pred_LR)

# Using pROC to calculate 95% CI of AUC
roc_LR <- roc(pcr_data$PCR, pred_LR, plot=T, legacy.axes=T)
auc(roc_LR)
ci(roc_LR)

#Using ROCR to generate ROC curve
pred_LR <- prediction(pred_LR, pcr_data$PCR)
perf_LR <- performance(pred_LR, "tpr", "fpr") #ROC curve
plot(perf_LR, colorize = T, ylab = "Sensitivity", xlab = "1 - Specificity") #colorize T shows p cut-offs
abline(a=0, b=1) #adds straight line

#Calculating AUC
auc_LR <- performance(pred_LR, "auc")
auc_LR <- unlist(slot(auc_LR, "y.values"))
auc_LR <- round(auc_LR, 4) #rounding AUC to 4 decimal places
legend(0.6, 0.3, auc_LR, title = "AUC", cex = 0.8) #adding AUC to graph


# Bootstrap internal validation

R = 200 #number of bootstrap samples
n = nrow(pcr_data) #sample size

# empty Rx2 matrix for bootstrap results 
B_LR = matrix(nrow = R, ncol = 3,
           dimnames = list(paste('Sample',1:R),
                           c("auc_orig","auc_boot", "opt")))

set.seed(701)
for(i in 1:R){
  
  # draw a random sample
obs.boot_LR <- sample(x = 1:n, size = n, replace = T)
data.boot_LR <- pcr_data[obs.boot_LR, ]
  
  # fit the model on bootstrap sample
logit.boot_LR <- glm(PCR ~ Pt_Age_at_Sx+T_Stage__Largest_+Specify_Surgery+ER_Positive+PR_Positive+HER2_Positive,
                  data=data.boot_LR,
                  family = "binomial")
  
  # apply model to original data
prob_LR_1 = predict(logit.boot_LR, pcr_data, type='response')
pred_LR_1 = prediction(prob_LR_1, pcr_data$PCR)
auc_LR_1 = performance(pred_LR_1,"auc")@y.values[[1]][1]
B_LR[i, 1] = auc_LR_1
  
  # apply model to bootstrap data
prob_LR_2 = predict(logit.boot_LR, data.boot_LR, type='response')
pred_LR_2 = prediction(prob_LR_2, data.boot_LR$PCR)
auc_LR_2 = performance(pred_LR_2,"auc")@y.values[[1]][1]
B_LR[i, 2] = auc_LR_2

opt_LR = (auc_LR_2-auc_LR_1)
B_LR[i, 3] = opt_LR
}

is.matrix(B_LR)
boot_data_LR <- as.data.frame(B_LR) # converting matrix to data frame
is.data.frame(boot_data_LR)

summary(boot_data_LR)

opt_auc_LR <- (0.6785 - 0.0405)
opt_auc_LR

############################# Expert logistic model

logit_exp <- glm(PCR ~ Pt_Age_at_Sx+T_Stage__Largest_+Specify_Surgery+Herceptin_Clinical_response+ER_Positive+PR_Positive+HER2_Positive+Zone_Tx,
                data=pcr_data, family="binomial") #binomial for logistic regression
summary(logit_exp)

exp(cbind(OR = coef(logit_exp), confint(logit_exp))) #exponentiating regression coefficients and 95% CIs

pred_exp <- predict(logit_exp, pcr_data, type='response') #calculating probability predictions
head(pred_exp)

# Using pROC to calculate 95% CI of AUC
roc_exp <- roc(pcr_data$PCR, pred_exp, plot=T, legacy.axes=T)
auc(roc_exp)
ci(roc_exp)

#Using ROCR to generate ROC curve
pred_exp <- prediction(pred_exp, pcr_data$PCR)
perf_exp <- performance(pred_exp, "tpr", "fpr") #ROC curve
plot(perf_exp, colorize = T, ylab = "Sensitivity", xlab = "1 - Specificity") #colorize T shows p cut-offs
abline(a=0, b=1) #adds straight line

#Calculating AUC
auc_exp <- performance(pred_exp, "auc")
auc_exp <- unlist(slot(auc_exp, "y.values"))
auc_exp <- round(auc_exp, 4) #rounding AUC to 4 decimal places
legend(0.6, 0.3, auc_exp, title = "AUC", cex = 0.8) #adding AUC to ROC curve

# Bootstrap internal validation

R = 200
n = nrow(pcr_data)

# empty Rx2 matrix for bootstrap results 
B_exp = matrix(nrow = R, ncol = 3,
              dimnames = list(paste('Sample',1:R),
                              c("auc_orig","auc_boot", "opt")))

set.seed(701)
for(i in 1:R){
  
  # draw a random sample
  obs.boot_exp <- sample(x = 1:n, size = n, replace = T)
  data.boot_exp <- pcr_data[obs.boot_exp, ]
  
  # fit the model on bootstrap sample
  logit.boot_exp <- glm(PCR ~ Pt_Age_at_Sx+T_Stage__Largest_+Specify_Surgery+Herceptin_Clinical_response+ER_Positive+PR_Positive+HER2_Positive+Zone_Tx,
                       data=data.boot_exp,
                       family = "binomial")
  
  # apply model to original data
  prob_exp_1 = predict(logit.boot_exp, pcr_data, type='response')
  pred_exp_1 = prediction(prob_exp_1, pcr_data$PCR)
  auc_exp_1 = performance(pred_exp_1,"auc")@y.values[[1]][1]
  B_exp[i, 1] = auc_exp_1
  
  # apply model to bootstrap data
  prob_exp_2 = predict(logit.boot_exp, data.boot_exp, type='response')
  pred_exp_2 = prediction(prob_exp_2, data.boot_exp$PCR)
  auc_exp_2 = performance(pred_exp_2,"auc")@y.values[[1]][1]
  B_exp[i, 2] = auc_exp_2
  
  opt_exp = (auc_exp_2-auc_exp_1)
  B_exp[i, 3] = opt_exp
}

is.matrix(B_exp)
boot_data_exp <- as.data.frame(B_exp) # converting matrix to data frame
is.data.frame(boot_data_exp)

summary(boot_data_exp)
quantile(boot_data_exp$opt, p=0.025, p=0.975)
quantile(boot_data_exp$opt, p=0.975)

opt_auc_exp <- (0.7226 - 0.0512)
opt_auc_exp

############################# All variables logistic model

logit_all <- glm(PCR ~ Pt_Age_at_Sx + BCS_Candidate + Definitive_Sx_Simple + LN_Sx_Type + Surgery_Side + Clinical_LN_Status_at_Dx + Zone_Tx +
                   chemo + rad + hormone + Herceptin_Clinical_response + T_Stage__Largest_ +
                   ER_Positive + PR_Positive + HER2_Positive + Specify_Surgery,
                   data=pcr_data, family="binomial") #binomial for logistic regression
summary(logit_all)

exp(cbind(OR = coef(logit_all), confint(logit_all))) #exponentiating regression coefficients and 95% CIs

pred_all <- predict(logit_all, pcr_data, type='response') #calculating probability predictions
head(pred_all)

# Using pROC to calculate 95% CI of AUC
roc_all <- roc(pcr_data$PCR, pred_all, plot=T, legacy.axes=T)
auc(roc_all)
ci(roc_all)

#Using ROCR to generate ROC curve
pred_all <- prediction(pred_all, pcr_data$PCR)
perf_all <- performance(pred_all, "tpr", "fpr") #ROC curve
plot(perf_all, colorize = T, ylab = "Sensitivity", xlab = "1 - Specificity") #colorize T shows p cut-offs
abline(a=0, b=1) #adds straight line

#Calculating AUC
auc_all <- performance(pred_all, "auc")
auc_all <- unlist(slot(auc_all, "y.values"))
auc_all <- round(auc_all, 4) #rounding AUC to 4 decimal places
legend(0.6, 0.3, auc_all, title = "AUC", cex = 0.8) #adding AUC to ROC curve

# Bootstrap internal validation

R = 200
n = nrow(pcr_data)

# empty Rx2 matrix for bootstrap results 
B_all = matrix(nrow = R, ncol = 3,
               dimnames = list(paste('Sample',1:R),
                               c("auc_orig","auc_boot", "opt")))

set.seed(701)
for(i in 1:R){
  
  # draw a random sample
  obs.boot_all <- sample(x = 1:n, size = n, replace = T)
  data.boot_all <- pcr_data[obs.boot_all, ]
  
  # fit the model on bootstrap sample
  logit.boot_all <- glm(PCR ~ Pt_Age_at_Sx + BCS_Candidate + Definitive_Sx_Simple + LN_Sx_Type + Surgery_Side + Clinical_LN_Status_at_Dx + Zone_Tx +
                          chemo + rad + hormone + Herceptin_Clinical_response + T_Stage__Largest_ +
                          ER_Positive + PR_Positive + HER2_Positive + Specify_Surgery,
                          data=data.boot_all, family = "binomial")
  
  # apply model to original data
  prob_all_1 = predict(logit.boot_all, pcr_data, type='response')
  pred_all_1 = prediction(prob_all_1, pcr_data$PCR)
  auc_all_1 = performance(pred_all_1,"auc")@y.values[[1]][1]
  B_all[i, 1] = auc_all_1
  
  # apply model to bootstrap data
  prob_all_2 = predict(logit.boot_all, data.boot_all, type='response')
  pred_all_2 = prediction(prob_all_2, data.boot_all$PCR)
  auc_all_2 = performance(pred_all_2,"auc")@y.values[[1]][1]
  B_all[i, 2] = auc_all_2
  
  opt_all = (auc_all_2-auc_all_1)
  B_all[i, 3] = opt_all
}

is.matrix(B_all)
boot_data_all <- as.data.frame(B_all) # converting matrix to data frame
is.data.frame(boot_data_all)

summary(boot_data_all)
opt_auc_all <- (0.7576 - 0.07162)
opt_auc_all

############################# Lasso logistic model

install.packages("glmnet")
library(glmnet)

# Lasso only worked when a matrix was created
pcr_pred_mat <- model.matrix(PCR ~ Pt_Age_at_Sx + BCS_Candidate + Definitive_Sx_Simple + LN_Sx_Type + Surgery_Side + Clinical_LN_Status_at_Dx + Zone_Tx +
                             chemo + rad + hormone + Herceptin_Clinical_response + T_Stage__Largest_ +
                             ER_Positive + PR_Positive + HER2_Positive + Specify_Surgery, data=pcr_data)[,-1] # Initial T size and Diagnosis Uni were excluded because it cannot handled NAs
View(pcr_pred_mat)

# Using CV to determine optimal lambda value
set.seed(123)
cv.lasso_logit <- cv.glmnet(pcr_pred_mat, pcr_data$PCR, alpha = 1, family ="binomial")
cv.lasso_logit$lambda.1se #Optimal lambda is 0.4536

# Running Lasso logistic regression model with optimal lambda
lasso_logit <- glmnet(pcr_pred_mat, pcr_data$PCR, alpha = 1, family ="binomial", lambda = cv.lasso_logit$lambda.1se)
lasso_logit
coefficients(lasso_logit)

# Calculating probability predictions for pCR from lasso model
prob_lasso = predict(lasso_logit, newx=pcr_pred_mat, type='response')
prob_lasso <- as.vector(prob_lasso) # converting probability prediction matrix to a vector

# Using pROC to calculate 95% CI of AUC
roc_lasso <- roc(pcr_data$PCR, prob_lasso, plot=T, legacy.axes=T)
auc(roc_lasso)
ci(roc_lasso)

# Using ROCR to generate ROC curve
prob_lasso <- prediction(prob_lasso, pcr_data$PCR)
perf_lasso <- performance(prob_lasso, "tpr", "fpr") #ROC curve
plot(perf_lasso, colorize = T, ylab = "Sensitivity", xlab = "1 - Specificity") #colorize T shows p cut-offs
abline(a=0, b=1) #adds straight line

# Calculating AUC
auc_lasso <- performance(prob_lasso, "auc")
auc_lasso <- unlist(slot(auc_lasso, "y.values"))
auc_lasso <- round(auc_lasso, 4) #rounding AUC to 4 decimal places
legend(0.6, 0.3, auc_lasso, title = "AUC", cex = 0.8) #adding AUC to ROC curve


# Bootstrap internal validation

R = 200
n = nrow(pcr_data)

# empty Rx2 matrix for bootstrap results 
B_lasso = matrix(nrow = R, ncol = 3,
               dimnames = list(paste('Sample',1:R),
                               c("auc_orig","auc_boot", "opt")))

set.seed(701)
for(i in 1:R){
  
  # draw a random sample
  obs.boot_lasso <- sample(x = 1:n, size = n, replace = T)
  data.boot_lasso <- pcr_data[obs.boot_lasso, ]
  

  # Using CV to determine optimal lambda in bootstrap sample. Could not set seed because it resulted in the same AUC in each sample.
  boot.cv.lasso_logit <- cv.glmnet(model.matrix(PCR ~ Pt_Age_at_Sx + BCS_Candidate + Definitive_Sx_Simple + LN_Sx_Type + Surgery_Side + Clinical_LN_Status_at_Dx + Zone_Tx +
                                                  chemo + rad + hormone + Herceptin_Clinical_response + T_Stage__Largest_ +
                                                  ER_Positive + PR_Positive + HER2_Positive + Specify_Surgery, data=data.boot_lasso)[,-1] , data.boot_lasso$PCR, alpha = 1, family ="binomial")
  
  #Fitting model to bootstrap sample
  boot.lasso_logit <- glmnet(model.matrix(PCR ~ Pt_Age_at_Sx + BCS_Candidate + Definitive_Sx_Simple + LN_Sx_Type + Surgery_Side + Clinical_LN_Status_at_Dx + Zone_Tx +
                                            chemo + rad + hormone + Herceptin_Clinical_response + T_Stage__Largest_ +
                                            ER_Positive + PR_Positive + HER2_Positive + Specify_Surgery, data=data.boot_lasso)[,-1] , data.boot_lasso$PCR, alpha = 1, family ="binomial", lambda = boot.cv.lasso_logit$lambda.1se)
  
  # apply model to original data
  prob_lasso_1 = predict(boot.lasso_logit, newx=model.matrix(PCR ~ Pt_Age_at_Sx + BCS_Candidate + Definitive_Sx_Simple + LN_Sx_Type + Surgery_Side + Clinical_LN_Status_at_Dx + Zone_Tx +
                                                               chemo + rad + hormone + Herceptin_Clinical_response + T_Stage__Largest_ +
                                                               ER_Positive + PR_Positive + HER2_Positive + Specify_Surgery, data=pcr_data)[,-1] , type='response')
  prob_lasso_1 <- as.vector(prob_lasso_1)
  pred_lasso_1 = prediction(prob_lasso_1, pcr_data$PCR)
  auc_lasso_1 = performance(pred_lasso_1,"auc")@y.values[[1]][1]
  B_lasso[i, 1] = auc_lasso_1
  
  # apply model to bootstrap data
  prob_lasso_2 = predict(boot.lasso_logit, newx=model.matrix(PCR ~ Pt_Age_at_Sx + BCS_Candidate + Definitive_Sx_Simple + LN_Sx_Type + Surgery_Side + Clinical_LN_Status_at_Dx + Zone_Tx +
                                                               chemo + rad + hormone + Herceptin_Clinical_response + T_Stage__Largest_ +
                                                               ER_Positive + PR_Positive + HER2_Positive + Specify_Surgery, data=data.boot_lasso)[,-1] , type='response')
  prob_lasso_2 <- as.vector(prob_lasso_2)
  pred_lasso_2 = prediction(prob_lasso_2, data.boot_lasso$PCR)
  auc_lasso_2 = performance(pred_lasso_2,"auc")@y.values[[1]][1]
  B_lasso[i, 2] = auc_lasso_2
  
  opt_lasso = (auc_lasso_2-auc_lasso_1)
  B_lasso[i, 3] = opt_lasso
}

is.matrix(B_lasso)
boot_data_lasso <- as.data.frame(B_lasso) # converting matrix to data frame
is.data.frame(boot_data_lasso)

summary(boot_data_lasso)

opt_auc_lasso <- (0.7292 - 0.04943)
opt_auc_lasso


############################# Lit Review Support Vector Machine

install.packages("e1071") #installing SVM package 
library(e1071)

# Try making data frame of only relevant variables. Tuning function wouldn't work.
svm_data_LR <- pcr_data[,c(2:6, 18, 26)]


# Using 10-fold CV for determine optimal cost violation and gamma. 

costlist <- 2^(0:7) # creating a list of different costs, applied to 'exp' and 'all' svm models
gammalist <- seq(0.1, 1, 0.1) # creating a list of different gammas, applied to 'exp' and 'all' svm models

set.seed (420) 
tune_LR <- tune(svm, PCR~., data=svm_data_LR, kernel="radial", scale = F,
                ranges=list(cost=costlist,
                            gamma=gammalist))

summary(tune_LR) #Optimal cost was 0.1, optimal gamma was 0.1

# Storing cost and gamme into the following values. 
svm_LR_cost <- tune_LR$best.parameters #makes data frame of cost and gamma from tuning procedure
svm_LR_cost <- svm_LR_cost[,c(1)] #storing optimal cost value, which is the first column
svm_LR_gamma <- tune_LR$best.parameters #makes data frame of cost and gamma from tuning procedure
svm_LR_gamma <- svm_LR_gamma[,c(2)] #storing optimal gamma value, which is the second column

#SVM model, Probablility = T to allow for probability predictions
svm_LR <- svm(PCR ~., data=svm_data_LR, kernel = "radial", cost = svm_LR_cost, gamma = svm_LR_gamma, scale = F, probability=T)
summary(svm_LR)

# Prediction outputs are class predictions, probability probability predictions must be extracted from attributes
pred_svm_LR <- predict(svm_LR, svm_data_LR, probability = TRUE)
pred_svm_LR <- attr(pred_svm_LR, "probabilities")
pred_svm_LR <- pred_svm_LR[,c(2)]

# Using pROC to calculate 95% CI of AUC
roc_svm_LR <- roc(svm_data_LR$PCR, pred_svm_LR, plot=T, legacy.axes=T)
auc(roc_svm_LR)
ci(roc_svm_LR)

#Using ROCR to generate ROC curve
pred_svm_LR <- prediction(pred_svm_LR, svm_data_LR$PCR)
perf_svm_LR <- performance(pred_svm_LR, "tpr", "fpr") #ROC curve
plot(perf_svm_LR, colorize = T, ylab = "Sensitivity", xlab = "1 - Specificity") #colorize T shows p cut-offs
abline(a=0, b=1) #adds straight line

#Calculating AUC
auc_svm_LR <- performance(pred_svm_LR, "auc")
auc_svm_LR <- unlist(slot(auc_svm_LR, "y.values"))
auc_svm_LR <- round(auc_svm_LR, 4) #rounding AUC to 4 decimal places
legend(0.6, 0.3, auc_svm_LR, title = "AUC", cex = 0.8) #adding AUC to ROC curve

# Bootstrap internal validation

R = 200
n = nrow(svm_data_LR)

# empty Rx2 matrix for bootstrap results 
B_svm_LR = matrix(nrow = R, ncol = 3,
               dimnames = list(paste('Sample',1:R),
                               c("auc_orig","auc_boot", "opt")))

set.seed(701)
for(i in 1:R){
  
  # draw a random sample
  obs.boot_svm_LR <- sample(x = 1:n, size = n, replace = T)
  data.boot_svm_LR <- svm_data_LR[obs.boot_svm_LR, ]

  # fit the model on bootstrap sample
  svm.boot_LR <- svm(PCR ~., data=data.boot_svm_LR, kernel = "radial",cost = svm_LR_cost, gamma = svm_LR_gamma, scale = F, probability=T)
  
  # apply model to original data
  prob_svm_LR_1 = predict(svm.boot_LR, svm_data_LR, probability = T)
  prob_svm_LR_1 = attr(prob_svm_LR_1, "probabilities")
  prob_svm_LR_1 = prob_svm_LR_1[,c(2)]
  pred_svm_LR_1 = prediction(prob_svm_LR_1, svm_data_LR$PCR)
  auc_svm_LR_1 = performance(pred_svm_LR_1,"auc")@y.values[[1]][1]
  B_svm_LR[i, 1] = auc_svm_LR_1
  
  # apply model to bootstrap data
  prob_svm_LR_2 = predict(svm.boot_LR, data.boot_svm_LR, probability = T)
  prob_svm_LR_2 = attr(prob_svm_LR_2, "probabilities")
  prob_svm_LR_2 = prob_svm_LR_2[,c(2)]
  pred_svm_LR_2 = prediction(prob_svm_LR_2, data.boot_svm_LR$PCR)
  auc_svm_LR_2 = performance(pred_exp_2,"auc")@y.values[[1]][1]
  B_svm_LR[i, 2] = auc_svm_LR_2
  
  opt_svm_LR = (auc_svm_LR_2-auc_svm_LR_1)
  B_svm_LR[i, 3] = opt_svm_LR
}

is.matrix(B_svm_LR)
boot_data_svm_LR <- as.data.frame(B_svm_LR) # converting matrix to data frame
is.data.frame(boot_data_svm_LR)

summary(boot_data_svm_LR)

opt_auc_svm_LR <- (0.9378 - 0.0158)
opt_auc_svm_LR

############################# Expert Support Vector Machine

#Try making data frame of only relevant variables. Tuning function wouldn't work.
svm_data_exp <- pcr_data[,c(2:6, 18, 24:26)]

#Using 10-fold CV for determine optimal cost violation and gamma. 

set.seed (420) 
tune_exp <- tune(svm, PCR~., data=svm_data_exp, kernel="radial", scale = F,
                    ranges=list(cost=costlist,
                                gamma=gammalist))
summary(tune_exp) #Optimal cost was 0.1, optimal gamma was 0.1.

# Storing cost and gamme into the following values. 
svm_exp_cost <- tune_exp$best.parameters #makes data frame of cost and gamma from tuning procedure
svm_exp_cost <- svm_exp_cost[,c(1)] #storing optimal cost value, which is the first column
svm_exp_gamma <- tune_exp$best.parameters #makes data frame of cost and gamma from tuning procedure
svm_exp_gamma <- svm_exp_gamma[,c(2)] #storing optimal gamma value, which is the second column


#SVM model, Probablility = T to allow for probability predictions
svm_exp <- svm(PCR ~., data=svm_data_exp, kernel = "radial", cost = svm_exp_cost, gamma = svm_exp_gamma, scale = F, probability = T)
summary(svm_exp)

# Prediction outputs are class predictions, probability probability predictions must be extracted from attributes
pred_svm_exp <- predict(svm_exp, svm_data_exp, probability = T)
pred_svm_exp <- attr(pred_svm_exp, "probabilities")
pred_svm_exp <- pred_svm_exp[,c(2)] #pCR predictions are in the 2nd column

# Using pROC to calculate 95% CI of AUC
roc_svm_exp <- roc(svm_data_exp$PCR, pred_svm_exp, plot=T, legacy.axes=T)
auc(roc_svm_exp)
ci(roc_svm_exp)

#Using ROCR to generate ROC curve
pred_svm_exp <- prediction(pred_svm_exp, svm_data_exp$PCR)
perf_svm_exp <- performance(pred_svm_exp, "tpr", "fpr") #ROC curve
plot(perf_svm_exp, colorize = T, ylab = "Sensitivity", xlab = "1 - Specificity") #colorize T shows p cut-offs
abline(a=0, b=1) #adds straight line

#Calculating AUC
auc_svm_exp <- performance(pred_svm_exp, "auc")
auc_svm_exp <- unlist(slot(auc_svm_exp, "y.values"))
auc_svm_exp <- round(auc_svm_exp, 4) #rounding AUC to 4 decimal places
legend(0.6, 0.3, auc_svm_exp, title = "AUC", cex = 0.8) #adding AUC to ROC curve

# Bootstrap internal validation

R = 200
n = nrow(svm_data_exp)

# empty Rx2 matrix for bootstrap results 
B_svm_exp = matrix(nrow = R, ncol = 3,
                  dimnames = list(paste('Sample',1:R),
                                  c("auc_orig","auc_boot", "opt")))

set.seed(701)
for(i in 1:R){
  
  # draw a random sample
  obs.boot_svm_exp <- sample(x = 1:n, size = n, replace = T)
  data.boot_svm_exp <- svm_data_exp[obs.boot_svm_exp, ]
  
  # fit the model on bootstrap sample
  svm.boot_exp <- svm(PCR ~., data=data.boot_svm_exp, kernel = "radial", cost = svm_exp_cost, gamma = svm_exp_gamma, scale = F, probability=T)
  
  # apply model to original data
  prob_svm_exp_1 = predict(svm.boot_exp, svm_data_exp, probability = T)
  prob_svm_exp_1 = attr(prob_svm_exp_1, "probabilities")
  prob_svm_exp_1 = prob_svm_exp_1[,c(2)]
  pred_svm_exp_1 = prediction(prob_svm_exp_1, svm_data_exp$PCR)
  auc_svm_exp_1 = performance(pred_svm_exp_1,"auc")@y.values[[1]][1]
  B_svm_exp[i, 1] = auc_svm_exp_1
  
  # apply model to bootstrap data
  prob_svm_exp_2 = predict(svm.boot_exp, data.boot_svm_exp, probability = T)
  prob_svm_exp_2 = attr(prob_svm_exp_2, "probabilities")
  prob_svm_exp_2 = prob_svm_exp_2[,c(2)]
  pred_svm_exp_2 = prediction(prob_svm_exp_2, data.boot_svm_exp$PCR)
  auc_svm_exp_2 = performance(pred_exp_2,"auc")@y.values[[1]][1]
  B_svm_exp[i, 2] = auc_svm_exp_2
  
  opt_svm_exp = (auc_svm_exp_2-auc_svm_exp_1)
  B_svm_exp[i, 3] = opt_svm_exp
}

is.matrix(B_svm_exp)
boot_data_svm_exp <- as.data.frame(B_svm_exp) # converting matrix to data frame
is.data.frame(boot_data_svm_exp)

summary(boot_data_svm_exp)

opt_auc_svm_exp <- (0.9725 - 0.024)
opt_auc_svm_exp

############################# All variables Support Vector Machine

#Try making data frame of only relevant variables. Tuning function wouldn't work.
svm_data_all <- pcr_data[,c(2:7, 15:18, 21:27)]

set.seed (420) #Using 10-fold CV for determine optimal cost violation and gamma. 
tune_all <- tune(svm, PCR~., data=svm_data_all, kernel="radial", scale = F,
                 ranges=list(cost=costlist,
                             gamma=gammalist))
summary(tune_all) #Optimal cost was 1, optimal gamma was 0.8.

# Storing cost and gamme into the following values. 
svm_all_cost <- tune_all$best.parameters #makes data frame of cost and gamma from tuning procedure
svm_all_cost <- svm_all_cost[,c(1)] #storing optimal cost value, which is the first column
svm_all_gamma <- tune_all$best.parameters #makes data frame of cost and gamma from tuning procedure
svm_all_gamma <- svm_all_gamma[,c(2)] #storing optimal gamma value, which is the second column

#SVM model, Probablility = T to allow for probability predictions
svm_all <- svm(PCR ~., data=svm_data_all, kernel = "radial", scale = F, probability = T)
summary(svm_all)

# Prediction outputs are class predictions, probability probability predictions must be extracted from attributes
pred_svm_all <- predict(svm_all, svm_data_all, probability = T)
pred_svm_all <- attr(pred_svm_all, "probabilities")
pred_svm_all <- pred_svm_all[,c(2)] #pCR predictions are in the 2nd column

# Using pROC to calculate 95% CI of AUC
roc_svm_all <- roc(svm_data_all$PCR, pred_svm_all, plot=T, legacy.axes=T)
auc(roc_svm_all)
ci(roc_svm_all)

#Using ROCR to generate ROC curve
pred_svm_all <- prediction(pred_svm_all, svm_data_all$PCR)
perf_svm_all <- performance(pred_svm_all, "tpr", "fpr") #ROC curve
plot(perf_svm_all, colorize = T, ylab = "Sensitivity", xlab = "1 - Specificity") #colorize T shows p cut-offs
abline(a=0, b=1) #adds straight line

#Calculating AUC
auc_svm_all <- performance(pred_svm_all, "auc")
auc_svm_all <- unlist(slot(auc_svm_all, "y.values"))
auc_svm_all <- round(auc_svm_all, 4) #rounding AUC to 4 decimal places
legend(0.6, 0.3, auc_svm_all, title = "AUC", cex = 0.8) #adding AUC to ROC curve

# Bootstrap internal validation

R = 200
n = nrow(svm_data_all)

# empty Rx2 matrix for bootstrap results 
B_svm_all = matrix(nrow = R, ncol = 3,
                   dimnames = list(paste('Sample',1:R),
                                   c("auc_orig","auc_boot", "opt")))

set.seed(701)
for(i in 1:R){
  
  # draw a random sample
  obs.boot_svm_all <- sample(x = 1:n, size = n, replace = T)
  data.boot_svm_all <- svm_data_all[obs.boot_svm_all, ]
  
  # fit the model on bootstrap sample
  svm.boot_all <- svm(PCR ~., data=data.boot_svm_all, kernel = "radial", cost = svm_all_cost, gamma = svm_all_gamma, scale = F, probability=T)
  
  # apply model to original data
  prob_svm_all_1 = predict(svm.boot_all, svm_data_all, probability = T)
  prob_svm_all_1 = attr(prob_svm_all_1, "probabilities")
  prob_svm_all_1 = prob_svm_all_1[,c(2)]
  pred_svm_all_1 = prediction(prob_svm_all_1, svm_data_all$PCR)
  auc_svm_all_1 = performance(pred_svm_all_1,"auc")@y.values[[1]][1]
  B_svm_all[i, 1] = auc_svm_all_1
  
  # apply model to bootstrap data
  prob_svm_all_2 = predict(svm.boot_all, data.boot_svm_all, probability = T)
  prob_svm_all_2 = attr(prob_svm_all_2, "probabilities")
  prob_svm_all_2 = prob_svm_all_2[,c(2)]
  pred_svm_all_2 = prediction(prob_svm_all_2, data.boot_svm_all$PCR)
  auc_svm_all_2 = performance(pred_all_2,"auc")@y.values[[1]][1]
  B_svm_all[i, 2] = auc_svm_all_2
  
  opt_svm_all = (auc_svm_all_2-auc_svm_all_1)
  B_svm_all[i, 3] = opt_svm_all
}

is.matrix(B_svm_all)
boot_data_svm_all <- as.data.frame(B_svm_all) # converting matrix to data frame
is.data.frame(boot_data_svm_all)

summary(boot_data_svm_all)

opt_auc_svm_all <- (0.9335 - 0.0774)
opt_auc_svm_all


