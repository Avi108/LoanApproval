# Download the data set as german_credit
library(class)
library(car)
library(caTools)
library(Hmisc)
library(ROCR)
library(caret)
library(GGally)
library(MASS)
library(e1071)
german_credit <- read.csv("german.csv")
# Data prpeapartion and feature transformation
sum(is.na(german_credit))
german_credit$Default_status <- as.factor(german_credit$Default_status)
german_credit$Number.of.existing.credits.at.this.bank. <- as.factor(german_credit$Number.of.existing.credits.at.this.bank.)
german_credit$Number.of.people.being.liable.to.provide.maintenance.for. <- as.factor(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.)
german_credit$Present.residence.since  <- as.factor(german_credit$Present.residence.since )
german_credit$Installment.rate.in.percentage.of.disposable.income  <- as.factor(german_credit$Installment.rate.in.percentage.of.disposable.income)
dummy1 <- as.data.frame(model.matrix( ~ Status.of.existing.checking.account -1, data = german_credit))
dummy2 <- as.data.frame(model.matrix( ~ Credit.history -1, data = german_credit))
dummy3 <- as.data.frame(model.matrix( ~ Purpose -1, data = german_credit))
dummy4 <- as.data.frame(model.matrix( ~ Savings.account.bonds -1, data = german_credit))
dummy5 <- as.data.frame(model.matrix( ~ Present.employment.since. -1, data = german_credit))
dummy7 <- as.data.frame(model.matrix( ~ Installment.rate.in.percentage.of.disposable.income -1, data = german_credit))
dummy8 <- as.data.frame(model.matrix( ~ Personal.status.and.sex -1, data = german_credit))
dummy9 <- as.data.frame(model.matrix( ~ Other.debtors...guarantors -1, data = german_credit))
dummy10 <- as.data.frame(model.matrix( ~ Present.residence.since -1, data = german_credit))
dummy11 <- as.data.frame(model.matrix( ~ Property -1, data = german_credit))
dummy12 <- as.data.frame(model.matrix( ~ Other.installment.plans -1, data = german_credit))
dummy13 <- as.data.frame(model.matrix( ~ Housing. -1, data = german_credit))
dummy14 <- as.data.frame(model.matrix( ~ Number.of.existing.credits.at.this.bank. -1, data = german_credit))
dummy15 <- as.data.frame(model.matrix( ~ Job_status -1, data = german_credit))

master <- cbind(dummy1, dummy2, dummy3, dummy4, dummy5, dummy7, dummy8, dummy9, dummy10, dummy11, dummy12, dummy13, dummy14, dummy15, german_credit[, c(2, 5, 13, 18, 19, 20, 21)])

quantile(master$Age.in.Years, prob=seq(0,1,0.01))
quantile(master$Duration.in.month, prob=seq(0,1,0.01))
master$Duration.in.month[master$Duration.in.month > 48.00] <- 48.00
quantile(master$Credit.amount, prob=seq(0,1,0.01))
master$Credit.amount[master$Credit.amount > 7985.95] <- 7985.95

master$Age.in.Years <- sqrt(master$Age.in.Years)
master$Duration.in.month <- sqrt(master$Duration.in.month)
master$Credit.amount <- sqrt(master$Credit.amount)

master$Age.in.Years <- scale(master$Age.in.Years)
master$Duration.in.month <- scale(master$Duration.in.month)
master$Credit.amount <- scale(master$Credit.amount)
# Exploratory Data Analysis
pairs(master[, c(63, 64, 65)])
# Initial Model with all variables

set.seed(100)
split_indices <- sample.split(master$Default_status, SplitRatio = 0.70)
train <- master[split_indices == T, ]
test <- master[split_indices == F, ]
initial_model <- glm(Default_status~ . - Credit.amount - Duration.in.month, family = "binomial", data = train)
summary(initial_model)

# Stepwise selection
best_model <- stepAIC(initial_model, direction = "both")

best_model
summary(best_model)
vif(best_model)

model_1 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                Status.of.existing.checking.accountA12 +Status.of.existing.checking.accountA13 + 
                Credit.historyA30 + Credit.historyA31 + Credit.historyA32 + 
                PurposeA40 + PurposeA41 + PurposeA46 + Savings.account.bondsA61 + 
                Savings.account.bondsA64 + Present.employment.since.A74 + 
                Installment.rate.in.percentage.of.disposable.income1 + Installment.rate.in.percentage.of.disposable.income2 + 
                Personal.status.and.sexA93 + Other.debtors...guarantorsA101 + 
                Other.debtors...guarantorsA102 + Present.residence.since2 + 
                Other.installment.plansA141 + Housing.A151 + Duration.in.month + 
                foreign.worker + Credit.amount + Duration.in.month, family = "binomial", data = train)

summary(model_1)
vif(model_1)

#Credit.amount

model_2 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                Status.of.existing.checking.accountA12 +Status.of.existing.checking.accountA13 + 
                Credit.historyA30 + Credit.historyA31 + Credit.historyA32 + 
                PurposeA40 + PurposeA41 + PurposeA46 + Savings.account.bondsA61 + 
                Savings.account.bondsA64 + Present.employment.since.A74 + 
                Installment.rate.in.percentage.of.disposable.income1 + Installment.rate.in.percentage.of.disposable.income2 + 
                Personal.status.and.sexA93 + Other.debtors...guarantorsA101 + 
                Other.debtors...guarantorsA102 + Present.residence.since2 + 
                Other.installment.plansA141 + Housing.A151 + Duration.in.month + 
                foreign.worker+ Duration.in.month, family = "binomial", data = train)

summary(model_2)
vif(model_2)

#Other.debtors...guarantorsA101

model_3 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                Status.of.existing.checking.accountA12 +Status.of.existing.checking.accountA13 + 
                Credit.historyA30 + Credit.historyA31 + Credit.historyA32 + 
                PurposeA40 + PurposeA41 + PurposeA46 + Savings.account.bondsA61 + 
                Savings.account.bondsA64 + Present.employment.since.A74 + 
                Installment.rate.in.percentage.of.disposable.income1 + Installment.rate.in.percentage.of.disposable.income2 + 
                Personal.status.and.sexA93 + 
                Other.debtors...guarantorsA102 + Present.residence.since2 + 
                Other.installment.plansA141 + Housing.A151 + Duration.in.month + 
                foreign.worker+ Duration.in.month, family = "binomial", data = train)

summary(model_3)
vif(model_3)

#Status.of.existing.checking.accountA13

model_4 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                Status.of.existing.checking.accountA12 +
                Credit.historyA30 + Credit.historyA31 + Credit.historyA32 + 
                PurposeA40 + PurposeA41 + PurposeA46 + Savings.account.bondsA61 + 
                Savings.account.bondsA64 + Present.employment.since.A74 + 
                Installment.rate.in.percentage.of.disposable.income1 + Installment.rate.in.percentage.of.disposable.income2 + 
                Personal.status.and.sexA93 + 
                Other.debtors...guarantorsA102 + Present.residence.since2 + 
                Other.installment.plansA141 + Housing.A151 + Duration.in.month + 
                foreign.worker+ Duration.in.month, family = "binomial", data = train)

summary(model_4)
vif(model_4)

#Credit.historyA32 

model_5 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                Status.of.existing.checking.accountA12 +
                Credit.historyA30 + Credit.historyA31 + 
                PurposeA40 + PurposeA41 + PurposeA46 + Savings.account.bondsA61 + 
                Savings.account.bondsA64 + Present.employment.since.A74 + 
                Installment.rate.in.percentage.of.disposable.income1 + Installment.rate.in.percentage.of.disposable.income2 + 
                Personal.status.and.sexA93 + 
                Other.debtors...guarantorsA102 + Present.residence.since2 + 
                Other.installment.plansA141 + Housing.A151 + Duration.in.month + 
                foreign.worker+ Duration.in.month, family = "binomial", data = train)

summary(model_5)
vif(model_5)

#PurposeA41
model_6 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                Status.of.existing.checking.accountA12 +
                Credit.historyA30 + Credit.historyA31 + 
                PurposeA40 +  PurposeA46 + Savings.account.bondsA61 + 
                Savings.account.bondsA64 + Present.employment.since.A74 + 
                Installment.rate.in.percentage.of.disposable.income1 + Installment.rate.in.percentage.of.disposable.income2 + 
                Personal.status.and.sexA93 + 
                Other.debtors...guarantorsA102 + Present.residence.since2 + 
                Other.installment.plansA141 + Housing.A151 + Duration.in.month + 
                foreign.worker+ Duration.in.month, family = "binomial", data = train)

summary(model_6)
vif(model_6)

#Other.debtors...guarantorsA102  
model_7 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                Status.of.existing.checking.accountA12 +
                Credit.historyA30 + Credit.historyA31 + 
                PurposeA40 +  PurposeA46 + Savings.account.bondsA61 + 
                Savings.account.bondsA64 + Present.employment.since.A74 + 
                Installment.rate.in.percentage.of.disposable.income1 + Installment.rate.in.percentage.of.disposable.income2 + 
                Personal.status.and.sexA93 + 
                Present.residence.since2 + 
                Other.installment.plansA141 + Housing.A151 + Duration.in.month + 
                foreign.worker+ Duration.in.month, family = "binomial", data = train)

summary(model_7)
vif(model_7)

#Savings.account.bondsA64  
model_8 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                Status.of.existing.checking.accountA12 +
                Credit.historyA30 + Credit.historyA31 + 
                PurposeA40 +  PurposeA46 + Savings.account.bondsA61 + 
                Present.employment.since.A74 + 
                Installment.rate.in.percentage.of.disposable.income1 + Installment.rate.in.percentage.of.disposable.income2 + 
                Personal.status.and.sexA93 + 
                Present.residence.since2 + 
                Other.installment.plansA141 + Housing.A151 + Duration.in.month + 
                foreign.worker+ Duration.in.month, family = "binomial", data = train)

summary(model_8)
vif(model_8)

#Housing.A151
model_9 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                Status.of.existing.checking.accountA12 +
                Credit.historyA30 + Credit.historyA31 + 
                PurposeA40 +  PurposeA46 + Savings.account.bondsA61 + 
                Present.employment.since.A74 + 
                Installment.rate.in.percentage.of.disposable.income1 + Installment.rate.in.percentage.of.disposable.income2 + 
                Personal.status.and.sexA93 + 
                Present.residence.since2 + 
                Other.installment.plansA141 +  Duration.in.month + 
                foreign.worker+ Duration.in.month, family = "binomial", data = train)

summary(model_9)
vif(model_9)

#Installment.rate.in.percentage.of.disposable.income1
model_10 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                Status.of.existing.checking.accountA12 +
                Credit.historyA30 + Credit.historyA31 + 
                PurposeA40 +  PurposeA46 + Savings.account.bondsA61 + 
                Present.employment.since.A74 + 
                Installment.rate.in.percentage.of.disposable.income2 + 
                Personal.status.and.sexA93 + 
                Present.residence.since2 + 
                Other.installment.plansA141 +  Duration.in.month + 
                foreign.worker+ Duration.in.month, family = "binomial", data = train)

summary(model_10)
vif(model_10)

#foreign.workerA202
model_11 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                 Status.of.existing.checking.accountA12 +
                 Credit.historyA30 + Credit.historyA31 + 
                 PurposeA40 +  PurposeA46 + Savings.account.bondsA61 + 
                 Present.employment.since.A74 + 
                 Installment.rate.in.percentage.of.disposable.income2 + 
                 Personal.status.and.sexA93 + 
                 Present.residence.since2 + 
                 Other.installment.plansA141 +  Duration.in.month + 
                  Duration.in.month, family = "binomial", data = train)

summary(model_11)
vif(model_11)

#Other.installment.plansA141
model_12 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                 Status.of.existing.checking.accountA12 +
                 Credit.historyA30 + Credit.historyA31 + 
                 PurposeA40 +  PurposeA46 + Savings.account.bondsA61 + 
                 Present.employment.since.A74 + 
                 Installment.rate.in.percentage.of.disposable.income2 + 
                 Personal.status.and.sexA93 + 
                 Present.residence.since2 + 
                 Duration.in.month , family = "binomial", data = train)

summary(model_12)
vif(model_12)

#Present.employment.since.A74
model_13 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                 Status.of.existing.checking.accountA12 +
                 Credit.historyA30 + Credit.historyA31 + 
                 PurposeA40 +  PurposeA46 + Savings.account.bondsA61 + 
                 Installment.rate.in.percentage.of.disposable.income2 + 
                 Personal.status.and.sexA93 + 
                 Present.residence.since2 + 
                 Duration.in.month , family = "binomial", data = train)

summary(model_13)
vif(model_13)

#Installment.rate.in.percentage.of.disposable.income2
model_14 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                 Status.of.existing.checking.accountA12 +
                 Credit.historyA30 + Credit.historyA31 + 
                 PurposeA40 +  PurposeA46 + Savings.account.bondsA61 + 
                 Personal.status.and.sexA93 + 
                 Present.residence.since2 + 
                 Duration.in.month , family = "binomial", data = train)

summary(model_14)
vif(model_14)

#Present.residence.since2
model_15 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                 Status.of.existing.checking.accountA12 +
                 Credit.historyA30 + Credit.historyA31 + 
                 PurposeA40 +  PurposeA46 + Savings.account.bondsA61 + 
                 Personal.status.and.sexA93 + 
                 Duration.in.month , family = "binomial", data = train)

summary(model_15)
vif(model_15)

#Personal.status.and.sexA93
model_16 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                 Status.of.existing.checking.accountA12 +
                 Credit.historyA30 + Credit.historyA31 + 
                 PurposeA40 +  PurposeA46 + Savings.account.bondsA61 + 
                 Duration.in.month , family = "binomial", data = train)

summary(model_16)
vif(model_16)

#Savings.account.bondsA61
model_17 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                 Status.of.existing.checking.accountA12 +
                 Credit.historyA30 + Credit.historyA31 + 
                 PurposeA40 +  PurposeA46 + 
                 Duration.in.month , family = "binomial", data = train)

summary(model_17)
vif(model_17)

#PurposeA46
model_18 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                 Status.of.existing.checking.accountA12 +
                 Credit.historyA30 + Credit.historyA31 + 
                 PurposeA40 + 
                 Duration.in.month , family = "binomial", data = train)

summary(model_18)
vif(model_18)

#PurposeA40
model_19 = glm(formula = Default_status ~ Status.of.existing.checking.accountA11 + 
                 Status.of.existing.checking.accountA12 +
                 Credit.historyA30 + Credit.historyA31 + 
                 Duration.in.month , family = "binomial", data = train)

summary(model_19)
vif(model_19)

model_final = model_19
# c-statistic and KS -statistic

predictions_data_train = predict(model_final,  type = "response")

## C-statistic
library(Hmisc)

#for train dataset
rcorr.cens(predictions_data_train, train$Default_status) # 1st argument is your vector of predicted probabilities, 2nd observed values of outcome variable

#for test dataset
predictions_data_test = predict(model_final, newdata = test,type = "response")
rcorr.cens(predictions_data_test, test$Default_status)

#KS-statistic
install.packages("ROCR")
library(ROCR)
model_score <- prediction(predictions_data_train, train$Default_status)
model_perf <- performance(model_score, "tpr", "fpr")
ks_table <- attr(model_perf, "y.values")[[1]] - (attr(model_perf, "x.values")[[1]])
ks = max(ks_table)
which(ks_table == ks)
#for train dataset
ks_table[which(ks_table == ks)]

model_score_test <- prediction(predictions_data_test, test$Default_status)
model_perf_test <- performance(model_score_test, "tpr", "fpr")
ks_table_test <- attr(model_perf_test, "y.values")[[1]] - (attr(model_perf_test, "x.values")[[1]])
ks_test = max(ks_table_test)
which(ks_table_test == ks_test)
#for test dataset
ks_table[which(ks_table_test == ks_test)]

# Selecting threshold value
# ROC curve
plot(model_perf_test,col = "red", lab = c(10,10,10))
#confusion matrix
confusionMatrix(as.numeric(predictions_data_test > 0.31),test$Default_status, positive = "1")

#area under curve
auc <- performance(model_score_test, measure = "auc")
auc <- auc@y.values[[1]]

print("Threshold value is 0.31")


