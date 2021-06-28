library(MASS)
library(pROC)
library(MuMIn)

# Read In Data ------------------------------------------------------------

questionnaire <- read.csv("thai_questionnare.csv")
questionnaire <-na.omit(questionnaire)
View(questionnaire)

#Condense Some Small Categories to Other
questionnaire$gender[questionnaire$gender == 3 | questionnaire$gender ==  4] <- 5 #code trans as other
questionnaire$sexuality[questionnaire$sexuality == 5 | questionnaire$sexuality == 6] <- 7 #code asexual and prefer.not as other 

#Turn integers into factors for categorical variables
questionnaire$gender                    <- factor(questionnaire$gender, labels = c('male', 'female', 'other'))
questionnaire$sexuality                 <- factor(questionnaire$sexuality, labels = c('hetero', 'gay', 'lesbian', 'bisexual', 'other'))
questionnaire$sex.in.3.years            <- factor(questionnaire$sex.in.3.years, labels = c('yes', 'no'))

#Sexual Activities
participate.anal    <- questionnaire$activity.perform.anal | questionnaire$activity.receive.anal
participate.vaginal <-questionnaire$activity.perform.vaginal | questionnaire$activity.receive.vaginal

questionnaire$activity.perform.anal     <- factor(questionnaire$activity.perform.anal, labels = c('no', 'yes'))
questionnaire$activity.perform.vaginal  <- factor(questionnaire$activity.perform.vaginal, labels = c('no', 'yes'))
questionnaire$activity.receive.anal     <- factor(questionnaire$activity.receive.anal, labels = c('no', 'yes'))
questionnaire$activity.receive.vaginal  <- factor(questionnaire$activity.receive.vaginal, labels = c('no', 'yes'))
questionnaire$activity.other            <- factor(questionnaire$activity.other, labels = c('no', 'yes'))
questionnaire$activity.participate.anal <- factor(participate.anal, labels = c('no', 'yes'))
questionnaire$activity.participate.vaginal <- factor(participate.vaginal, labels = c('no', 'yes'))
questionnaire$prep.user                    <- factor(questionnaire$prep.user, labels = c('yes', 'no'))

#Condom Usage
questionnaire$condom                    <- factor(questionnaire$condom, labels = c('always', 'never', 'most', 'seldom'))

#Condom Behavious
# questionnaire$behaviour.broke           <- factor(questionnaire$behaviour.broke, labels = c('no', 'yes'))
# questionnaire$behaviour.consent         <- factor(questionnaire$behaviour.consent, labels = c('no', 'yes'))
# questionnaire$behaviour.forgot          <- factor(questionnaire$behaviour.forgot, labels = c('no', 'yes'))
# questionnaire$behaviour.not.available   <- factor(questionnaire$behaviour.not.available, labels = c('no', 'yes'))
# questionnaire$behaviour.not.regular     <- factor(questionnaire$behaviour.not.regular, labels = c('no', 'yes'))
# questionnaire$behaviour.na              <- factor(questionnaire$behaviour.na, labels = c('no', 'yes'))

# #Testing Behaviour
# questionnaire$outcome.advice.test       <- factor(questionnaire$outcome.advice.test)
# questionnaire$outcome.diagnosis         <- factor(questionnaire$outcome.diagnosis)
# questionnaire$outcome.no.test           <- factor(questionnaire$outcome.no.test)
# questionnaire$outcome.test              <- factor(questionnaire$outcome.test)
# questionnaire$interested                <- factor(questionnaire$interested)
# questionnaire$HIV                       <- factor(questionnaire$HIV)

HIV.status <- ifelse(questionnaire$HIV ==2, "+", "-")
questionnaire$HIV.status <- HIV.status
####Summary Statistics
summary(questionnaire)

#For People who reported broken condom
table(questionnaire$behaviour.broke, questionnaire$gender)
table(questionnaire$behaviour.broke, questionnaire$sexuality)
table(questionnaire$behaviour.broke, questionnaire$activity.perform.anal)
table(questionnaire$behaviour.broke, questionnaire$activity.receive.anal)
table(questionnaire$behaviour.broke, questionnaire$activity.perform.vaginal)
table(questionnaire$behaviour.broke, questionnaire$activity.receive.vaginal)


round(table(questionnaire$behaviour.broke, questionnaire$sexuality)[2, ]/colSums(table(questionnaire$behaviour.broke, questionnaire$sexuality)), 2)
round(table(questionnaire$behaviour.broke, questionnaire$activity.perform.anal)[2, ]/colSums(table(questionnaire$behaviour.broke, questionnaire$activity.perform.anal)), 2)
round(table(questionnaire$behaviour.broke, questionnaire$activity.receive.anal)[2, ]/colSums(table(questionnaire$behaviour.broke, questionnaire$activity.receive.anal)), 2)
round(table(questionnaire$behaviour.broke, questionnaire$activity.perform.vaginal)[2, ]/colSums(table(questionnaire$behaviour.broke, questionnaire$activity.perform.vaginal)), 2)
round(table(questionnaire$behaviour.broke, questionnaire$activity.receive.vaginal)[2, ]/colSums(table(questionnaire$behaviour.broke, questionnaire$activity.receive.vaginal)), 2)


#For People who reported forgetting
table(questionnaire$behaviour.forgot, questionnaire$gender)
table(questionnaire$behaviour.forgot, questionnaire$sexuality)
table(questionnaire$behaviour.forgot, questionnaire$activity.perform.anal)
table(questionnaire$behaviour.forgot, questionnaire$activity.receive.anal)
table(questionnaire$behaviour.forgot, questionnaire$activity.perform.vaginal)
table(questionnaire$behaviour.forgot, questionnaire$activity.receive.vaginal)

round(table(questionnaire$behaviour.forgot, questionnaire$gender)[2, ]/colSums(table(questionnaire$behaviour.forgot, questionnaire$gender)), 2)
round(table(questionnaire$behaviour.forgot, questionnaire$sexuality)[2, ]/colSums(table(questionnaire$behaviour.forgot, questionnaire$sexuality)), 2)
round(table(questionnaire$behaviour.forgot, questionnaire$activity.perform.anal)[2, ]/colSums(table(questionnaire$behaviour.forgot, questionnaire$activity.perform.anal)), 2)
round(table(questionnaire$behaviour.forgot, questionnaire$activity.receive.anal)[2, ]/colSums(table(questionnaire$behaviour.forgot, questionnaire$activity.receive.anal)), 2)
round(table(questionnaire$behaviour.forgot, questionnaire$activity.perform.vaginal)[2, ]/colSums(table(questionnaire$behaviour.forgot, questionnaire$activity.perform.vaginal)), 2)
round(table(questionnaire$behaviour.forgot, questionnaire$activity.receive.vaginal)[2, ]/colSums(table(questionnaire$behaviour.forgot, questionnaire$activity.receive.vaginal)), 2)


#For People who reported forgetting
table(questionnaire$behaviour.forgot, questionnaire$gender)
table(questionnaire$behaviour.forgot, questionnaire$sexuality)
table(questionnaire$behaviour.forgot, questionnaire$activity.perform.anal)
table(questionnaire$behaviour.forgot, questionnaire$activity.receive.anal)
table(questionnaire$behaviour.forgot, questionnaire$activity.perform.vaginal)
table(questionnaire$behaviour.forgot, questionnaire$activity.receive.vaginal)

round(table(questionnaire$behaviour.forgot, questionnaire$gender)[2, ]/colSums(table(questionnaire$behaviour.forgot, questionnaire$gender)), 2)
round(table(questionnaire$behaviour.forgot, questionnaire$sexuality)[2, ]/colSums(table(questionnaire$behaviour.forgot, questionnaire$sexuality)), 2)
round(table(questionnaire$behaviour.forgot, questionnaire$activity.perform.anal)[2, ]/colSums(table(questionnaire$behaviour.forgot, questionnaire$activity.perform.anal)), 2)
round(table(questionnaire$behaviour.forgot, questionnaire$activity.receive.anal)[2, ]/colSums(table(questionnaire$behaviour.forgot, questionnaire$activity.receive.anal)), 2)
round(table(questionnaire$behaviour.forgot, questionnaire$activity.perform.vaginal)[2, ]/colSums(table(questionnaire$behaviour.forgot, questionnaire$activity.perform.vaginal)), 2)
round(table(questionnaire$behaviour.forgot, questionnaire$activity.receive.vaginal)[2, ]/colSums(table(questionnaire$behaviour.forgot, questionnaire$activity.receive.vaginal)), 2)

#For People who reported condom was not available
table(questionnaire$behaviour.not.available, questionnaire$gender)
table(questionnaire$behaviour.not.available, questionnaire$sexuality)
table(questionnaire$behaviour.not.available, questionnaire$activity.perform.anal)
table(questionnaire$behaviour.not.available, questionnaire$activity.receive.anal)
table(questionnaire$behaviour.not.available, questionnaire$activity.perform.vaginal)
table(questionnaire$behaviour.not.available, questionnaire$activity.receive.vaginal)

round(table(questionnaire$behaviour.not.available, questionnaire$gender)[2, ]/colSums(table(questionnaire$behaviour.not.available, questionnaire$gender)), 2)
round(table(questionnaire$behaviour.not.available, questionnaire$sexuality)[2, ]/colSums(table(questionnaire$behaviour.not.available, questionnaire$sexuality)), 2)
round(table(questionnaire$behaviour.not.available, questionnaire$activity.perform.anal)[2, ]/colSums(table(questionnaire$behaviour.not.available, questionnaire$activity.perform.anal)), 2)
round(table(questionnaire$behaviour.not.available, questionnaire$activity.receive.anal)[2, ]/colSums(table(questionnaire$behaviour.not.available, questionnaire$activity.receive.anal)), 2)
round(table(questionnaire$behaviour.not.available, questionnaire$activity.perform.vaginal)[2, ]/colSums(table(questionnaire$behaviour.not.available, questionnaire$activity.perform.vaginal)), 2)
round(table(questionnaire$behaviour.not.available, questionnaire$activity.receive.vaginal)[2, ]/colSums(table(questionnaire$behaviour.not.available, questionnaire$activity.receive.vaginal)), 2)


table(questionnaire$outcome.diagnosis, questionnaire$gender)
table(questionnaire$outcome.diagnosis, questionnaire$sexuality)
table(questionnaire$outcome.diagnosis, questionnaire$activity.perform.anal)
table(questionnaire$outcome.diagnosis, questionnaire$activity.receive.anal)
table(questionnaire$outcome.diagnosis, questionnaire$activity.perform.vaginal)
table(questionnaire$outcome.diagnosis, questionnaire$activity.receive.vaginal)



round(table(questionnaire$outcome.diagnosis, questionnaire$gender)[2, ]/colSums(table(questionnaire$outcome.diagnosis, questionnaire$gender)), 2)
round(table(questionnaire$outcome.diagnosis, questionnaire$sexuality)[2, ]/colSums(table(questionnaire$outcome.diagnosis, questionnaire$sexuality)), 2)
round(table(questionnaire$outcome.diagnosis, questionnaire$activity.perform.anal)[2, ]/colSums(table(questionnaire$outcome.diagnosis, questionnaire$activity.perform.anal)), 2)
round(table(questionnaire$outcome.diagnosis, questionnaire$activity.receive.anal)[2, ]/colSums(table(questionnaire$outcome.diagnosis, questionnaire$activity.receive.anal)), 2)
round(table(questionnaire$outcome.diagnosis, questionnaire$activity.perform.vaginal)[2, ]/colSums(table(questionnaire$outcome.diagnosis, questionnaire$activity.perform.vaginal)), 2)
round(table(questionnaire$outcome.diagnosis, questionnaire$activity.receive.vaginal)[2, ]/colSums(table(questionnaire$outcome.diagnosis, questionnaire$activity.receive.vaginal)), 2)



table(questionnaire$activity.receive.vaginal, questionnaire$gender) #obvious collinearity between gender and recieving vaginal sex

table(questionnaire$activity.perform.vaginal, questionnaire$gender) #obvious collinearity between gender and recieving anal sex

table(questionnaire$behaviour.forgot, questionnaire$condom)[2, ]/sum(table(questionnaire$behaviour.forgot, questionnaire$condom))


table(questionnaire$behaviour.forgot)
table(questionnaire$behaviour.broke)


non.zero.risk <- questionnaire[questionnaire$condom != "never" & questionnaire$sex.in.3.years == "yes", ]
non.zero.risk <- questionnaire[questionnaire$behaviour.broke == 1 | questionnaire$behaviour.not.available == 1 | questionnaire$behaviour.forgot == 1, ]

length(which(non.zero.risk$outcome.no.test == 1))

logistic.regression.or.ci <- function(regress.out, level=0.95)
{
  ################################################################
  #                                                              #
  #  This function takes the output from a glm                   #
  #  (logistic model) command in R and provides not              #
  #  only the usual output from the summary command, but         #
  #  adds confidence intervals for all coefficients and OR's.    #
  #                                                              #
  #  This version accommodates multiple regression parameters    #
  #                                                              #
  ################################################################
  usual.output <- summary(regress.out)
  z.quantile <- qnorm(1-(1-level)/2)
  number.vars <- length(regress.out$coefficients)
  OR <- exp(regress.out$coefficients[-1])
  temp.store.result <- matrix(rep(NA, number.vars*2), nrow=number.vars)
  for(i in 1:number.vars)
  {
    temp.store.result[i,] <- summary(regress.out)$coefficients[i] +
      c(-1, 1) * z.quantile * summary(regress.out)$coefficients[i+number.vars]
  }
  intercept.ci <- temp.store.result[1,]
  slopes.ci <- temp.store.result[-1,]
  OR.ci <- exp(slopes.ci)
  output <- list(regression.table = usual.output, intercept.ci = intercept.ci,
                 slopes.ci = slopes.ci, OR=OR, OR.ci = OR.ci)
  return(output)
}




# Simple LM with One Variable ---------------------------------------------

#Univariate for Broken
lm.gender <- glm(behaviour.broke ~ gender, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.gender)
lm.sexuality <- glm(behaviour.broke ~ sexuality, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.sexuality)
lm.condom <- glm(behaviour.broke ~ condom, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.condom)
lm.perform.anal <- glm(behaviour.broke ~ activity.perform.anal, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.perform.anal)
lm.receive.anal <- glm(behaviour.broke ~ activity.receive.anal, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.receive.anal)
lm.perform.vaginal <- glm(behaviour.broke ~ activity.perform.vaginal, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.perform.vaginal)
lm.receive.vaginal <- glm(behaviour.broke ~ activity.receive.vaginal, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.receive.vaginal)



#Univariate for Forgotten
lm.gender <- lm(behaviour.forgot ~ gender, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.gender)
lm.sexuality <- lm(behaviour.forgot ~ sexuality, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.sexuality)
lm.perform.anal <- lm(behaviour.forgot ~ activity.perform.anal, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.perform.anal)
lm.receive.anal <- lm(behaviour.forgot ~ activity.receive.anal, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.receive.anal)
lm.perform.vaginal <- lm(behaviour.forgot ~ activity.perform.vaginal, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.perform.vaginal)
lm.receive.vaginal <- lm(behaviour.forgot ~ activity.receive.vaginal, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.receive.vaginal)
lm.condom <- lm(behaviour.forgot ~ condom, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.condom)

#Univariate for Not Available
lm.gender <- lm(behaviour.not.available ~ gender, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.gender)
lm.sexuality <- lm(behaviour.not.available ~ sexuality, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.sexuality)
lm.perform.anal <- lm(behaviour.not.available ~ activity.perform.anal, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.perform.anal)
lm.receive.anal <- lm(behaviour.not.available ~ activity.receive.anal, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.receive.anal)
lm.perform.vaginal <- lm(behaviour.not.available ~ activity.perform.vaginal, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.perform.vaginal)
lm.receive.vaginal <- lm(behaviour.not.available ~ activity.receive.vaginal, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.receive.vaginal)
lm.condom <- lm(behaviour.not.available ~ condom, family = binomial(link = 'logit'),data = questionnaire)
logistic.regression.or.ci(lm.condom)




# Model Fitting for Broken Condom -----------------------------------------------------------

#First Suggestion
glm.formula <- behaviour.broke ~ gender + sexuality +
  sexuality:activity.receive.anal + sexuality:activity.receive.anal +
  gender:activity.receive.anal + HIV
lm.full     <- glm(glm.formula, family = binomial(link = 'logit'), data = questionnaire)
prob        <- predict(lm.full,type=c("response"))
g           <- roc(as.numeric(questionnaire$behaviour.broke) ~ prob)
# plot(g)
auc(g)



#Begin with over fitted model
lm.best  <- MASS::stepAIC(glm(behaviour.broke ~ gender + sexuality + condom  + 
                                activity.participate.anal + activity.participate.vaginal+ 
                                sexuality:activity.participate.anal + HIV.status + age + prep.user, data = questionnaire, family = binomial(link = 'logit')), direction = "backward")

lm.best <- glm(behaviour.broke ~ gender + activity.participate.anal, data = questionnaire)
prob <- predict(lm.best,type=c("response"))
g <- roc(as.numeric(questionnaire$behaviour.broke) ~ prob)
auc(g)
logistic.regression.or.ci(lm.best) 

 lm.best.subset <- glm(lm.best$formula, family = binomial(link = 'logit'), data = questionnaire[1:350, ])
prob        <- predict(lm.best.subset,type=c("response"), newdata = questionnaire[-c(1:350), ])
prediction <- rbinom(length(prob), 1, prob)
table(prediction, questionnaire[-c(1:350), ]$behaviour.broke)

full <- glm(behaviour.broke ~ gender + sexuality + condom  + 
              activity.participate.anal + activity.participate.vaginal+ 
              sexuality:activity.participate.anal + HIV + age + prep.user + HIV + age + prep.user, data = questionnaire, na.action = "na.fail")
allfits <- dredge(full)



# Model Fitting for Forget Condom  -----------------------------------------------------------

#First Suggestion
glm.formula <- behaviour.forgot ~ gender + sexuality +
  sexuality:activity.receive.anal + sexuality:activity.perform.anal +
  gender:activity.receive.anal
lm.full     <- glm(glm.formula, family = binomial(link = 'logit'), data = questionnaire)
prob        <- predict(lm.full,type=c("response"))
g           <- roc(as.numeric(questionnaire$behaviour.forgot) ~ prob)
# plot(g)
auc(g)



#Begin with over fitted model
lm.best  <- MASS::stepAIC(glm(behaviour.forgot ~ gender + sexuality + condom  + 
                                activity.participate.anal + activity.participate.vaginal+ 
                                sexuality:activity.participate.anal + HIV.status + age + prep.user, data = questionnaire, family = binomial(link = 'logit')), direction = "backward")
prob <- predict(lm.best,type=c("response"))
g <- roc(as.numeric(questionnaire$behaviour.forgot) ~ prob)
auc(g)  
logistic.regression.or.ci(lm.best)

lm.best.subset <- glm(lm.best$formula, family = binomial(link = 'logit'), data = questionnaire[1:350, ])
prob        <- predict(lm.best.subset,type=c("response"), newdata = questionnaire[-c(1:350), ])
prediction <- rbinom(length(prob), 1, prob)
table(prediction, questionnaire[-c(1:350), ]$behaviour.forgot)


allfits <- dredge(full)

# Model Fitting for Not Available  -----------------------------------------------------------

#First Suggestion
glm.formula <- behaviour.not.available ~ gender + sexuality +
  sexuality:activity.receive.anal + sexuality:activity.perform.anal +
  gender:activity.receive.anal + age + prep.user + HIV
lm.full     <- glm(glm.formula, family = binomial(link = 'logit'), data = questionnaire)
prob        <- predict(lm.full,type=c("response"))
g           <- roc(as.numeric(questionnaire$behaviour.not.available) ~ prob)
# plot(g)
auc(g)



#Begin with over fitted model
lm.best  <- MASS::stepAIC(glm(behaviour.not.available ~ gender + sexuality +
                                sexuality:activity.receive.anal + sexuality:activity.perform.anal +
                                gender:activity.receive.anal + age + prep.user + HIV.status, data = questionnaire, family = binomial(link = 'logit')), direction = "backward")
prob <- predict(lm.best,type=c("response"))
g <- roc(as.numeric(questionnaire$behaviour.not.available) ~ prob)
auc(g)
logistic.regression.or.ci(lm.best)



lm.best.subset <- glm(lm.best$formula, family = binomial(link = 'logit'), data = questionnaire[1:350, ])
prob        <- predict(lm.best.subset,type=c("response"), newdata = questionnaire[-c(1:350), ])
prediction <- rbinom(length(prob), 1, prob)
table(prediction, questionnaire[-c(1:350), ]$behaviour.not.available)




# Model Fitting for STD Diagnosis  -----------------------------------------------------------

#Begin with over fitted model
lm.best  <- MASS::stepAIC(glm(outcome.diagnosis ~ gender + sexuality + condom  + 
                                activity.participate.anal + activity.participate.vaginal+ 
                                sexuality:activity.participate.anal + HIV.status + age + prep.user, data = questionnaire, family = binomial(link = 'logit')), direction = "both")
prob <- predict(lm.best,type=c("response"))
g <- roc(as.numeric(questionnaire$outcome.diagnosis) ~ prob)
auc(g)
logistic.regression.or.ci(lm.best)



lm.best.subset <- glm(lm.best$formula, family = binomial(link = 'logit'), data = questionnaire[1:350, ])
prob        <- predict(lm.best.subset,type=c("response"), newdata = questionnaire[-c(1:350), ])
prediction <- rbinom(length(prob), 1, prob)
table(prediction, questionnaire[-c(1:350), ]$behaviour.not.available)



library(MuMIn)
options(na.action = "na.fail")
full <- glm(outcome.diagnosis ~ gender + sexuality + condom  + 
              activity.perform.anal + activity.receive.anal + activity.perform.vaginal + 
              sexuality:activity.perform.anal  + sexuality:activity.receive.anal + sexuality:activity.perform.vaginal + 
              gender:activity.receive.anal + age + prep.user, data = questionnaire)
allfits <- dredge(full)




# Model Fitting for No Action  -----------------------------------------------------------

#Begin with over fitted model
lm.best  <- MASS::stepAIC(glm(outcome.no.test~ gender + sexuality + condom  + 
                                activity.participate.anal + activity.participate.vaginal+ 
                                 + HIV.status + age + prep.user, data = questionnaire, family = binomial(link = 'logit')), direction = "back")
prob <- predict(lm.best,type=c("response"))
g <- roc(as.numeric(questionnaire$outcome.no.test) ~ prob)
auc(g)
logistic.regression.or.ci(lm.best)



lm.best.subset <- glm(lm.best$formula, family = binomial(link = 'logit'), data = questionnaire[1:350, ])
prob        <- predict(lm.best.subset,type=c("response"), newdata = questionnaire[-c(1:350), ])
prediction <- rbinom(length(prob), 1, prob)
table(prediction, questionnaire[-c(1:350), ]$behaviour.not.available)



library(MuMIn)
options(na.action = "na.fail")
full <- glm(outcome.no.test ~ gender + sexuality + condom  + 
              activity.perform.anal + activity.receive.anal + activity.perform.vaginal + 
              sexuality:activity.perform.anal  + sexuality:activity.receive.anal + sexuality:activity.perform.vaginal + 
              gender:activity.receive.anal + age + prep.user + HIV, data = questionnaire)
allfits <- dredge(full)



lm.best.no.condom  <- MASS::stepAIC(glm(outcome.no.test~ gender + sexuality +  
                                          activity.perform.anal + activity.receive.anal + activity.perform.vaginal + 
                                          sexuality:activity.perform.anal  + sexuality:activity.receive.anal + sexuality:activity.perform.vaginal + 
                                          gender:activity.receive.anal + age + prep.user + HIV, data = questionnaire[questionnaire$condom == "never", ]), direction = "back")
prob <- predict(lm.best.no.condom,type=c("response"))
g <- roc(as.numeric(questionnaire[questionnaire$condom == "never", ]$outcome.no.test) ~ prob)
auc(g)
logistic.regression.or.ci(lm.best.no.condom)





