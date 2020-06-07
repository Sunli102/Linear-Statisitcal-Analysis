#################################################
### import data
#################################################
bank = read.csv('~/GSU/Dataset/bank-additional/bank-additional-full.csv', header = T, sep = ';')
str(bank)
names(bank)
dim = dim(bank)

#################################################
### check if there are missing values
#################################################
sum(is.na(bank))  


#####################################################
### plot the histogram of each variables separately
#####################################################
# plot continuous variable
par(mfrow = c(2, 5))
library(ggplot2)
for (i in c(1,11:14,16:20))
{print(qplot(bank[,i], geom = 'histogram', bins = 5,xlab = names(bank[i])))}

# plot categorical variable
library(ggplot2)
for (i in c(2:10,15,21))
{
  ggbar = ggplot(data.frame(bank[,i]), aes(x=bank[,i]))+ geom_bar()
  ggbar = ggbar + labs(x = colnames(bank)[i])
  print(ggbar)
}


#################################################
### plot histogram of subsamples
#################################################
# plot histogram of subsamples of continous variables
par(mfrow = c(2,5))
for (i in c(1,11:14,16:20))
{
  hist(bank[,i][which(bank$y=='no')],  border = 'blue', main = 
         paste('Histogram of ', names(bank[i])), xlab = names(bank[i]),
       ylab = 'y yes / y no',xlim = range(bank[i]))
  hist(bank[,i][which(bank$y=='yes')], border = 'red',
       ylab = 'presence', add=T)
  if (i == 1)
  {legend('topright', legend=c("no", "yes"),
          col=c("blue", "red"), lty=1:1.5, cex=0.9)
  }
}

# plot the histogram of subsamples of categorical variables
for (i in c(2:10,15))
{
  lengdText = F
  if (i == 2){lengdText = T}
  roll1 = bank[,i][which(bank$y=='no')]
  roll2 = bank[,i][which(bank$y=='yes')]
  rollAll = rbind(table(roll1), table(roll2))
  rownames(rollAll) = c('no', 'yes')
  
  barplot(rollAll, beside = T, col = c('blue', 'red'), legend.text = lengdText, args.legend = list(x = "topright"),
          main = paste('Barplot of ', names(bank[i])), xlab = names(bank[i]),
          ylab = 'yes / no')
}



#################################################
### fitting the full model
#################################################
fit.full = glm(y~., data = bank, family = binomial)
summary(fit.full)


#################################################
## Stepwise Model Selection without interactions
#################################################
fit.null = glm(y~1, data =bank, family = binomial)
select1.1 = step(fit.null, scope = list(lower = fit.null, upper = fit.full),
                 direction = 'both')

fit.mainFactors = glm(y ~ duration + nr.employed + month + poutcome + emp.var.rate + 
  cons.price.idx + job + contact + euribor3m + default + day_of_week + pdays + campaign + cons.conf.idx, 
  data = bank, family = 'binomial')
summary(fit.mainFactors)



### Anova Test to Determine Goodness of Fit: 
anova(fit.mainFactors,test = 'Chisq')


############################################################
### Stepwise Model Selection (including interaction terms)
############################################################
bank.1 = (select1.1$model)
#colnames(bank.1)

pred.names = colnames(bank.1)[-1]
m = length(pred.names)

for (i in 1:(m-1)){
  for (j in (i+1):m) {
    pred.names = c(pred.names, paste(pred.names[i], ':', pred.names[j]))
  }
}

Formula = formula(paste('y ~ ', paste(pred.names, collapse = '+'))); Formula

fit.full.1 = glm(Formula, data = bank.1, family = binomial)
fit.null.1 = glm(y ~ 1, data = bank.1, family = binomial )

select1.2 = step(fit.null.1, scope = list(lower=fit.null.1, upper = fit.full.1),
                 direction = 'both')



### 
bank.2 = (select1.2$model)
fit.interaction = glm(y ~ duration + nr.employed + month + poutcome + emp.var.rate +
  contact + job + day_of_week + campaign + pdays + default +
  euribor3m + nr.employed:month + duration:month + month:emp.var.rate +
  duration:emp.var.rate + month:day_of_week + duration:contact +
  nr.employed:contact + month:campaign + month:default + poutcome:emp.var.rate +
  campaign:pdays + duration:poutcome + emp.var.rate:default +
  day_of_week:pdays + contact:pdays + emp.var.rate:day_of_week +
  month:euribor3m + duration:euribor3m + poutcome:euribor3m +
  nr.employed:euribor3m + emp.var.rate:contact + contact:day_of_week +
  job:euribor3m + contact:campaign, data=bank.2, family = binomial)
summary(fit.interaction)

  

### Anova Test to Determine Goodness of Fit:  
anova(fit.interaction, test="Chisq")

### removed the insignificant variable 'emp.var.rate': 
fit.interaction.1 = glm(y ~ duration + nr.employed + month + poutcome + 
                        contact + job + day_of_week + campaign + pdays + default +
                        euribor3m + nr.employed:month + duration:month +
                         month:day_of_week + duration:contact +
                        nr.employed:contact + month:campaign + month:default  +
                        campaign:pdays + duration:poutcome  +
                        day_of_week:pdays + contact:pdays + 
                        month:euribor3m + duration:euribor3m + poutcome:euribor3m +
                        nr.employed:euribor3m  + contact:day_of_week +
                        job:euribor3m + contact:campaign, data=bank.2, family = binomial)
summary(fit.interaction.1)


### Anova Test to Determine Goodness of Fit: 
anova(fit.interaction.1, test="Chisq")



### romove interaction 'poutcome:euribor3m'
fit.interaction.2 = glm(y ~ duration + nr.employed + month + poutcome + 
                          contact + job + day_of_week + campaign + pdays + default +
                          euribor3m + nr.employed:month + duration:month +
                          month:day_of_week + duration:contact +
                          nr.employed:contact + month:campaign + month:default  +
                          campaign:pdays + duration:poutcome  +
                          day_of_week:pdays + contact:pdays + 
                          month:euribor3m + duration:euribor3m  +
                          nr.employed:euribor3m  + contact:day_of_week +
                          job:euribor3m + contact:campaign, data=bank.2, family = binomial)
summary(fit.interaction.2)


### Anova Test to Determine Goodness of Fit: 
anova(fit.interaction.2, test="Chisq")


### romove interaction 'poutcome:euribor3m'
fit.interaction.3 = glm(y ~ duration + nr.employed + month + poutcome + 
                          contact + job + day_of_week + campaign + pdays + default +
                          euribor3m + nr.employed:month + duration:month +
                          month:day_of_week +
                          nr.employed:contact + month:campaign + month:default  +
                          campaign:pdays + duration:poutcome  +
                          day_of_week:pdays + contact:pdays + 
                          month:euribor3m + duration:euribor3m  +
                          nr.employed:euribor3m  + contact:day_of_week +
                          job:euribor3m, data=bank.2, family = binomial)
summary(fit.interaction.3)
### Anova Test to Determine Goodness of Fit: 
anova(fit.interaction.3, test="Chisq")


### romove interaction ' duration:poutcome '
fit.interaction.4 = glm(y ~ duration + nr.employed + month + poutcome + 
                          contact + job + day_of_week + campaign + pdays + default +
                          euribor3m + nr.employed:month + duration:month +
                          month:day_of_week +
                          nr.employed:contact + month:campaign + month:default  +
                          campaign:pdays + 
                          day_of_week:pdays + contact:pdays + 
                          month:euribor3m + duration:euribor3m  +
                          nr.employed:euribor3m  + contact:day_of_week +
                          job:euribor3m, data=bank.2, family = binomial)
summary(fit.interaction.4)
### Anova Test to Determine Goodness of Fit: 
anova(fit.interaction.4, test="Chisq")

### romove interaction 'contact:pdays 0.0977156'
fit.interaction.5 = glm(y ~ duration + nr.employed + month + poutcome + 
                          contact + job + day_of_week + campaign + pdays + default +
                          euribor3m + nr.employed:month + duration:month +
                          month:day_of_week +
                          nr.employed:contact + month:campaign + month:default  +
                          campaign:pdays + 
                          day_of_week:pdays + 
                          month:euribor3m + duration:euribor3m  +
                          nr.employed:euribor3m  + contact:day_of_week +
                          job:euribor3m, data=bank.2, family = binomial)
summary(fit.interaction.5)
### Anova Test to Determine Goodness of Fit: 
anova(fit.interaction.5, test="Chisq")


### romove interaction 'contact:day_of_week 0.0582671 and contact:day_of_week 0.0654699' 
fit.interaction.6 = glm(y ~ duration + nr.employed + month + poutcome + 
                          contact + job + day_of_week + campaign + pdays + default +
                          euribor3m + nr.employed:month + duration:month +
                          month:day_of_week +nr.employed:contact + month:campaign + month:default  +
                          campaign:pdays + day_of_week:pdays + month:euribor3m + duration:euribor3m  +
                          job:euribor3m, data=bank, family = binomial)
summary(fit.interaction.6)


### Anova Test to Determine Goodness of Fit: 
anova(fit.interaction.6, test="Chisq")


#################################################
### Anova Test to Determine Goodness of Fit: 
#################################################
anova(fit.interaction.6, test="Chisq")
anova(fit.mainFactors, fit.interaction, test = 'LRT')



#################################################
### comparison of two models
#################################################
anova(fit.mainFactors, fit.interaction.6)
anova(fit.interaction.6, fit.mainFactors, test = 'LRT')


#################################################
### Analyzing Cook’s Distance:
#################################################
cooks.distance_mainFactors<-cooks.distance(fit.mainFactors)
which(cooks.distance_mainFactors>1)   

cooks.distance_interaction<-cooks.distance(fit.interaction)
which(cooks.distance_interaction>1)    

cooks.distance_interaction6<-cooks.distance(fit.interaction.6)
which(cooks.distance_interaction6>1)   



############################################################
### Wald Test to determine if predictors are significant:
############################################################
library(survey)
names.mainFactors = all.vars(formula(fit.mainFactors)[-2])
n=length(names.mainFactors)
for (i in 1:n){
  print(names.mainFactors[i])
  print(regTermTest(fit.mainFactors,names.mainFactors[i]))
}

names.interaction6 = all.vars(formula(fit.interaction.6)[-2])
names.interaction6 = c(names.interaction6, c('nr.employed:month' , 
                                               'duration:month' , 'month:day_of_week' , 'nr.employed:contact' , 
                                               'month:campaign' , 'month:default' , 'campaign:pdays' , 'day_of_week:pdays' , 
                                               'month:euribor3m' , 'duration:euribor3m' , 'job:euribor3m'))
n=length(names.interaction6)
for (i in 1:n){
  print(names.interaction6[i])
  print(regTermTest(fit.interaction.6,names.interaction6[i]))
}



#################################################
### To convert the coefficients to odds-ratios:
#################################################
exp(coef(fit.mainFactors))
exp(coef(fit.interaction.6))


#####################################################
### To create a confidence interval of odds-ratios:
#####################################################
exp(cbind(OR=coef(fit.mainFactors),confint(fit.mainFactors)))
exp(cbind(OR=coef(fit.interaction.6),confint(fit.interaction.6)))


#################################################
### Hoslem-Lemeshow Goodness of Fit Test: (g>p+1)
#################################################
#install.packages('ResourceSelection')
library(ResourceSelection)
hoslem.test(fit.mainFactors$y,fitted(fit.mainFactors),g=15)
hoslem.test(fit.mainFactors$y,fitted(fit.interaction.6),g=25)



#################################################
### Looking at VIF for Collinearity:
#################################################
library(car)
vif(fit.mainFactors)
vif(fit.interaction.6)
# Error in vif.default(fit.interaction.6) : 
#   there are aliased coefficients in the model
alias(fit.interaction.6)    



################################################# 
### Determining the Pseudo-Rsq: 
#################################################
#install.packages('pscl')
library(pscl)
pR2(fit.mainFactors)
pR2(fit.interaction.6)


#####################################################################
### Plotting the effects  predict subscription:
#####################################################################
#install.packages('effects')
library(effects)
eff = allEffects(fit.mainFactors)
n = length(eff)
for(i in 1:n){
  plot(eff[i])
}

eff.interaction = allEffects(fit.interaction.6)
n = length(eff.interaction)
for(i in 1:n){
  plot(eff.interaction[i])
}


#################################################
### Cross Validation to obtain accuracy of model:
#################################################
library(caret)
Train<-createDataPartition(bank$y,p=0.8,list=FALSE)
training<-bank[Train,]
testing<-bank[-Train,]
ctrl<-trainControl(method="repeatedcv",number=10,savePredictions=TRUE)


# train full model 
mod_fit.full<-train(fit.full$formula,data=training,method="glm",family="binomial",trControl=ctrl,tuneLength=5)
pred.full<-predict(mod_fit.full,newdata=testing)
confusionMatrix(data=pred.full,testing$y)
 
# train model with main factors only
mod_fit.mainFactors<-train(fit.mainFactors$formula,data=training,method="glm",family="binomial",trControl=ctrl,tuneLength=5)
pred.mainFactors<-predict(mod_fit.mainFactors,newdata=testing)
confusionMatrix(data=pred.mainFactors,testing$y)
    
# train model with interactions
mod_fit.interaction.6<-train(fit.interaction.6$formula,data=training,method="glm",family="binomial",trControl=ctrl,tuneLength=5)
pred.interaction.6<-predict(mod_fit.interaction.6,newdata=testing)
confusionMatrix(data=pred.interaction.6,testing$y)




#################################################
### Determining Variables of Importance:
#################################################
varImp(mod_fit.mainFactors)
varImp(mod_fit.interaction.6)



###########################################################
### Graphing and finding the area underneath the ROC Curve:
###########################################################
#install.packages('ROCR')
library(ROCR)
p.mainFactors<-predict(fit.mainFactors,newdata=testing,type="response")
pr.mainFactors<-prediction(p.mainFactors,testing$y)
prf.mainFactors<-performance(pr.mainFactors,measure="tpr",x.measure="fpr")
plot(prf.mainFactors, main='ROC Curve of mainFactors')
auc.mainFactors<-performance(pr.mainFactors,measure="auc")
auc.mainFactors<-auc.mainFactors@y.values[[1]]
auc.mainFactors
# [1] 0.941257


p.interaction.6<-predict(fit.interaction.6,newdata=testing,type="response")
pr.interaction.6<-prediction(p.interaction.6,testing$y)
prf.interaction.6<-performance(pr.interaction.6,measure="tpr",x.measure="fpr")
plot(prf.interaction.6, main='ROC Curve of MainFactors with Interaction')
auc.interaction.6<-performance(pr.interaction.6,measure="auc")
auc.interaction.6<-auc.interaction.6@y.values[[1]]
auc.interaction.6
# [1] 0.9470135
