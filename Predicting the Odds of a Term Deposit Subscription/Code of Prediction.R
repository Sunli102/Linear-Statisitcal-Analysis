#################################################
### import data
#################################################
bank = read.csv('~/GSU/2019 Fall/Linear Stat/Final Project/Dataset/bank-additional/bank-additional-full.csv', header = T, sep = ';')
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
# Step:  AIC=17170.27
# y ~ duration + nr.employed + month + poutcome + emp.var.rate +
#   cons.price.idx + job + contact + euribor3m + default + day_of_week +
#   pdays + campaign + cons.conf.idx
# Df Deviance   AIC
# <none>                 17094 17170
# - nr.employed     1    17097 17171
# + previous        1    17093 17171
# + education       7    17082 17172
# + age             1    17094 17172
# + loan            2    17093 17173
# + housing         2    17094 17174
# + marital         3    17092 17174
# - cons.conf.idx   1    17101 17175
# - euribor3m       1    17101 17175
# - campaign        1    17107 17181
# - day_of_week     4    17117 17185
# - pdays           1    17111 17185
# - default         2    17117 17189
# - job            11    17145 17199
# - cons.price.idx  1    17168 17242
# - contact         1    17169 17243
# - poutcome        2    17184 17256
# - emp.var.rate    1    17246 17320
# - month           9    17658 17716
# - duration        1    22724 22798


    
    

fit.mainFactors = glm(y ~ duration + nr.employed + month + poutcome + emp.var.rate + 
  cons.price.idx + job + contact + euribor3m + default + day_of_week + pdays + campaign + cons.conf.idx, 
  data = bank, family = 'binomial')
summary(fit.mainFactors)

# 
# Call:
#   glm(formula = y ~ duration + nr.employed + month + poutcome + 
#         emp.var.rate + cons.price.idx + job + contact + euribor3m + 
#         default + day_of_week + pdays + campaign + cons.conf.idx, 
#       family = "binomial", data = bank)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -5.9947  -0.2991  -0.1855  -0.1348   3.3485  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)         -2.323e+02  3.822e+01  -6.078 1.22e-09 ***
#   duration             4.702e-03  7.450e-05  63.116  < 2e-16 ***
#   nr.employed          5.116e-03  3.108e-03   1.646 0.099749 .
# monthaug             8.674e-01  1.202e-01   7.217 5.32e-13 ***
#   monthdec             3.019e-01  2.088e-01   1.446 0.148150
# monthjul             1.361e-01  9.598e-02   1.418 0.156227
# monthjun            -5.115e-01  1.258e-01  -4.068 4.75e-05 ***
#   monthmar             2.019e+00  1.441e-01  14.007  < 2e-16 ***
#   monthmay            -4.553e-01  8.233e-02  -5.530 3.20e-08 ***
#   monthnov            -4.253e-01  1.208e-01  -3.522 0.000429 ***
#   monthoct             1.803e-01  1.535e-01   1.175 0.240163
# monthsep             3.607e-01  1.793e-01   2.012 0.044220 *
#   poutcomenonexistent  5.026e-01  6.411e-02   7.840 4.52e-15 ***
#   poutcomesuccess      1.036e+00  2.040e-01   5.081 3.76e-07 ***
#   emp.var.rate        -1.752e+00  1.419e-01 -12.350  < 2e-16 ***
#   cons.price.idx       2.160e+00  2.516e-01   8.586  < 2e-16 ***
#   jobblue-collar      -3.329e-01  6.586e-02  -5.055 4.31e-07 ***
#   jobentrepreneur     -2.029e-01  1.244e-01  -1.631 0.102947
# jobhousemaid        -1.118e-01  1.409e-01  -0.793 0.427695
# jobmanagement       -4.317e-02  8.341e-02  -0.518 0.604744
# jobretired           2.047e-01  8.381e-02   2.442 0.014610 *
#   jobself-employed    -1.509e-01  1.169e-01  -1.291 0.196739
# jobservices         -2.128e-01  8.168e-02  -2.605 0.009185 **
#   jobstudent           1.770e-01  1.018e-01   1.739 0.081968 .
# jobtechnician       -2.728e-02  6.348e-02  -0.430 0.667323
# jobunemployed       -3.686e-02  1.261e-01  -0.292 0.769998
# jobunknown          -9.286e-02  2.344e-01  -0.396 0.691981
# contacttelephone    -6.421e-01  7.674e-02  -8.368  < 2e-16 ***
#   euribor3m            3.422e-01  1.297e-01   2.638 0.008333 **
#   defaultunknown      -3.106e-01  6.636e-02  -4.681 2.86e-06 ***
#   defaultyes          -7.326e+00  1.134e+02  -0.065 0.948510
# day_of_weekmon      -1.172e-01  6.604e-02  -1.775 0.075831 .
# day_of_weekthu       5.858e-02  6.401e-02   0.915 0.360104
# day_of_weektue       9.500e-02  6.575e-02   1.445 0.148527
# day_of_weekwed       1.727e-01  6.562e-02   2.632 0.008488 **
#   pdays               -8.445e-04  2.036e-04  -4.149 3.34e-05 ***
#   campaign            -3.981e-02  1.155e-02  -3.448 0.000564 ***
#   cons.conf.idx        2.039e-02  7.734e-03   2.637 0.008377 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 28999  on 41187  degrees of freedom
# Residual deviance: 17094  on 41150  degrees of freedom
# AIC: 17170
# 
# Number of Fisher Scoring iterations: 10


### Anova Test to Determine Goodness of Fit: 
anova(fit.mainFactors,test = 'Chisq')
# Analysis of Deviance Table
# 
# Model: binomial, link: logit
# 
# Response: y
# 
# Terms added sequentially (first to last)
# 
# 
# Df Deviance Resid. Df Resid. Dev  Pr(>Chi)
# NULL                           41187      28999
# duration        1   4892.6     41186      24106 < 2.2e-16 ***
#   nr.employed     1   5150.7     41185      18956 < 2.2e-16 ***
#   month           9    923.4     41176      18032 < 2.2e-16 ***
#   poutcome        2    497.2     41174      17535 < 2.2e-16 ***
#   emp.var.rate    1    140.0     41173      17395 < 2.2e-16 ***
#   cons.price.idx  1     60.3     41172      17335 7.983e-15 ***
#   job            11     67.9     41161      17267 3.114e-10 ***
#   contact         1     43.0     41160      17224 5.385e-11 ***
#   euribor3m       1     44.7     41159      17179 2.302e-11 ***
#   default         2     23.3     41157      17156 8.884e-06 ***
#   day_of_week     4     24.7     41153      17131 5.679e-05 ***
#   pdays           1     17.1     41152      17114 3.585e-05 ***
#   campaign        1     12.7     41151      17101  0.000372 ***
#   cons.conf.idx   1      7.0     41150      17094  0.008373 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1




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
#pred.names

Formula = formula(paste('y ~ ', paste(pred.names, collapse = '+'))); Formula

fit.full.1 = glm(Formula, data = bank.1, family = binomial)
fit.null.1 = glm(y ~ 1, data = bank.1, family = binomial )

select1.2 = step(fit.null.1, scope = list(lower=fit.null.1, upper = fit.full.1),
                 direction = 'both')

# Step:  AIC=16333.39
# y ~ duration + nr.employed + month + poutcome + emp.var.rate + 
#   contact + job + day_of_week + campaign + pdays + default + 
#   euribor3m + nr.employed:month + duration:month + month:emp.var.rate + 
#   duration:emp.var.rate + month:day_of_week + duration:contact + 
#   nr.employed:contact + month:campaign + month:default + poutcome:emp.var.rate + 
#   campaign:pdays + duration:poutcome + emp.var.rate:default + 
#   day_of_week:pdays + contact:pdays + emp.var.rate:day_of_week + 
#   month:euribor3m + duration:euribor3m + poutcome:euribor3m + 
#   nr.employed:euribor3m + emp.var.rate:contact + contact:day_of_week + 
#   job:euribor3m + contact:campaign
# 
# Df Deviance   AIC
# <none>                           16009 16333
# + poutcome:pdays            1    16008 16334
# - day_of_week:pdays         4    16018 16334
# - contact:campaign          1    16012 16334
# - job:euribor3m            11    16032 16334
# - month:default             9    16028 16334
# + day_of_week:campaign      4    16002 16334
# + emp.var.rate:euribor3m    1    16008 16334
# - contact:day_of_week       4    16018 16334
# + emp.var.rate:campaign     1    16008 16334
# + duration:nr.employed      1    16008 16334
# + default:campaign          1    16008 16334
# + duration:default          2    16007 16335
# + poutcome:default          2    16007 16335
# - emp.var.rate:day_of_week  4    16019 16335
# - emp.var.rate:contact      1    16013 16335
# + contact:euribor3m         1    16009 16335
# + contact:default           1    16009 16335
# + duration:campaign         1    16009 16335
# + euribor3m:default         1    16009 16335
# + emp.var.rate:pdays        1    16009 16335
# + nr.employed:campaign      1    16009 16335
# + nr.employed:default       1    16009 16335
# + nr.employed:pdays         1    16009 16335
# + duration:pdays            1    16009 16335
# + euribor3m:pdays           1    16009 16335
# + euribor3m:campaign        1    16009 16335
# + default:pdays             1    16009 16335
# + cons.conf.idx             1    16009 16335
# - nr.employed:euribor3m     1    16014 16336
# - contact:pdays             1    16014 16336
# - duration:poutcome         2    16016 16336
# + poutcome:campaign         2    16009 16337
# - emp.var.rate:default      1    16015 16337
# - campaign:pdays            1    16015 16337
# + poutcome:contact          2    16009 16337
# + nr.employed:day_of_week   4    16005 16337
# - poutcome:euribor3m        2    16017 16337
# + nr.employed:poutcome      2    16009 16337
# + default:day_of_week       4    16005 16337
# + euribor3m:day_of_week     4    16006 16338
# + month:contact             9    15996 16338
# + nr.employed:job          11    15992 16338
# + poutcome:job             22    15971 16339
# + month:pdays               9    15997 16339
# + duration:day_of_week      4    16007 16339
# + job:pdays                11    15995 16341
# - duration:contact          1    16020 16342
# + poutcome:day_of_week      8    16003 16343
# + job:contact              11    15999 16345
# + emp.var.rate:job         11    16000 16346
# + duration:job             11    16001 16347
# + job:campaign             11    16001 16347
# - month:campaign            9    16041 16347
# - nr.employed:contact       1    16026 16348
# + month:poutcome           18    15988 16348
# - poutcome:emp.var.rate     2    16030 16350
# - month:emp.var.rate        5    16037 16351
# + job:default              11    16006 16352
# - duration:euribor3m        1    16042 16364
# + job:day_of_week          44    15968 16380
# - duration:emp.var.rate     1    16067 16389
# + month:job                99    15870 16392
# - duration:month            9    16103 16409
# - nr.employed:month         7    16103 16413
# - month:euribor3m           9    16120 16426
# - month:day_of_week        36    16227 16479
# Warning messages:
# 1: glm.fit: algorithm did not converge 
# 2: glm.fit: algorithm did not converge 
# 3: glm.fit: algorithm did not converge 
# 4: glm.fit: algorithm did not converge 
# 5: glm.fit: algorithm did not converge 


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

  
  
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -5.7225  -0.2661  -0.1562  -0.1083   3.2923  
# 
# Coefficients: (13 not defined because of singularities)
# Estimate Std. Error z value Pr(>|z|)
# (Intercept)                      -7.516e+02  1.202e+02  -6.253 4.02e-10 ***
#   duration                          8.503e-03  7.577e-04  11.221  < 2e-16 ***
#   nr.employed                       1.519e-01  2.420e-02   6.275 3.49e-10 ***
#   monthaug                          2.693e+02  2.557e+02   1.053 0.292364
#   monthdec                         -1.289e+03  1.452e+03  -0.888 0.374639
#   monthjul                          5.669e+02  2.338e+02   2.425 0.015294 *
#   monthjun                          6.911e+02  1.575e+02   4.388 1.14e-05 ***
#   monthmar                          8.052e+02  1.286e+02   6.263 3.77e-10 ***
#   monthmay                          6.563e+02  1.176e+02   5.581 2.39e-08 ***
#   monthnov                          9.413e+02  1.227e+02   7.670 1.73e-14 ***
#   monthoct                          1.587e+02  2.714e+02   0.585 0.558781
#   monthsep                          2.580e+02  3.223e+02   0.800 0.423491
#   poutcomenonexistent               1.844e+00  3.391e-01   5.437 5.42e-08 ***
#   poutcomesuccess                   2.558e+00  4.535e-01   5.641 1.69e-08 ***
# ############   emp.var.rate                      1.016e+00  1.396e+00   0.728 0.466868
# contacttelephone                 -3.171e+01  7.925e+00  -4.001 6.30e-05 ***
#   jobblue-collar                   -5.266e-01  1.243e-01  -4.236 2.27e-05 ***
#   jobentrepreneur                  -3.053e-01  2.336e-01  -1.307 0.191341
# jobhousemaid                      2.404e-01  2.393e-01   1.005 0.314967
# jobmanagement                    -2.119e-01  1.438e-01  -1.474 0.140484
# jobretired                        1.056e-02  1.269e-01   0.083 0.933679
# jobself-employed                 -9.157e-02  2.011e-01  -0.455 0.648861
# jobservices                      -3.270e-01  1.486e-01  -2.200 0.027821 *
#   jobstudent                       -3.808e-02  1.645e-01  -0.231 0.816949
# jobtechnician                     7.608e-03  1.086e-01   0.070 0.944159
# jobunemployed                    -4.592e-02  2.004e-01  -0.229 0.818777
# jobunknown                       -7.292e-01  3.830e-01  -1.904 0.056947 .
# day_of_weekmon                    2.317e-01  3.210e-01   0.722 0.470516
# day_of_weekthu                    1.271e+00  3.080e-01   4.127 3.67e-05 ***
#   day_of_weektue                    1.951e+00  3.353e-01   5.820 5.88e-09 ***
#   day_of_weekwed                    2.384e+00  3.401e-01   7.009 2.40e-12 ***
#   campaign                         -2.466e-01  7.163e-02  -3.442 0.000577 ***
#   pdays                            -9.401e-04  2.845e-04  -3.305 0.000950 ***
#   defaultunknown                   -1.039e+00  2.894e-01  -3.591 0.000330 ***
#   defaultyes                       -6.672e+00  1.970e+02  -0.034 0.972978
# ######## euribor3m                         1.675e+02  9.186e+01   1.824 0.068152 .
# nr.employed:monthaug             -5.400e-02  5.290e-02  -1.021 0.307285
# nr.employed:monthdec              2.576e-01  2.915e-01   0.884 0.376857
# nr.employed:monthjul             -1.160e-01  4.796e-02  -2.418 0.015603 *
#   nr.employed:monthjun             -1.413e-01  3.214e-02  -4.396 1.10e-05 ***
#   nr.employed:monthmar             -1.632e-01  2.602e-02  -6.272 3.56e-10 ***
#   nr.employed:monthmay             -1.301e-01  2.416e-02  -5.386 7.21e-08 ***
#   nr.employed:monthnov             -1.946e-01  2.465e-02  -7.891 2.99e-15 ***
#   nr.employed:monthoct             -3.323e-02  5.491e-02  -0.605 0.544992
# nr.employed:monthsep             -5.271e-02  6.465e-02  -0.815 0.414917
# duration:monthaug                 1.634e-03  3.562e-04   4.588 4.48e-06 ***
#   duration:monthdec                 4.934e-04  7.877e-04   0.626 0.531074
# duration:monthjul                 1.526e-03  3.539e-04   4.312 1.61e-05 ***
#   duration:monthjun                 2.253e-03  3.629e-04   6.209 5.34e-10 ***
#   duration:monthmar                 1.588e-03  8.127e-04   1.954 0.050663 .
# duration:monthmay                 2.209e-03  2.824e-04   7.823 5.16e-15 ***
#   duration:monthnov                 1.881e-03  3.924e-04   4.794 1.64e-06 ***
#   duration:monthoct                -1.596e-04  4.443e-04  -0.359 0.719380
# duration:monthsep                 1.704e-05  5.284e-04   0.032 0.974271
# monthaug:emp.var.rate             3.480e+00  3.539e+00   0.983 0.325374
# monthdec:emp.var.rate                    NA         NA      NA       NA
# monthjul:emp.var.rate            -3.490e-01  2.753e+00  -0.127 0.899107
# monthjun:emp.var.rate            -8.019e-01  1.779e+00  -0.451 0.652204
# monthmar:emp.var.rate                    NA         NA      NA       NA
# monthmay:emp.var.rate             7.269e+00  3.152e+00   2.306 0.021093 *
#   monthnov:emp.var.rate            -5.765e+00  1.546e+00  -3.728 0.000193 ***
#   monthoct:emp.var.rate                    NA         NA      NA       NA
# monthsep:emp.var.rate                    NA         NA      NA       NA
# duration:emp.var.rate             1.712e-03  2.321e-04   7.376 1.63e-13 ***
#   monthaug:day_of_weekmon          -6.089e-01  2.773e-01  -2.196 0.028106 *
#   monthdec:day_of_weekmon          -2.707e-01  6.437e-01  -0.421 0.674085
# monthjul:day_of_weekmon          -2.428e-01  3.026e-01  -0.802 0.422295
# monthjun:day_of_weekmon          -4.546e-02  2.923e-01  -0.156 0.876423
# monthmar:day_of_weekmon          -7.319e-01  3.734e-01  -1.960 0.049981 *
#   monthmay:day_of_weekmon           1.935e-03  2.526e-01   0.008 0.993886
# monthnov:day_of_weekmon          -1.844e+00  3.235e-01  -5.700 1.20e-08 ***
#   monthoct:day_of_weekmon          -8.217e-01  3.571e-01  -2.301 0.021395 *
#   monthsep:day_of_weekmon          -6.322e-01  4.048e-01  -1.562 0.118366
# monthaug:day_of_weekthu          -1.384e+00  2.605e-01  -5.314 1.07e-07 ***
#   monthdec:day_of_weekthu          -9.991e-01  6.552e-01  -1.525 0.127284
# monthjul:day_of_weekthu          -1.126e+00  2.831e-01  -3.978 6.94e-05 ***
#   monthjun:day_of_weekthu          -8.545e-01  2.980e-01  -2.867 0.004139 **
#   monthmar:day_of_weekthu          -1.410e+00  3.771e-01  -3.738 0.000185 ***
#   monthmay:day_of_weekthu          -1.266e+00  2.418e-01  -5.237 1.63e-07 ***
#   monthnov:day_of_weekthu          -1.294e+00  2.950e-01  -4.385 1.16e-05 ***
#   monthoct:day_of_weekthu          -1.189e+00  3.222e-01  -3.690 0.000224 ***
#   monthsep:day_of_weekthu          -1.160e+00  3.694e-01  -3.139 0.001694 **
#   monthaug:day_of_weektue          -1.911e+00  2.846e-01  -6.713 1.90e-11 ***
#   monthdec:day_of_weektue          -1.189e+00  7.325e-01  -1.623 0.104619
# monthjul:day_of_weektue          -1.873e+00  3.100e-01  -6.042 1.52e-09 ***
#   monthjun:day_of_weektue          -2.021e+00  3.082e-01  -6.557 5.50e-11 ***
#   monthmar:day_of_weektue          -1.727e+00  3.792e-01  -4.556 5.22e-06 ***
#   monthmay:day_of_weektue          -1.981e+00  2.742e-01  -7.224 5.07e-13 ***
#   monthnov:day_of_weektue          -2.801e+00  3.303e-01  -8.481  < 2e-16 ***
#   monthoct:day_of_weektue          -1.887e+00  3.482e-01  -5.418 6.01e-08 ***
#   monthsep:day_of_weektue          -1.469e+00  3.792e-01  -3.872 0.000108 ***
#   monthaug:day_of_weekwed          -1.942e+00  2.850e-01  -6.816 9.36e-12 ***
#   monthdec:day_of_weekwed          -6.655e-01  6.756e-01  -0.985 0.324635
# monthjul:day_of_weekwed          -1.878e+00  3.086e-01  -6.085 1.17e-09 ***
#   monthjun:day_of_weekwed          -1.770e+00  3.101e-01  -5.709 1.14e-08 ***
#   monthmar:day_of_weekwed          -1.341e+00  4.223e-01  -3.177 0.001489 **
#   monthmay:day_of_weekwed          -1.834e+00  2.682e-01  -6.837 8.10e-12 ***
#   monthnov:day_of_weekwed          -2.882e+00  3.226e-01  -8.936  < 2e-16 ***
#   monthoct:day_of_weekwed          -1.984e+00  3.577e-01  -5.547 2.90e-08 ***
#   monthsep:day_of_weekwed          -1.649e+00  3.810e-01  -4.328 1.50e-05 ***
#   duration:contacttelephone        -7.678e-04  2.312e-04  -3.322 0.000895 ***
#   nr.employed:contacttelephone      6.272e-03  1.558e-03   4.026 5.67e-05 ***
#   monthaug:campaign                 1.392e-01  5.341e-02   2.607 0.009127 **
#   monthdec:campaign                 6.855e-03  1.249e-01   0.055 0.956228
# monthjul:campaign                 6.979e-02  5.045e-02   1.383 0.166571
# monthjun:campaign                 8.287e-02  5.529e-02   1.499 0.133899
# monthmar:campaign                -1.952e-02  7.266e-02  -0.269 0.788235
# monthmay:campaign                 4.234e-02  5.252e-02   0.806 0.420146
# monthnov:campaign                -2.244e-01  8.342e-02  -2.691 0.007130 **
#   monthoct:campaign                 1.821e-02  1.100e-01   0.166 0.868463
# monthsep:campaign                -2.093e-02  9.819e-02  -0.213 0.831208
# monthaug:defaultunknown           7.143e-01  3.571e-01   2.000 0.045460 *
#   monthdec:defaultunknown           1.559e+00  1.006e+00   1.550 0.121236
# monthjul:defaultunknown           8.874e-01  3.598e-01   2.466 0.013648 *
#   monthjun:defaultunknown           7.746e-01  3.693e-01   2.098 0.035931 *
#   monthmar:defaultunknown           1.137e+00  6.192e-01   1.837 0.066248 .
# monthmay:defaultunknown           7.877e-01  3.063e-01   2.571 0.010127 *
#   monthnov:defaultunknown           1.075e+00  3.991e-01   2.693 0.007084 **
#   monthoct:defaultunknown           1.895e+00  5.932e-01   3.194 0.001401 **
#   monthsep:defaultunknown           1.681e+00  7.850e-01   2.141 0.032269 *
#   monthaug:defaultyes              -2.389e-01  2.410e+02  -0.001 0.999209
# monthdec:defaultyes                      NA         NA      NA       NA
# monthjul:defaultyes                      NA         NA      NA       NA
# monthjun:defaultyes                      NA         NA      NA       NA
# monthmar:defaultyes                      NA         NA      NA       NA
# monthmay:defaultyes                      NA         NA      NA       NA
# monthnov:defaultyes                      NA         NA      NA       NA
# monthoct:defaultyes                      NA         NA      NA       NA
# monthsep:defaultyes                      NA         NA      NA       NA
# poutcomenonexistent:emp.var.rate  4.166e-01  1.004e-01   4.151 3.31e-05 ***
#   poutcomesuccess:emp.var.rate      4.513e-01  1.208e-01   3.735 0.000188 ***
#   campaign:pdays                    1.372e-04  5.827e-05   2.355 0.018517 *
#   duration:poutcomenonexistent     -5.289e-04  2.527e-04  -2.093 0.036318 *
#   duration:poutcomesuccess         -9.918e-04  4.451e-04  -2.228 0.025855 *
#   emp.var.rate:defaultunknown       1.344e-01  5.935e-02   2.264 0.023589 *
#   emp.var.rate:defaultyes                  NA         NA      NA       NA
# day_of_weekmon:pdays              1.356e-04  2.266e-04   0.599 0.549506
# day_of_weekthu:pdays             -3.381e-04  2.260e-04  -1.496 0.134550
# day_of_weektue:pdays             -1.133e-04  2.289e-04  -0.495 0.620674
# day_of_weekwed:pdays             -4.093e-04  2.368e-04  -1.729 0.083849 .
# contacttelephone:pdays           -5.758e-04  2.762e-04  -2.085 0.037065 *
#   emp.var.rate:day_of_weekmon       5.487e-02  5.224e-02   1.050 0.293638
# emp.var.rate:day_of_weekthu      -3.050e-02  5.164e-02  -0.591 0.554848
# emp.var.rate:day_of_weektue      -2.099e-02  5.081e-02  -0.413 0.679573
# emp.var.rate:day_of_weekwed       9.658e-02  5.019e-02   1.924 0.054324 .
# monthaug:euribor3m                1.325e+01  5.120e+00   2.589 0.009632 **
#   monthdec:euribor3m               -5.426e+00  1.811e+01  -0.300 0.764413
# monthjul:euribor3m                1.958e+01  3.726e+00   5.257 1.47e-07 ***
#   monthjun:euribor3m                2.123e+01  3.554e+00   5.973 2.33e-09 ***
#   monthmar:euribor3m                2.031e+01  2.849e+00   7.129 1.01e-12 ***
#   monthmay:euribor3m                1.344e+01  3.040e+00   4.423 9.75e-06 ***
#   monthnov:euribor3m                2.723e+01  3.176e+00   8.575  < 2e-16 ***
#   monthoct:euribor3m                1.577e+01  3.941e+00   4.000 6.32e-05 ***
#   monthsep:euribor3m                1.325e+01  6.608e+00   2.005 0.044954 *
#   duration:euribor3m               -1.237e-03  2.205e-04  -5.609 2.04e-08 ***
#   poutcomenonexistent:euribor3m    -2.935e-01  1.117e-01  -2.628 0.008598 **
#   poutcomesuccess:euribor3m        -4.190e-01  2.029e-01  -2.065 0.038931 *
#   nr.employed:euribor3m            -3.615e-02  1.801e-02  -2.007 0.044745 *
#   emp.var.rate:contacttelephone    -1.547e-01  8.197e-02  -1.887 0.059102 .
# contacttelephone:day_of_weekmon  -4.935e-01  2.022e-01  -2.441 0.014635 *
#   contacttelephone:day_of_weekthu  -4.813e-02  2.007e-01  -0.240 0.810468
# contacttelephone:day_of_weektue  -3.793e-01  2.007e-01  -1.890 0.058764 .
# contacttelephone:day_of_weekwed  -2.841e-01  1.967e-01  -1.445 0.148585
# jobblue-collar:euribor3m          1.096e-01  3.826e-02   2.865 0.004172 **
#   jobentrepreneur:euribor3m         7.205e-02  7.163e-02   1.006 0.314508
# jobhousemaid:euribor3m           -1.203e-01  8.151e-02  -1.476 0.139956
# jobmanagement:euribor3m           8.690e-02  4.867e-02   1.786 0.074148 .
# jobretired:euribor3m              5.994e-02  5.666e-02   1.058 0.290138
# jobself-employed:euribor3m       -3.509e-02  6.968e-02  -0.504 0.614552
# jobservices:euribor3m             9.514e-02  4.683e-02   2.032 0.042203 *
#   jobstudent:euribor3m              9.662e-02  9.581e-02   1.008 0.313237
# jobtechnician:euribor3m          -7.471e-03  3.697e-02  -0.202 0.839857
# jobunemployed:euribor3m           2.481e-03  8.129e-02   0.031 0.975658
# jobunknown:euribor3m              2.285e-01  1.230e-01   1.858 0.063197 .
# contacttelephone:campaign         4.628e-02  2.901e-02   1.595 0.110660


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

# Terms added sequentially (first to last)
# 
# 
# Df Deviance Resid. Df Resid. Dev  Pr(>Chi)
# NULL                                  41187      28999
# duration               1   4892.6     41186      24106 < 2.2e-16 ***
#   nr.employed            1   5150.7     41185      18956 < 2.2e-16 ***
#   month                  9    923.4     41176      18032 < 2.2e-16 ***
#   poutcome               2    497.2     41174      17535 < 2.2e-16 ***
#   contact                1     61.2     41173      17474 5.129e-15 ***
#   job                   11     72.9     41162      17401 3.351e-11 ***
#   day_of_week            4     26.0     41158      17375 3.216e-05 ***
#   campaign               1     19.7     41157      17355 9.275e-06 ***
#   pdays                  1     16.6     41156      17339 4.674e-05 ***
#   default                2     28.6     41154      17310 6.176e-07 ***
#   euribor3m              1     27.3     41153      17283 1.715e-07 ***
#   nr.employed:month      9    270.7     41144      17012 < 2.2e-16 ***
#   duration:month         9    241.6     41135      16770 < 2.2e-16 ***
#   month:day_of_week     36    189.9     41099      16580 < 2.2e-16 ***
#   duration:contact       1      0.7     41098      16580 0.3930268
# nr.employed:contact    1     37.2     41097      16543 1.074e-09 ***
#   month:campaign         9     36.2     41088      16506 3.717e-05 ***
#   month:default         10     32.7     41078      16474 0.0003083 ***
#   campaign:pdays         1      8.8     41077      16465 0.0030674 **
#   duration:poutcome      2      4.0     41075      16461 0.1374390
# day_of_week:pdays      4     11.9     41071      16449 0.0181411 *
#   contact:pdays          1      3.1     41070      16446 0.0767107 .
# month:euribor3m        9    201.6     41061      16244 < 2.2e-16 ***
#   duration:euribor3m     1     24.1     41060      16220 9.134e-07 ***
# ####   poutcome:euribor3m     2      0.8     41058      16220 0.6715170
# nr.employed:euribor3m  1      2.9     41057      16216 0.0863545 .
# contact:day_of_week    4      8.2     41053      16208 0.0836825 .
# job:euribor3m         11     24.3     41042      16184 0.0114478 *
#   contact:campaign       1      1.1     41041      16183 0.2991032
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

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

# Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
# NULL                                  41187      28999              
# duration               1   4892.6     41186      24106 < 2.2e-16 ***
#   nr.employed            1   5150.7     41185      18956 < 2.2e-16 ***
#   month                  9    923.4     41176      18032 < 2.2e-16 ***
#   poutcome               2    497.2     41174      17535 < 2.2e-16 ***
#   contact                1     61.2     41173      17474 5.129e-15 ***
#   job                   11     72.9     41162      17401 3.351e-11 ***
#   day_of_week            4     26.0     41158      17375 3.216e-05 ***
#   campaign               1     19.7     41157      17355 9.275e-06 ***
#   pdays                  1     16.6     41156      17339 4.674e-05 ***
#   default                2     28.6     41154      17310 6.176e-07 ***
#   euribor3m              1     27.3     41153      17283 1.715e-07 ***
#   nr.employed:month      9    270.7     41144      17012 < 2.2e-16 ***
#   duration:month         9    241.6     41135      16770 < 2.2e-16 ***
#   month:day_of_week     36    189.9     41099      16580 < 2.2e-16 ***
#######   duration:contact       1      0.7     41098      16580 0.3930268    
# nr.employed:contact    1     37.2     41097      16543 1.074e-09 ***
#   month:campaign         9     36.2     41088      16506 3.717e-05 ***
#   month:default         10     32.7     41078      16474 0.0003083 ***
#   campaign:pdays         1      8.8     41077      16465 0.0030674 ** 
#   duration:poutcome      2      4.0     41075      16461 0.1374390    
# day_of_week:pdays      4     11.9     41071      16449 0.0181411 *  
#   contact:pdays          1      3.1     41070      16446 0.0767107 .  
# month:euribor3m        9    201.6     41061      16244 < 2.2e-16 ***
#   duration:euribor3m     1     24.1     41060      16220 9.134e-07 ***
#   nr.employed:euribor3m  1      3.1     41059      16217 0.0806291 .  
# contact:day_of_week    4      8.2     41055      16209 0.0850185 .  
# job:euribor3m         11     24.2     41044      16185 0.0117713 *  
#######   contact:campaign       1      1.1     41043      16184 0.2989141    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

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
# Coefficients: (8 not defined because of singularities)
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                  -6.306e+02  1.039e+02  -6.068 1.29e-09 ***
#   duration                      3.032e-03  2.320e-04  13.073  < 2e-16 ***
#   nr.employed                   1.280e-01  2.107e-02   6.075 1.24e-09 ***
#   monthaug                      6.711e+02  1.044e+02   6.426 1.31e-10 ***
#   monthdec                     -1.238e+03  1.335e+03  -0.927 0.353756    
# monthjul                      6.714e+02  1.050e+02   6.396 1.60e-10 ***
#   monthjun                      5.945e+02  1.051e+02   5.658 1.53e-08 ***
#   monthmar                      8.035e+02  1.286e+02   6.249 4.13e-10 ***
#   monthmay                      7.788e+02  1.048e+02   7.431 1.08e-13 ***
#   monthnov                      6.412e+02  1.052e+02   6.098 1.08e-09 ***
#   monthoct                      6.344e+02  1.050e+02   6.045 1.50e-09 ***
#   monthsep                      6.834e+02  1.213e+02   5.634 1.76e-08 ***
#   poutcomenonexistent           4.368e-01  6.408e-02   6.816 9.34e-12 ***
#   poutcomesuccess               9.155e-01  2.048e-01   4.471 7.80e-06 ***
#   contacttelephone             -2.270e+01  4.599e+00  -4.935 8.03e-07 ***
#   jobblue-collar               -5.400e-01  1.223e-01  -4.415 1.01e-05 ***
#   jobentrepreneur              -3.292e-01  2.327e-01  -1.415 0.157161    
# jobhousemaid                  1.927e-01  2.370e-01   0.813 0.416070    
# jobmanagement                -2.043e-01  1.428e-01  -1.431 0.152417    
# jobretired                    6.274e-03  1.258e-01   0.050 0.960219    
# jobself-employed             -9.964e-02  2.007e-01  -0.496 0.619561    
# jobservices                  -3.581e-01  1.474e-01  -2.430 0.015102 *  
#   jobstudent                   -4.550e-02  1.628e-01  -0.279 0.779932    
# jobtechnician                 1.039e-02  1.078e-01   0.096 0.923200    
# jobunemployed                -7.597e-03  1.993e-01  -0.038 0.969592    
# jobunknown                   -6.956e-01  3.808e-01  -1.826 0.067783 .  
# day_of_weekmon                1.356e-02  2.896e-01   0.047 0.962656    
# day_of_weekthu                1.331e+00  2.762e-01   4.821 1.43e-06 ***
#   day_of_weektue                1.933e+00  3.065e-01   6.306 2.86e-10 ***
#   day_of_weekwed                2.063e+00  3.100e-01   6.655 2.83e-11 ***
#   campaign                     -2.377e-01  7.162e-02  -3.318 0.000905 ***
#   pdays                        -9.626e-04  2.760e-04  -3.488 0.000486 ***
#   defaultunknown               -1.292e+00  2.683e-01  -4.817 1.46e-06 ***
#   defaultyes                   -6.869e+00  1.970e+02  -0.035 0.972181    
# euribor3m                    -1.729e+01  2.497e+00  -6.925 4.36e-12 ***
#   nr.employed:monthaug         -1.361e-01  2.117e-02  -6.432 1.26e-10 ***
#   nr.employed:monthdec          2.473e-01  2.682e-01   0.922 0.356472    
# nr.employed:monthjul         -1.361e-01  2.128e-02  -6.398 1.57e-10 ***
#   nr.employed:monthjun         -1.207e-01  2.129e-02  -5.670 1.43e-08 ***
#   nr.employed:monthmar         -1.629e-01  2.603e-02  -6.259 3.88e-10 ***
#   nr.employed:monthmay         -1.577e-01  2.124e-02  -7.425 1.13e-13 ***
#   nr.employed:monthnov         -1.299e-01  2.131e-02  -6.094 1.10e-09 ***
#   nr.employed:monthoct         -1.287e-01  2.127e-02  -6.053 1.43e-09 ***
#   nr.employed:monthsep         -1.384e-01  2.403e-02  -5.761 8.37e-09 ***
#   duration:monthaug             1.738e-03  3.468e-04   5.011 5.43e-07 ***
#   duration:monthdec            -1.035e-03  7.864e-04  -1.317 0.187989    
# duration:monthjul             1.810e-03  3.470e-04   5.216 1.83e-07 ***
#   duration:monthjun             1.716e-03  3.584e-04   4.788 1.69e-06 ***
#   duration:monthmar             1.757e-03  8.114e-04   2.165 0.030398 *  
#   duration:monthmay             1.964e-03  2.765e-04   7.101 1.23e-12 ***
#   duration:monthnov             7.014e-04  3.414e-04   2.055 0.039911 *  
#   duration:monthoct            -1.848e-03  4.048e-04  -4.564 5.02e-06 ***
#   duration:monthsep            -5.904e-04  5.004e-04  -1.180 0.238124    
# monthaug:day_of_weekmon      -5.492e-01  2.719e-01  -2.020 0.043377 *  
#   monthdec:day_of_weekmon      -3.665e-01  6.427e-01  -0.570 0.568535    
# monthjul:day_of_weekmon      -1.633e-01  2.781e-01  -0.587 0.557137    
# monthjun:day_of_weekmon      -1.676e-01  2.797e-01  -0.599 0.549060    
# monthmar:day_of_weekmon      -6.978e-01  3.694e-01  -1.889 0.058864 .  
# monthmay:day_of_weekmon      -9.245e-02  2.448e-01  -0.378 0.705645    
# monthnov:day_of_weekmon      -1.143e+00  3.071e-01  -3.722 0.000197 ***
#   monthoct:day_of_weekmon      -9.000e-01  3.472e-01  -2.592 0.009542 ** 
#   monthsep:day_of_weekmon      -6.505e-01  3.931e-01  -1.655 0.097971 .  
# monthaug:day_of_weekthu      -1.436e+00  2.561e-01  -5.608 2.05e-08 ***
#   monthdec:day_of_weekthu      -1.005e+00  6.557e-01  -1.533 0.125299    
# monthjul:day_of_weekthu      -1.202e+00  2.618e-01  -4.590 4.44e-06 ***
#   monthjun:day_of_weekthu      -8.986e-01  2.800e-01  -3.209 0.001330 ** 
#   monthmar:day_of_weekthu      -1.385e+00  3.732e-01  -3.710 0.000207 ***
#   monthmay:day_of_weekthu      -1.354e+00  2.323e-01  -5.831 5.52e-09 ***
#   monthnov:day_of_weekthu      -1.121e+00  2.792e-01  -4.014 5.98e-05 ***
#   monthoct:day_of_weekthu      -1.122e+00  3.133e-01  -3.580 0.000343 ***
#   monthsep:day_of_weekthu      -1.144e+00  3.621e-01  -3.158 0.001587 ** 
#   monthaug:day_of_weektue      -1.928e+00  2.796e-01  -6.896 5.36e-12 ***
#   monthdec:day_of_weektue      -1.210e+00  7.319e-01  -1.654 0.098223 .  
# monthjul:day_of_weektue      -1.957e+00  2.904e-01  -6.737 1.62e-11 ***
#   monthjun:day_of_weektue      -2.128e+00  2.978e-01  -7.147 8.87e-13 ***
#   monthmar:day_of_weektue      -1.674e+00  3.766e-01  -4.444 8.83e-06 ***
#   monthmay:day_of_weektue      -2.156e+00  2.651e-01  -8.133 4.18e-16 ***
#   monthnov:day_of_weektue      -2.147e+00  3.126e-01  -6.868 6.50e-12 ***
#   monthoct:day_of_weektue      -1.866e+00  3.409e-01  -5.473 4.43e-08 ***
#   monthsep:day_of_weektue      -1.353e+00  3.763e-01  -3.596 0.000324 ***
#   monthaug:day_of_weekwed      -1.851e+00  2.786e-01  -6.643 3.08e-11 ***
#   monthdec:day_of_weekwed      -8.090e-01  6.756e-01  -1.197 0.231141    
# monthjul:day_of_weekwed      -1.683e+00  2.881e-01  -5.842 5.16e-09 ***
#   monthjun:day_of_weekwed      -1.789e+00  2.987e-01  -5.989 2.11e-09 ***
#   monthmar:day_of_weekwed      -1.367e+00  4.189e-01  -3.263 0.001104 ** 
#   monthmay:day_of_weekwed      -1.879e+00  2.578e-01  -7.288 3.15e-13 ***
#   monthnov:day_of_weekwed      -2.276e+00  3.058e-01  -7.444 9.75e-14 ***
#   monthoct:day_of_weekwed      -1.951e+00  3.468e-01  -5.626 1.84e-08 ***
#   monthsep:day_of_weekwed      -1.665e+00  3.734e-01  -4.460 8.20e-06 ***
#   nr.employed:contacttelephone  4.372e-03  9.052e-04   4.830 1.36e-06 ***
#   monthaug:campaign             1.322e-01  5.391e-02   2.452 0.014194 *  
#   monthdec:campaign             1.322e-02  1.255e-01   0.105 0.916065    
# monthjul:campaign             6.003e-02  5.159e-02   1.164 0.244587    
# monthjun:campaign             1.132e-01  5.307e-02   2.133 0.032892 *  
#   monthmar:campaign            -2.717e-02  7.313e-02  -0.372 0.710218    
# monthmay:campaign             5.874e-02  5.278e-02   1.113 0.265777    
# monthnov:campaign            -2.470e-01  8.259e-02  -2.991 0.002782 ** 
#   monthoct:campaign             1.878e-02  1.080e-01   0.174 0.862008    
# monthsep:campaign             5.104e-03  9.548e-02   0.053 0.957370    
# monthaug:defaultunknown       1.071e+00  3.220e-01   3.325 0.000884 ***
#   monthdec:defaultunknown       1.336e+00  1.006e+00   1.328 0.184052    
# monthjul:defaultunknown       1.300e+00  3.093e-01   4.204 2.63e-05 ***
#   monthjun:defaultunknown       1.148e+00  3.319e-01   3.459 0.000543 ***
#   monthmar:defaultunknown       1.106e+00  6.142e-01   1.801 0.071680 .  
# monthmay:defaultunknown       9.674e-01  2.941e-01   3.289 0.001006 ** 
#   monthnov:defaultunknown       1.204e+00  3.852e-01   3.124 0.001783 ** 
#   monthoct:defaultunknown       1.580e+00  5.905e-01   2.677 0.007437 ** 
#   monthsep:defaultunknown       1.827e+00  7.544e-01   2.422 0.015447 *  
#   monthaug:defaultyes          -2.238e-01  2.411e+02  -0.001 0.999259    
# monthdec:defaultyes                  NA         NA      NA       NA    
# monthjul:defaultyes                  NA         NA      NA       NA    
# monthjun:defaultyes                  NA         NA      NA       NA    
# monthmar:defaultyes                  NA         NA      NA       NA    
# monthmay:defaultyes                  NA         NA      NA       NA    
# monthnov:defaultyes                  NA         NA      NA       NA    
# monthoct:defaultyes                  NA         NA      NA       NA    
# monthsep:defaultyes                  NA         NA      NA       NA    
# campaign:pdays                1.394e-04  5.714e-05   2.439 0.014730 *  
#   day_of_weekmon:pdays          2.163e-04  2.192e-04   0.987 0.323636    
# day_of_weekthu:pdays         -3.499e-04  2.188e-04  -1.599 0.109864    
# day_of_weektue:pdays         -1.152e-04  2.224e-04  -0.518 0.604304    
# day_of_weekwed:pdays         -2.802e-04  2.290e-04  -1.223 0.221153    
# monthaug:euribor3m            1.684e+01  2.498e+00   6.743 1.55e-11 ***
#   monthdec:euribor3m           -5.829e+00  1.671e+01  -0.349 0.727153    
# monthjul:euribor3m            1.672e+01  2.501e+00   6.684 2.33e-11 ***
#   monthjun:euribor3m            1.588e+01  2.502e+00   6.349 2.17e-10 ***
#   monthmar:euribor3m            2.033e+01  2.848e+00   7.138 9.50e-13 ***
#   monthmay:euribor3m            1.743e+01  2.500e+00   6.974 3.09e-12 ***
#   monthnov:euribor3m            1.637e+01  2.504e+00   6.537 6.26e-11 ***
#   monthoct:euribor3m            1.751e+01  2.501e+00   7.001 2.54e-12 ***
#   monthsep:euribor3m            1.624e+01  6.636e+00   2.447 0.014399 *  
#   duration:euribor3m            1.552e-04  5.728e-05   2.709 0.006751 ** 
#   jobblue-collar:euribor3m      1.147e-01  3.775e-02   3.038 0.002383 ** 
#   jobentrepreneur:euribor3m     8.106e-02  7.130e-02   1.137 0.255615    
# jobhousemaid:euribor3m       -1.115e-01  8.096e-02  -1.377 0.168519    
# jobmanagement:euribor3m       8.884e-02  4.831e-02   1.839 0.065923 .  
# jobretired:euribor3m          6.905e-02  5.614e-02   1.230 0.218709    
# jobself-employed:euribor3m   -3.030e-02  6.902e-02  -0.439 0.660654    
# jobservices:euribor3m         9.657e-02  4.662e-02   2.072 0.038306 *  
#   jobstudent:euribor3m          9.943e-02  9.557e-02   1.040 0.298138    
# jobtechnician:euribor3m      -9.405e-03  3.666e-02  -0.257 0.797508    
# jobunemployed:euribor3m      -1.103e-02  8.115e-02  -0.136 0.891921    
# jobunknown:euribor3m          2.380e-01  1.229e-01   1.938 0.052681 .  
# ---
anova(fit.interaction.6, test="Chisq")


#################################################
### Anova Test to Determine Goodness of Fit: 
#################################################
anova(fit.interaction.6, test="Chisq")
# Analysis of Deviance Table
# 
# Model: binomial, link: logit
# 
# Response: y
# 
# Terms added sequentially (first to last)
# 
# 
# Df Deviance Resid. Df Resid. Dev  Pr(>Chi)
# NULL                                41187      28999
# duration             1   4892.6     41186      24106 < 2.2e-16 ***
#   nr.employed          1   5150.7     41185      18956 < 2.2e-16 ***
#   month                9    923.4     41176      18032 < 2.2e-16 ***
#   poutcome             2    497.2     41174      17535 < 2.2e-16 ***
#   contact              1     61.2     41173      17474 5.129e-15 ***
#   job                 11     72.9     41162      17401 3.351e-11 ***
#   day_of_week          4     26.0     41158      17375 3.216e-05 ***
#   campaign             1     19.7     41157      17355 9.275e-06 ***
#   pdays                1     16.6     41156      17339 4.674e-05 ***
#   default              2     28.6     41154      17310 6.176e-07 ***
#   euribor3m            1     27.3     41153      17283 1.715e-07 ***
#   nr.employed:month    9    270.7     41144      17012 < 2.2e-16 ***
#   duration:month       9    241.6     41135      16770 < 2.2e-16 ***
#   month:day_of_week   36    189.9     41099      16580 < 2.2e-16 ***
#   nr.employed:contact  1     37.4     41098      16543 9.643e-10 ***
#   month:campaign       9     36.1     41089      16507 3.740e-05 ***
#   month:default       10     32.6     41079      16474 0.0003124 ***
#   campaign:pdays       1      8.7     41078      16466 0.0030970 **
#   day_of_week:pdays    4     11.8     41074      16454 0.0187934 *
#   month:euribor3m      9    195.8     41065      16258 < 2.2e-16 ***
#   duration:euribor3m   1      7.7     41064      16250 0.0053819 **
#   job:euribor3m       11     24.2     41053      16226 0.0117907 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


anova(fit.mainFactors, fit.interaction, test = 'LRT')
# Analysis of Deviance Table
# 
# Model 1: y ~ duration + nr.employed + month + poutcome + emp.var.rate + 
#   cons.price.idx + job + contact + euribor3m + default + day_of_week + 
#   pdays + campaign + cons.conf.idx
# Model 2: y ~ duration + nr.employed + month + poutcome + emp.var.rate + 
#   contact + job + day_of_week + campaign + pdays + default + 
#   euribor3m + nr.employed:month + duration:month + month:emp.var.rate + 
#   duration:emp.var.rate + month:day_of_week + duration:contact + 
#   nr.employed:contact + month:campaign + month:default + poutcome:emp.var.rate + 
#   campaign:pdays + duration:poutcome + emp.var.rate:default + 
#   day_of_week:pdays + contact:pdays + emp.var.rate:day_of_week + 
#   month:euribor3m + duration:euribor3m + poutcome:euribor3m + 
#   nr.employed:euribor3m + emp.var.rate:contact + contact:day_of_week + 
#   job:euribor3m + contact:campaign
# Resid. Df Resid. Dev  Df Deviance  Pr(>Chi)    
# 1     41150      17094                           
# 2     41026      16009 124   1084.9 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#################################################
### comparison of two models
#################################################
anova(fit.mainFactors, fit.interaction.6)
# Analysis of Deviance Table
# 
# Model 1: y ~ duration + nr.employed + month + poutcome + emp.var.rate +
#   cons.price.idx + job + contact + euribor3m + default + day_of_week +
#   pdays + campaign + cons.conf.idx
# Model 2: y ~ duration + nr.employed + month + poutcome + contact + job +
#   day_of_week + campaign + pdays + default + euribor3m + nr.employed:month +
#   duration:month + month:day_of_week + nr.employed:contact +
#   month:campaign + month:default + campaign:pdays + day_of_week:pdays +
#   month:euribor3m + duration:euribor3m + job:euribor3m
# Resid. Df Resid. Dev Df Deviance  Pr(>Chi)
# 1     41150      17094
# 2     41053      16226 97   868.34 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


anova(fit.interaction.6, fit.mainFactors, test = 'LRT')
# Analysis of Deviance Table
# 
# Model 1: y ~ duration + nr.employed + month + poutcome + contact + job + 
#   day_of_week + campaign + pdays + default + euribor3m + nr.employed:month + 
#   duration:month + month:day_of_week + nr.employed:contact + 
#   month:campaign + month:default + campaign:pdays + day_of_week:pdays + 
#   month:euribor3m + duration:euribor3m + job:euribor3m
# Model 2: y ~ duration + nr.employed + month + poutcome + emp.var.rate + 
#   cons.price.idx + job + contact + euribor3m + default + day_of_week + 
#   pdays + campaign + cons.conf.idx
# Resid. Df Resid. Dev  Df Deviance  Pr(>Chi)    
# 1     41053      16226                           
# 2     41150      17094 -97  -868.34 < 2.2e-16 ***


#################################################
### Analyzing Cook’s Distance:
#################################################
cooks.distance_mainFactors<-cooks.distance(fit.mainFactors)
which(cooks.distance_mainFactors>1)   #named integer(0)

cooks.distance_interaction<-cooks.distance(fit.interaction)
which(cooks.distance_interaction>1)   #24867 

cooks.distance_interaction6<-cooks.distance(fit.interaction.6)
which(cooks.distance_interaction6>1)   #named integer(0)



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
# [1] "duration"
# Wald test for duration
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          emp.var.rate + cons.price.idx + job + contact + euribor3m +
#          default + day_of_week + pdays + campaign + cons.conf.idx,
#        family = "binomial", data = bank)
# F =  3983.583  on  1  and  41150  df: p= < 2.22e-16
# [1] "nr.employed"
# Wald test for nr.employed
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          emp.var.rate + cons.price.idx + job + contact + euribor3m +
#          default + day_of_week + pdays + campaign + cons.conf.idx,
#        family = "binomial", data = bank)
# F =  2.70956  on  1  and  41150  df: p= 0.099756
# [1] "month"
# Wald test for month
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          emp.var.rate + cons.price.idx + job + contact + euribor3m +
#          default + day_of_week + pdays + campaign + cons.conf.idx,
#        family = "binomial", data = bank)
# F =  65.62167  on  9  and  41150  df: p= < 2.22e-16
# [1] "poutcome"
# Wald test for poutcome
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          emp.var.rate + cons.price.idx + job + contact + euribor3m +
#          default + day_of_week + pdays + campaign + cons.conf.idx,
#        family = "binomial", data = bank)
# F =  43.34506  on  2  and  41150  df: p= < 2.22e-16
# [1] "emp.var.rate"
# Wald test for emp.var.rate
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          emp.var.rate + cons.price.idx + job + contact + euribor3m +
#          default + day_of_week + pdays + campaign + cons.conf.idx,
#        family = "binomial", data = bank)
# F =  152.5118  on  1  and  41150  df: p= < 2.22e-16
# [1] "cons.price.idx"
# Wald test for cons.price.idx
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          emp.var.rate + cons.price.idx + job + contact + euribor3m +
#          default + day_of_week + pdays + campaign + cons.conf.idx,
#        family = "binomial", data = bank)
# F =  73.71093  on  1  and  41150  df: p= < 2.22e-16
# [1] "job"
# Wald test for job
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          emp.var.rate + cons.price.idx + job + contact + euribor3m +
#          default + day_of_week + pdays + campaign + cons.conf.idx,
#        family = "binomial", data = bank)
# F =  4.615807  on  11  and  41150  df: p= 4.5903e-07
# [1] "contact"
# Wald test for contact
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          emp.var.rate + cons.price.idx + job + contact + euribor3m +
#          default + day_of_week + pdays + campaign + cons.conf.idx,
#        family = "binomial", data = bank)
# F =  70.02117  on  1  and  41150  df: p= < 2.22e-16
# [1] "euribor3m"
# Wald test for euribor3m
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          emp.var.rate + cons.price.idx + job + contact + euribor3m +
#          default + day_of_week + pdays + campaign + cons.conf.idx,
#        family = "binomial", data = bank)
# F =  6.960387  on  1  and  41150  df: p= 0.0083365
# [1] "default"
# Wald test for default
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          emp.var.rate + cons.price.idx + job + contact + euribor3m +
#          default + day_of_week + pdays + campaign + cons.conf.idx,
#        family = "binomial", data = bank)
# F =  10.95708  on  2  and  41150  df: p= 1.7485e-05
# [1] "day_of_week"
# Wald test for day_of_week
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          emp.var.rate + cons.price.idx + job + contact + euribor3m +
#          default + day_of_week + pdays + campaign + cons.conf.idx,
#        family = "binomial", data = bank)
# F =  5.594668  on  4  and  41150  df: p= 0.0001689
# [1] "pdays"
# Wald test for pdays
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          emp.var.rate + cons.price.idx + job + contact + euribor3m +
#          default + day_of_week + pdays + campaign + cons.conf.idx,
#        family = "binomial", data = bank)
# F =  17.21098  on  1  and  41150  df: p= 3.3517e-05
# [1] "campaign"
# Wald test for campaign
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          emp.var.rate + cons.price.idx + job + contact + euribor3m +
#          default + day_of_week + pdays + campaign + cons.conf.idx,
#        family = "binomial", data = bank)
# F =  11.88966  on  1  and  41150  df: p= 0.00056503
# [1] "cons.conf.idx"
# Wald test for cons.conf.idx
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          emp.var.rate + cons.price.idx + job + contact + euribor3m +
#          default + day_of_week + pdays + campaign + cons.conf.idx,
#        family = "binomial", data = bank)
# F =  6.951141  on  1  and  41150  df: p= 0.0083797



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
# [1] "duration"
# Wald test for duration
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  170.8934  on  1  and  41053  df: p= < 2.22e-16
# [1] "nr.employed"
# Wald test for nr.employed
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  36.90044  on  1  and  41053  df: p= 1.2541e-09
# [1] "month"
# Wald test for month
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  17.23507  on  9  and  41053  df: p= < 2.22e-16
# [1] "poutcome"
# Wald test for poutcome
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  33.02613  on  2  and  41053  df: p= 4.6608e-15
# [1] "contact"
# Wald test for contact
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  24.34983  on  1  and  41053  df: p= 8.0648e-07
# [1] "job"
# Wald test for job
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  2.836523  on  11  and  41053  df: p= 0.0010263
# [1] "day_of_week"
# Wald test for day_of_week
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  23.1363  on  4  and  41053  df: p= < 2.22e-16
# [1] "campaign"
# Wald test for campaign
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  11.012  on  1  and  41053  df: p= 0.00090603
# [1] "pdays"
# Wald test for pdays
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  12.16787  on  1  and  41053  df: p= 0.00048671
# [1] "default"
# Wald test for default
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  11.60145  on  2  and  41053  df: p= 9.1829e-06
# [1] "euribor3m"
# Wald test for euribor3m
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  47.95508  on  1  and  41053  df: p= 4.425e-12
# [1] "nr.employed:month"
# Wald test for nr.employed:month
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  17.47998  on  9  and  41053  df: p= < 2.22e-16
# [1] "duration:month"
# Wald test for duration:month
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  16.11589  on  9  and  41053  df: p= < 2.22e-16
# [1] "month:day_of_week"
# Wald test for month:day_of_week
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  5.319091  on  36  and  41053  df: p= < 2.22e-16
# [1] "nr.employed:contact"
# Wald test for nr.employed:contact
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  23.33039  on  1  and  41053  df: p= 1.3692e-06
# [1] "month:campaign"
# Wald test for month:campaign
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  3.913879  on  9  and  41053  df: p= 5.4647e-05
# [1] "month:default"
# Wald test for month:default
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  2.130537  on  10  and  41053  df: p= 0.019084
# [1] "campaign:pdays"
# Wald test for campaign:pdays
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  NA  on  1  and  41053  df: p= NA
# [1] "day_of_week:pdays"
# Wald test for day_of_week:pdays
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  NA  on  4  and  41053  df: p= NA
# [1] "month:euribor3m"
# Wald test for month:euribor3m
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  NA  on  9  and  41053  df: p= NA
# [1] "duration:euribor3m"
# Wald test for duration:euribor3m
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  0.1217418  on  1  and  41053  df: p= 0.72715
# [1] "job:euribor3m"
# Wald test for job:euribor3m
# in glm(formula = y ~ duration + nr.employed + month + poutcome +
#          contact + job + day_of_week + campaign + pdays + default +
#          euribor3m + nr.employed:month + duration:month + month:day_of_week +
#          nr.employed:contact + month:campaign + month:default + campaign:pdays +
#          day_of_week:pdays + month:euribor3m + duration:euribor3m +
#          job:euribor3m, family = binomial, data = bank.2)
# F =  18.08437  on  11  and  41053  df: p= < 2.22e-16


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
# Hosmer and Lemeshow goodness of fit (GOF) test
# 
# data:  fit.mainFactors$y, fitted(fit.mainFactors)
# X-squared = 505.73, df = 8, p-value < 2.2e-16

hoslem.test(fit.mainFactors$y,fitted(fit.interaction.6),g=25)
# Hosmer and Lemeshow goodness of fit (GOF) test
# 
# data:  fit.mainFactors$y, fitted(fit.interaction.6)
# X-squared = 325.11, df = 8, p-value < 2.2e-16


#################################################
### Looking at VIF for Collinearity:
#################################################
library(car)
vif(fit.mainFactors)
# GVIF Df GVIF^(1/(2*Df))
# duration         1.241752  1        1.114339
# nr.employed    171.265895  1       13.086860
# month           60.412548  9        1.255890
# poutcome        10.861770  2        1.815412
# emp.var.rate   142.173716  1       11.923662
# cons.price.idx  67.736212  1        8.230201
# job              1.250031 11        1.010196
# contact          2.312992  1        1.520852
# euribor3m      134.624480  1       11.602779
# default          1.109608  2        1.026343
# day_of_week      1.060721  4        1.007396
# pdays            9.656016  1        3.107413
# campaign         1.051240  1        1.025300
# cons.conf.idx    5.292247  1        2.300489
vif(fit.interaction.6)
# Error in vif.default(fit.interaction.6) : 
#   there are aliased coefficients in the model
alias(fit.interaction.6)
# 
# ...
#                day_of_weekthu day_of_weektue day_of_weekwed campaign   pdays defaultunknown defaultyes
# monthdec:defaultyes  0              0              0              0        0     0              0        
# monthjul:defaultyes  0              0              0              0        0     0              0        
# monthjun:defaultyes  0              0              0              0        0     0              0        
# monthmar:defaultyes  0              0              0              0        0     0              0        
# monthmay:defaultyes  0              0              0              0        0     0              0        
# monthnov:defaultyes  0              0              0              0        0     0              1        
# monthoct:defaultyes  0              0              0              0        0     0              0  
# ...
#               monthsep:defaultunknown monthaug:defaultyes campaign:pdays day_of_weekmon:pdays
# monthdec:defaultyes  0                       0                   0              0                  
# monthjul:defaultyes  0                       0                   0              0                  
# monthjun:defaultyes  0                       0                   0              0                  
# monthmar:defaultyes  0                       0                   0              0                  
# monthmay:defaultyes  0                       0                   0              0                  
# monthnov:defaultyes  0                      -1                   0              0                  
# monthoct:defaultyes  0                       0                   0              0       



################################################# 
### Determining the Pseudo-Rsq: 
#################################################
#install.packages('pscl')
library(pscl)
pR2(fit.mainFactors)
# llh       llhNull            G2      McFadden          r2ML          r2CU 
# -8.547136e+03 -1.449936e+04  1.190445e+04  4.105164e-01  2.510082e-01  4.966274e-01 
pR2(fit.interaction.6)
# llh       llhNull            G2      McFadden          r2ML          r2CU
# -8.112964e+03 -1.449936e+04  1.277280e+04  4.404606e-01  2.666335e-01  5.275425e-01


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
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   no  yes
# no  7149  532
# yes  160  396
# 
# Accuracy : 0.916           
# 95% CI : (0.9098, 0.9219)
# No Information Rate : 0.8873          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.4907          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Sensitivity : 0.9781          
# Specificity : 0.4267          
# Pos Pred Value : 0.9307          
# Neg Pred Value : 0.7122          
# Prevalence : 0.8873          
# Detection Rate : 0.8679          
# Detection Prevalence : 0.9325          
# Balanced Accuracy : 0.7024          
# 
# 'Positive' Class : no   


# train model with main factors only
# mod_fit.mainFactors<-train(fit.mainFactors$formula,data=bank,method="glm",family="binomial",trControl=ctrl,tuneLength=5)
mod_fit.mainFactors<-train(fit.mainFactors$formula,data=training,method="glm",family="binomial",trControl=ctrl,tuneLength=5)
pred.mainFactors<-predict(mod_fit.mainFactors,newdata=testing)
confusionMatrix(data=pred.mainFactors,testing$y)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   no  yes
# no  7155  538
# yes  154  390
# 
# Accuracy : 0.916           
# 95% CI : (0.9098, 0.9219)
# No Information Rate : 0.8873          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.4872          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Sensitivity : 0.9789          
# Specificity : 0.4203          
# Pos Pred Value : 0.9301          
# Neg Pred Value : 0.7169          
# Prevalence : 0.8873          
# Detection Rate : 0.8686          
# Detection Prevalence : 0.9340          
# Balanced Accuracy : 0.6996          
# 
# 'Positive' Class : no      


# train model with interactions
mod_fit.interaction.6<-train(fit.interaction.6$formula,data=training,method="glm",family="binomial",trControl=ctrl,tuneLength=5)
pred.interaction.6<-predict(mod_fit.interaction.6,newdata=testing)
confusionMatrix(data=pred.interaction.6,testing$y)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   no  yes
# no  7139  511
# yes  170  417
# 
# Accuracy : 0.9173
# 95% CI : (0.9112, 0.9232)
# No Information Rate : 0.8873
# P-Value [Acc > NIR] : < 2.2e-16
# 
# Kappa : 0.5075
# 
# Mcnemar's Test P-Value : < 2.2e-16
# 
# Sensitivity : 0.9767
# Specificity : 0.4494
# Pos Pred Value : 0.9332
# Neg Pred Value : 0.7104
# Prevalence : 0.8873
# Detection Rate : 0.8667
# Detection Prevalence : 0.9287
# Balanced Accuracy : 0.7130
# 
# 'Positive' Class : no






#################################################
### Determining Variables of Importance:
#################################################
varImp(mod_fit.mainFactors)
# glm variable importance
# 
# only 20 most important variables shown (out of 37)
# 
# Overall
# duration            100.000
# monthmar             22.602
# emp.var.rate         19.141
# contacttelephone     14.420
# cons.price.idx       12.908
# poutcomenonexistent  12.255
# monthaug             10.624
# monthmay              8.911
# `jobblue-collar`      8.300
# pdays                 7.305
# poutcomesuccess       7.017
# monthnov              6.348
# defaultunknown        6.303
# campaign              5.959
# monthjun              5.266
# jobretired            4.754
# euribor3m             4.447
# cons.conf.idx         4.341
# jobservices           4.286
# day_of_weekwed        3.909

varImp(mod_fit.interaction.6)
# glm variable importance
# 
# only 20 most important variables shown (out of 134)
# 
# Overall
# duration                   100.00
# `duration:monthmay`         63.83
# `monthmay:day_of_weektue`   62.86
# `monthjun:day_of_weektue`   59.32
# `monthaug:day_of_weektue`   56.36
# day_of_weekwed              56.22
# monthmay                    55.67
# `nr.employed:monthmay`      55.62
# `monthaug:day_of_weekwed`   55.21
# `monthmay:day_of_weekwed`   54.12
# `monthnov:day_of_weektue`   52.83
# `monthjul:day_of_weektue`   52.64
# `monthnov:day_of_weekwed`   52.62
# `monthmar:euribor3m`        52.45
# poutcomenonexistent         51.68
# `monthmay:euribor3m`        51.58
# `monthoct:euribor3m`        51.28
# euribor3m                   50.97
# `duration:monthjul`         50.76
# day_of_weektue              49.95



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









