library(simts)
library(expsmooth)
library(quantmod)
library(pageviews)
library(robcor)

#1
data(hospital)
hosp = hospital[,8]
hosp
plot(hosp)

  #b) 
fit <- lm(hosp ~ c(1:length(hosp)))
new_ts <- gts(fit$residuals)
plot(new_ts)

  #c)
plot(new_ts)
check(fit)
plot(auto_corr(new_ts))
plot(auto_corr(new_ts, pacf = TRUE))
compare_acf(new_ts)

  #d)
mod1 = estimate(MA(9), gts(hosp))
check(mod1)

#2
today <- Sys.Date()
microsoft = getSymbols("MSFT", from = "2000-01-01", to = today)
microsoft

  #a)
prices = Cl(MSFT)
plot(prices)

  #b)
returns = na.omit(ClCl(MSFT))
returns
plot(returns)

  #c)
ret = gts(na.omit(returns))
plot(auto_corr(ret))
plot(auto_corr(ret, pacf = TRUE))

  #d)
retAbs = abs(returns)
retAbsMod = gts(na.omit(retAbs))
plot(auto_corr(retAbsMod))
plot(auto_corr(retAbsMod, pacf = TRUE))

  #e)
    #Returns
mod1 = estimate(AR(36), returns)
check(mod1)
    #ReturnsAbs
mod2 = estimate(AR(35), retAbs)
check(mod2)

#3
  #a
data(utility)
utility
plot(utility)
fit <- lm(utility ~ c(1:length(utility)))
resUtilO = gts(fit$residuals)
plot(resUtilO)

  #b
    #a
resUtil = gts(resUtilO[1:3000,])
mod1 = estimate(AR(1), resUtil)
check(mod1)

mod2 = estimate(AR(27), resUtil)
check(mod2)

    #b
utilPrediction = predict(mod2, n.ahead=24)
utilPrediction

    #c
util = gts(resUtilO[3001:3024,])
median(abs(utilPrediction$pred - util))

    #d
predictCI = utilPrediction$CI0.95
empirical = util <= utilPrediction$CI0.95[,2] & util >= utilPrediction$CI0.95[,1]
empirical
sum(empirical)/length(empirical)

#4
data(ukcars)
ukcars
  #1
plot(ukcars)
ukcars = gts(ukcars)
plot(ukcars)
fit <- lm(ukcars ~ c(1:length(ukcars)))
ukcarsRes = gts(fit$residuals)
plot(ukcarsRes)

  #2
    #a
CI2 = matrix(0,8,2)
mod1 = estimate(AR(8), ukcars, demean = FALSE)
c("phi" = mod1$mod$coef[1:8], "sigma2" = sqrt(mod1$mod$sigma2))
for(i in 1:8){
  CI2[i,1] = mod1$mod$coef[i] + (-1)*1.96*as.numeric(sqrt(mod1$mod$var.coef[i,i]))
  CI2[i,2] = mod1$mod$coef[i] + (1)*1.96*as.numeric(sqrt(mod1$mod$var.coef[i,i]))
}
colnames(CI2) <- c("CI.95.Low","CI.95.High")
CI2
    #b
B = 500
mat = matrix(NA,B,2)
model = AR(phi = mod1$mod$coef[1:8], sigma2 = fit$mod$sigma2)

for(i in seq_len(B)){
  set.seed(B+i)
  mat[i,] = tryCatch(estimate(AR(8), ukcars, demean = FALSE)$mod$coef,
           error = function(e) NA)
}

mod = estimate(AR(8), ukcars)
mod

B = 500
mat = matrix(0,B,8)
for (i in 1:B){
  for (j in 3:52){
    mat[i,j] = 0.75*mat[i,(j-1)] + 0.2*mat[i,(j-2)] + rnorm(1)
  }
}


#5
set.seed(2)
Xt = gen_gts(n = 200, AR(phi = c(0.75, 0.2), sigma2 = 1))


B = 5000
mat = matrix(0,B,52)
mat[,1] = rep(Xt[199], B)
mat[,2] = rep(Xt[200], B)
mat
for (i in 1:B){
  for (j in 3:52){
    mat[i,j] = 0.75*mat[i,(j-1)] + 0.2*mat[i,(j-2)] + rnorm(1)
  }
}

  #2
set.seed(2)
Xt = gen_gts(n = 200, AR(phi = c(0.75, 0.2), sigma2 = 1))
pointFor = vector()

B = 5000
mat = matrix(0,B,12)
mat[,1] = rep(Xt[199], B)
mat[,2] = rep(Xt[200], B)
for (i in 3:12){
  for (j in 1:B){
    mat[j,i] = 0.75*mat[j,(i-1)] + 0.2*mat[j,(i-2)]
  }
  pointFor[i-2] = mean(mat[,i])
}
pointFor

  #3
stderr <- vector()
CI <- matrix(0,10,2)
CI
for (i in 3:12){
  stderr[i] = sd(mat[,i])/sqrt(length(mat[,i]))
  CI[(i-2),1] = pointFor[i] + (-1)*1.96*stderr[i]
  CI[(i-2),2] = pointFor[i] + (1)*1.96*stderr[i]
}
colnames(CI) <- c("CI.95.Low","CI.95.High")
CI

  #4
set.seed(2)
Yt = gen_gts(n = 100, AR(phi = c(0.8), sigma2 = 0.5))
pointFor = vector()

B = 5000
mat = matrix(0,B,22)
mat[,1] = rep(Xt[199], B)
mat[,2] = rep(Xt[200], B)
for (i in 3:22){
  for (j in 1:B){
    mat[j,i] = 0.8*mat[j,(i-1)]
  }
  pointFor[i-2] = mean(mat[,i])
}
pointFor

stderr <- vector()
CI <- matrix(0,20,2)
CI
for (i in 3:22){
  stderr[i] = sd(mat[,i])/sqrt(length(mat[,i]))
  CI[(i-2),1] = pointFor[i] + (-1)*1.96*stderr[i]
  CI[(i-2),2] = pointFor[i] + (1)*1.96*stderr[i]
}
colnames(CI) <- c("CI.95.Low","CI.95.High")
CI

#6
cheese= article_pageviews(article="Cheese",start= "2018080100", end= "2018093000") 
cheese
cheeseg=gts(cheese$views)
plot(cheeseg)

diffch=diff(cheese$views)
diff_ch=gts(diffch)
plot(diff_ch)
cheese= article_pageviews(article="Cheese",start= "2018080100", end= "2018093000") 

plot(auto_corr(diff_ch))
plot(auto_corr(diff_ch,pacf=TRUE))

robacf(diff_ch)

modelmle <- estimate(AR(1), diff_ch, method="mle")
check(modelmle)
modelg= estimate(AR(1), diff_ch, method="gmwm")
modelg
check(modelg)

pointFor = vector()

pred = predict(modelg, n.ahead = 28)
predpredCon = pred$CI0.95

median(abs(pred$pred - cheese$views))

emp = pred$CI0.95[,2] & pred$CI0.95[,1]
emp
sum(emp)/length(emp)




