devtools::install_github("SMAC-Group/simts", force = TRUE)

library(simts)
library(pageviews)
library(astsa)
library(gmailr)
library(forecast463)

use_secret_file('stat463.json')

day <- Sys.Date()
day <- gsub("-","",day)
day <- paste0(day,"12") ## todays date
day2 <- as.character(as.numeric(day) - 0002000000) ## two years ago from today

#Collect Data from pageviews
wikiMobile = project_pageviews(granularity = "daily", start = "2018080912", end = day, platform = "mobile-app")
wikiDesktop = project_pageviews(granularity = "daily", start = "2018080912", end = day, platform = "desktop")
ArticleSilvio = article_pageviews(article = "Silvio_Berlusconi", start = day2, end = day)
ArticleBeyonce = article_pageviews(article = "Beyonce", start = day2, end = day)
ArticleChomsky = article_pageviews(article = "Noam_Chomsky", start = "2018080912", end = day)
ArticleLazio = article_pageviews(article = "SS_Lazio", start = "2018080912", end = day)
ArticleThanksgiving = article_pageviews(article = "Thanksgiving", start = "2017110112", end = "2017113011")
ArticleThanksgiving2 = article_pageviews(article = "Thanksgiving", start = "2018100112", end = day)

#Set variable with pageviews
wikiMobileViews = gts(wikiMobile$views)
wikiDesktopViews = gts(wikiDesktop$views)
ArticleSilvioViews = gts(ArticleSilvio$views)
ArticleBeyonceViews = gts(ArticleBeyonce$views)
ArticleChomskyViews = gts(ArticleChomsky$views)
ArticleLazioViews = gts(ArticleLazio$views)
ArticleThanksgivingViews = gts(ArticleThanksgiving$views)
ArticleThanksgivingViews2 = gts(ArticleThanksgiving2$views)

#Wiki Mobile Model
wikiMobile = project_pageviews(granularity = "daily", start = "2018080812", end = "2018110811", platform = "mobile-app")
wikiMobileViews = gts(wikiMobile$views)

plot(wikiMobileViews)
mod1 = estimate(AR(1), wikiMobileViews)
check(mod1)

mod2 = estimate(AR(7), wikiMobileViews)
check(mod2) 

mod3 = estimate(AR(13), wikiMobileViews)
check(mod3)

mod4 = estimate(AR(14), wikiMobileViews)
check(mod4)

mod5 = estimate(MA(1), wikiMobileViews)
check(mod5)

mod6 = estimate(MA(14), wikiMobileViews)
check(mod6)

mod7 = estimate(SARMA(ar=1, ma =1, s = 7), wikiMobileViews)
check(mod7)

evaluate(list(AR(13), AR(14), MA(14)), wikiMobileViews, criterion = "MAPE")
select(AR(14), wikiMobileViews, include.mean = TRUE)
mobile_mape = MAPE(AR(15), wikiMobileViews, start = 0.95)

mobile_prediction = predict(mod3, n.ahead = 1)
mobile_prediction

mobile_pred = as.numeric(mobile_prediction$pred)
mobile_ci = as.numeric(mobile_prediction$CI0.95) 
mobile_forecasts = list(mobile_pred = mobile_pred, mobile_ci = mobile_ci)

#Wiki Desktop Model
plot(wikiDesktopViews)

mod1 = estimate(AR(1), wikiDesktopViews)
check(mod1)

mod2 = estimate(AR(2), wikiDesktopViews)
check(mod2)

mod3 = estimate(AR(3), wikiDesktopViews)
check(mod3)

mod4 = estimate(AR(4), wikiDesktopViews)
check(mod4)

mod5 = estimate(AR(5), wikiDesktopViews)
check(mod5)

mod6 = estimate(MA(1), wikiDesktopViews)
check(mod6)

mod7 = estimate(MA(4), wikiDesktopViews)
check(mod7)

mod8 = estimate(SARMA(ar = 2, ma=1, s=7), wikiDesktopViews)
check(mod8)

select(AR(5), wikiDesktopViews, include.mean = TRUE)
select(MA(4), wikiDesktopViews, include.mean = TRUE)

Desktop_mape = MAPE(AR(5), ArticleChomskyViews, start = 0.80)
Desktop_mape = MAPE(MA(4), ArticleChomskyViews, start = 0.80)

desktop_prediction = predict(mod7, n.ahead = 1)

desktop_pred = as.numeric(desktop_prediction$pred)
desktop_ci = as.numeric(desktop_prediction$CI0.95) 
desktop_forecasts = list(desktop_pred = desktop_pred, desktop_ci = desktop_ci)

#Silvio Model
# AR MODEL
plot(ArticleSilvioViews)
### test for stationarity
Box.test(ArticleSilvioViews,lag = 20,type = "Ljung-Box")
adf.test(ArticleSilvioViews,alternative = "stationary")
kpss.test(ArticleSilvioViews)
### All three tests show stationarity

pacf(ArticleSilvioViews, lag.max=20)
best_Silvio <- select(AR(20),ArticleSilvioViews,include.mean = FALSE)
mod2 = estimate(AR(length(best_Silvio$coef)), ArticleSilvioViews)
check(mod2)
x <- predict(mod2, n.ahead = 1)

silvio_pred = as.numeric(x$pred) 
silvio_ci = as.numeric(x$CI0.95) 
silvio_forecasts = list(silvio_pred = silvio_pred, silvio_ci = silvio_ci)

#Beyonce Model
# AR MODELS
plot(ArticleBeyonceViews)
Box.test(ArticleBeyonceViews,lag = 20,type = "Ljung-Box")
adf.test(ArticleBeyonceViews,alternative = "stationary")
kpss.test(ArticleBeyonceViews)
best_Beyonce <- select(AR(20),ArticleBeyonceViews,include.mean = FALSE)
mod3 = estimate(AR(length(best_Beyonce$coef)), ArticleBeyonceViews)
check(mod3)
y <- predict(mod3, n.ahead = 1)

beyonce_pred = as.numeric(y$pred)
beyonce_ci = as.numeric(y$CI0.95)
beyonce_forecasts = list(beyonce_pred = beyonce_pred, beyonce_ci = beyonce_ci)

#Chomsky Model
plot(ArticleChomskyViews)
mod1 = estimate(AR(1), ArticleChomskyViews, method = "rgmwm")
check(mod1)

mod2 = estimate(AR(7), ArticleChomskyViews)
check(mod2)

mod3 = estimate(AR(3), ArticleChomskyViews)
check(mod3)

mod4 = estimate(MA(1), ArticleChomskyViews)
check(mod4)

mod5 = estimate(MA(2), ArticleChomskyViews)
check(mod5)

select
select(AR(3), ArticleChomskyViews, include.mean = TRUE)
select(MA(2), ArticleChomskyViews, include.mean = TRUE)

chomsky_mape = MAPE(AR(3), ArticleChomskyViews, start = 0.80)
chomsky_prediction = predict(mod3, n.ahead = 1)

chomsky_pred = as.numeric(chomsky_prediction$pred) 
chomsky_ci = as.numeric(chomsky_prediction$CI0.95)
chomsky_forecasts = list(chomsky_pred = chomsky_pred, chomsky_ci = chomsky_ci)

#Lazio Model
plot(ArticleLazioViews)
mod1 = estimate(AR(1), ArticleLazioViews)
check(mod1)

mod2 = estimate(AR(3), ArticleLazioViews)
check(mod2)

mod3 = estimate(AR(4), ArticleLazioViews)
check(mod3)

mod4 = estimate(MA(1), ArticleLazioViews)
check(mod4)

mod5 = estimate(MA(7), ArticleLazioViews)
check(mod5)

mod6 = estimate(ARMA(3,7), ArticleLazioViews)
check(mod6)

evaluate(list(AR(1), AR(3), AR(4), MA(1), MA(4), MA(7), ARMA(3,7)), ArticleLazioViews, criterion = "MAPE")
evaluate(list(AR(1), AR(3), AR(4), MA(1), MA(4), MA(7)), ArticleLazioViews)

select(AR(10), ArticleLazioViews, include.mean = TRUE)
select(MA(10), ArticleLazioViews, include.mean = TRUE)

Lazio_mape = MAPE(AR(4), ArticleLazioViews, start = 0.8)
Lazio_mape = MAPE(MA(4), ArticleLazioViews, start = 0.8)

lazio_prediction = predict(mod6, n.ahead=1)

lazio_pred = as.numeric(lazio_prediction$pred) 
lazio_ci = as.numeric(lazio_prediction$CI0.95) 
lazio_forecasts = list(lazio_pred = lazio_pred, lazio_ci = lazio_ci)

#Thanksgiving Model
ArticleThanksgiving = article_pageviews(article = "Thanksgiving", start = "2017110112", end = "2017113011")
ArticleThanksgiving2 = article_pageviews(article = "Thanksgiving", start = "2018110112", end = day)
ArticleThanksgivingViews = gts(ArticleThanksgiving$views)
ArticleThanksgivingViews2 = gts(ArticleThanksgiving2$views)

plot(ArticleThanksgivingViews)
plot(ArticleThanksgivingViews2)
mod1 = estimate(AR(1), ArticleThanksgivingViews2)
check(mod1)

mod2 = estimate(MA(2), ArticleThanksgivingViews2)
check(mod2)

mod3 = estimate(ARMA(1,2), ArticleThanksgivingViews2)
check(mod3)

mod4 = estimate(SARMA(1,2, s = 6), ArticleThanksgivingViews2)
check(mod4)

evaluate(list(AR(1), MA(2), ARMA(1,2), SARMA(1,2, s = 6)), ArticleThanksgivingViews2, criterion = "MAPE")
select(AR(5), ArticleThanksgivingViews, include.mean = TRUE)

Thanksgiving_mape = MAPE(MA(5), ArticleThanksgivingViews2, start = 0.8)

thanksgiving_prediction = predict(mod2, n.ahead = 1)

thanks_pred = as.numeric(thanksgiving_prediction$pred) 
thanks_ci = as.numeric(thanksgiving_prediction$CI0.95) 
thanks_forecasts = list(thanks_pred = thanks_pred, thnaks_ci = thanks_ci)

#Prediction Formation
prediction = list(mobile = mobile_forecasts,
                  desktop = desktop_forecasts, 
                  silvio = silvio_forecasts, 
                  beyonce = beyonce_forecasts, 
                  chomsky = chomsky_forecasts, 
                  lazio = lazio_forecasts, 
                  thanks = thanks_forecasts)
prediction

#Email Information and check to see if things are formatted correctly 
group = 13
from = "psu.forecasting.group.13@gmail.com"
key = "nbzJLbdZrZ0779X" 
credential_OK = check_credentials(group = group, from = from, key = key)
prediction_OK = check_prediction(prediction = prediction)

send_prediction(group = group, prediction = prediction, from = from, key = key)

#Actuals
as.numeric(mobile_prediction$pred)
wikiMobile
as.numeric(mobile_prediction$CI0.95)

as.numeric(desktop_prediction$pred)
wikiDesktop
as.numeric(desktop_prediction$CI0.95)

as.numeric(x$pred) 
ArticleSilvio
as.numeric(x$CI0.95)

as.numeric(y$pred)
ArticleBeyonce
as.numeric(y$CI0.95)

as.numeric(chomsky_prediction$pred)
ArticleChomsky
as.numeric(chomsky_prediction$CI0.95)

as.numeric(lazio_prediction$pred)
ArticleLazio
as.numeric(lazio_prediction$CI0.95)

as.numeric(thanksgiving_prediction$pred) 
ArticleThanksgiving2
as.numeric(thanksgiving_prediction$CI0.95)







