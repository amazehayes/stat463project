
library(simts)
library(pageviews)
library(astsa)
library(gmailr)
library(forecast463)

use_secret_file('stat463.json')

#Collect Data from pageviews
wikiMobile = project_pageviews(granularity = "daily", start = "2018050612", end = "2018110611", platform = "mobile-app")
wikiDesktop = project_pageviews(granularity = "daily", start = "2018050612", end = "2018110611", platform = "desktop")
ArticleSilvio = article_pageviews(article = "Silvio_Berlusconi", start = "2018050612", end = "2018110611")
ArticleBeyonce = article_pageviews(article = "Beyonce", start = "2018050612", end = "2018110611")
ArticleChomsky = article_pageviews(article = "Noam_Chomsky", start = "2018050612", end = "2018110611")
ArticleLazio = article_pageviews(article = "SS_Lazio", start = "2018050612", end = "2018110611")
ArticleThanksgiving = article_pageviews(article = "Thanksgiving", start = "2018050612", end = "2018110611")

#Set variable with pageviews
wikiMobileViews = gts(wikiMobile$views)
wikiDesktopViews = gts(wikiDesktop$views)
ArticleSilvioViews = gts(ArticleSilvio$views)
ArticleBeyonceViews = gts(ArticleBeyonce$views)
ArticleChomskyViews = gts(ArticleChomsky$views)
ArticleLazioViews = gts(ArticleLazio$views)
ArticleThanksgivingViews = gts(ArticleThanksgiving$views)

#Wiki Mobile Model
plot(wikiMobileViews)
mod = estimate(AR(20), wikiMobileViews)
check(mod)
select(AR(20), wikiMobileViews, include.mean = TRUE)
mobile_prediction = predict(mod, n.ahead = 1)

mobile_pred = as.numeric(mobile_prediction$pred)
mobile_ci = as.numeric(mobile_prediction$CI0.95) 
mobile_forecasts = list(mobile_pred = mobile_pred, mobile_ci = mobile_ci)

#Wiki Desktop Model
mod1 = estimate(AR(1), wikiDesktopViews)
desktop_prediction = predict(mod1, n.ahead = 1)

desktop_pred = as.numeric(desktop_prediction$pred)
desktop_ci = as.numeric(desktop_prediction$CI0.95) 
desktop_forecasts = list(desktop_pred = desktop_pred, desktop_ci = desktop_ci)

#Silvio Model
mod2 = estimate(AR(1), ArticleSilvioViews)
silvio_prediction = predict(mod2, n.ahead = 1)

silvio_pred = as.numeric(silvio_prediction$pred) 
silvio_ci = as.numeric(silvio_prediction$CI0.95) 
silvio_forecasts = list(silvio_pred = silvio_pred, silvio_ci = silvio_ci)

#Beyonce Model
mod3 = estimate(AR(1), ArticleBeyonceViews)
beyonce_prediction = predict(mod3, n.ahead = 1)

beyonce_pred = as.numeric(beyonce_prediction$pred)
beyonce_ci = as.numeric(beyonce_prediction$CI0.95)
beyonce_forecasts = list(beyonce_pred = beyonce_pred, beyonce_ci = beyonce_ci)

#Chomsky Model
mod4 = estimate(AR(1), ArticleChomskyViews)
chomsky_prediction = predict(mod4, n.ahead = 1)

chomsky_pred = as.numeric(chomsky_prediction$pred) 
chomsky_ci = as.numeric(chomsky_prediction$CI0.95)
chomsky_forecasts = list(chomsky_pred = chomsky_pred, chomsky_ci = chomsky_ci)

#Lazio Model
mod5 = estimate(AR(1), ArticleLazioViews)
lazio_prediction = predict(mod5, n.ahead = 1)

lazio_pred = as.numeric(lazio_prediction$pred) 
lazio_ci = as.numeric(lazio_prediction$CI0.95) 
lazio_forecasts = list(lazio_pred = lazio_pred, lazio_ci = lazio_ci)

#Thanksgiving Model
mod6 = estimate(AR(1), ArticleThanksgivingViews)
thanks_prediction = predict(mod6, n.ahead = 1)

thanks_pred = as.numeric(thanks_prediction$pred) 
thanks_ci = as.numeric(thanks_prediction$CI0.95) 
thanks_forecasts = list(thanks_pred = thanks_pred, thanks_ci = thanks_ci)

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
