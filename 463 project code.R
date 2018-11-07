install.packages(c("RcppArmadillo","devtools","knitr","rmarkdown"))
devtools::install_github("SMAC-Group/simts")

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

#Wiki Desktop Model
plot(wikiDesktopViews)
mod1 = estimate(AR(10), wikiDesktopViews)
check(mod1)

select(AR(10), wikiDesktopViews, include.mean= TRUE)
desktop_prediction = predict(mod1, n.ahead = 1)

desktop_mape= MAPE(AR(10), wikiDesktopViews, start=.8)
desktop_prediction= predict(mod1, n.ahead=1)

desktop_pred = as.numeric(desktop_prediction$pred)
desktop_ci = as.numeric(desktop_prediction$CI0.95) 
desktop_forecasts = list(desktop_pred = desktop_pred, desktop_ci = desktop_ci)


#Lazio Model
plot(ArticleLazioViews)
mod2 = estimate(AR(10), ArticleLazioViews)
check(mod2)

select(AR(10), ArticleLazioViews, include.mean= TRUE)
lazio_prediction = predict(mod2, n.ahead = 1)

Lazio_mape= MAPE(AR(10), wikiDesktopViews, start=.8)

lazio_pred = as.numeric(lazio_prediction$pred) 
lazio_ci = as.numeric(lazio_prediction$CI0.95) 
lazio_forecasts = list(lazio_pred = lazio_pred, lazio_ci = lazio_ci)
