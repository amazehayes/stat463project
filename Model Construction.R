#Wiki Mobile
wikiMobile = project_pageviews(granularity = "daily", start = "2018080812", end = "2018110811", platform = "mobile-app")
wikiMobileViews = gts(wikiMobile$views)


plot(wikiMobileViews)
mod1 = estimate(AR(1), wikiMobileViews, method = "rgmwm")
check(mod1)

mod7 = estimate(AR(7), wikiMobileViews, method = "rgmwm")
check(mod7) 

mod2 = estimate(AR(13), wikiMobileViews)
check(mod2)

mod6 = estimate(AR(14), wikiMobileViews)
check(mod6)

mod3 = estimate(MA(1), wikiMobileViews)
check(mod3)

mod4 = estimate(MA(15), wikiMobileViews)
check(mod4)

mod5 = estimate(ARMA(ar=6, ma =15), wikiMobileViews)
check(mod5)

select(AR(14), wikiMobileViews, include.mean = TRUE)
mobile_mape = MAPE(AR(15), wikiMobileViews, start = 0.95)

mobile_prediction = predict(mod2, n.ahead = 1)

#Wiki Desktop
plot(wikiDesktopViews)

mod1 = estimate(AR(1), wikiDesktopViews)
check(mod1)

mod2 = estimate(AR(2), wikiDesktopViews)
check(mod1)

mod3 = estimate(AR(3), wikiDesktopViews)
check(mod1)

mod4 = estimate(AR(4), wikiDesktopViews)
check(mod1)

mod5 = estimate(AR(5), wikiDesktopViews)
check(mod1)

mod6 = estimate(MA(1), wikiDesktopViews)
check(mod1)

mod7 = estimate(MA(4), wikiDesktopViews)
check(mod1)

select(AR(5), wikiDesktopViews, include.mean = TRUE)
select(MA(4), wikiDesktopViews, include.mean = TRUE)

Desktop_mape = MAPE(AR(5), ArticleChomskyViews, start = 0.80)
Desktop_mape = MAPE(MA(4), ArticleChomskyViews, start = 0.80)

#Silvio
plot(ArticleSilvioViews)
mod2 = estimate(AR(1), ArticleSilvioViews)
check(mod2)

#Beyonce
plot(ArticleBeyonceViews)
mod3 = estimate(AR(1), ArticleBeyonceViews)
check(mod3)

#Chomsky
plot(ArticleChomskyViews)
mod4 = estimate(AR(1), ArticleChomskyViews, method = "rgmwm")
check(mod4)

mod1 = estimate(AR(7), ArticleChomskyViews)
check(mod4)

mod2 = estimate(AR(3), ArticleChomskyViews)
check(mod4)

mod3 = estimate(MA(1), ArticleChomskyViews)
check(mod5)

mod4 = estimate(MA(2), ArticleChomskyViews)
check(mod5)

select
select(AR(3), ArticleChomskyViews, include.mean = TRUE)
select(MA(2), ArticleChomskyViews, include.mean = TRUE)

chomsky_mape = MAPE(AR(3), ArticleChomskyViews, start = 0.80)
chomsky_mape = MAPE(MA(3), ArticleChomskyViews, start = 0.80)

#Lazio
plot(ArticleLazioViews)
mod5 = estimate(AR(1), ArticleLazioViews)
check(mod5)

mod5 = estimate(AR(3), ArticleLazioViews)
check(mod5)

mod5 = estimate(AR(4), ArticleLazioViews)
check(mod5)

mod2 = estimate(MA(1), ArticleLazioViews)
check(mod2)

mod2 = estimate(MA(4), ArticleLazioViews)
check(mod2)

select(AR(10), ArticleLazioViews, include.mean = TRUE)
select(MA(10), ArticleLazioViews, include.mean = TRUE)

Lazio_mape = MAPE(AR(4), ArticleLazioViews, start = 0.95)
Lazio_mape = MAPE(MA(4), ArticleLazioViews, start = 0.95)

#Thanksgiving
ArticleThanksgiving = article_pageviews(article = "Thanksgiving", start = "2017110112", end = "2017113011")
ArticleThanksgivingViews = gts(ArticleThanksgiving$views)

plot(ArticleThanksgivingViews)
mod1 = estimate(AR(1), ArticleThanksgivingViews, method = "rgmwm")
check(mod1)

mod2 = estimate(MA(1), ArticleThanksgivingViews)
check(mod2)

select(AR(5), ArticleThanksgivingViews, include.mean = TRUE)

Thanksgiving_mape = MAPE(AR(2), ArticleThanksgivingViews, start = 0.8)

thanksgiving_prediction = predict(mod1, n.ahead = 1)
