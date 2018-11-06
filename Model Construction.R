#Wiki Mobile
plot(wikiMobileViews)
mod = estimate(AR(1), wikiMobileViews)
check(mod)

mod = estimate(AR(15), wikiMobileViews)
check(mod)

select(AR(15), wikiMobileViews, include.mean = TRUE)
mobile_prediction = predict(mod, n.ahead = 1)

mobile_mape = MAPE(AR(15), wikiMobileViews, start = 0.8)

mobile_prediction = predict(mod, n.ahead = 1)

#Wiki Desktop
plot(wikiDesktopViews)
mod1 = estimate(AR(1), wikiDesktopViews)
check(mod1)

mod1 = estimate(AR(7), wikiDesktopViews)
check(mod1)

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
mod4 = estimate(AR(1), ArticleChomskyViews)
check(mod4)

mod4 = estimate(AR(19), ArticleChomskyViews)
check(mod4)

select(AR(19), ArticleChomskyViews, include.mean = TRUE)

mod4 = estimate(AR(15), ArticleChomskyViews)
check(mod4)

chomsky_mape = MAPE(AR(16), ArticleChomskyViews, start = 0.8)

#Lazio
plot(ArticleLazioViews)
mod5 = estimate(AR(1), ArticleLazioViews)
check(mod5)

mod5 = estimate(AR(9), ArticleLazioViews)
check(mod5)

select(AR(9), ArticleLazioViews, include.mean = TRUE)

Lazio_mape = MAPE(AR(9), ArticleLazioViews, start = 0.8)


#Thanksgiving
plot(ArticleThanksgivingViews)
mod6 = estimate(AR(1), ArticleThanksgivingViews)
check(mod6)

mod6 = estimate(AR(3), ArticleThanksgivingViews)
check(mod6)

select(AR(3), ArticleThanksgivingViews, include.mean = TRUE)

Thanksgiving_mape = MAPE(AR(3), ArticleThanksgivingViews, start = 0.8)
