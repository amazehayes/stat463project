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
mod2 = estimate(AR(7), ArticleSilvioViews)
check(mod2)

#Beyonce

#Chomsky

#Lazio

#Thanksgiving