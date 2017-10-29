 pgc = c(10, 12, 14, 20, 25)
 imp = c(7, 8.5, 9.9, 13.2, 16.1)
 cor(pgc, imp)
mod1 = lm(pgc~imp)
summary(mod1)
str(mod1)
mod1$fitted.values

cor(USArrests)
summary( lm(USArrests$UrbanPop~USArrests$Rape+USArrests$Assault) )