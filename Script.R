library(ggplot2)
library(dplyr)
library(countrycode)
library(plm)

#Bring in labour productivity data
GDPh <- read.csv("DP_LIVE_16062020201012437.csv")
names(GDPh)[1] <- "Country"
names(GDPh)[7] <- "GDP.h"
GDPh <- GDPh[-c(2:5,8)]

#Bring in average annual wages
Wage <- read.csv("DP_LIVE_16062020201639206.csv")
names(Wage)[1] <- "Country"
names(Wage)[7] <- "Wage"
Wage <- Wage[-c(2:5,8)]

#Bring in average annual hours worked
Hours <- read.csv("DP_LIVE_16062020202140847.csv")
names(Hours)[1] <- "Country"
names(Hours)[7] <- "Hours.a"
Hours <- Hours[-c(2:5,8)]

Comb <- merge(GDPh, Hours)
Comb <- merge(Comb,Wage)

#Create notin function
`%notin%` <- Negate(`%in%`)

#Remove OECD observations
Comb <- subset(Comb, Country %notin% c("OECD"))

Comb$H_wage <- Comb$Wage/Comb$Hours.a

ols <- lm(H_wage~GDP.h, Comb)
summary(ols)

ggplot(Comb, aes(x=GDP.h, y=H_wage)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x = "GDP Per Hour Worked",
       y = "Hourly Wage")

#Plot
ggplot(Comb, aes(x=GDP.h, y=H_wage)) +
  geom_point() +
  facet_wrap(.~ Country, scales = "free") +
  labs(title = "Country Panel",
       caption = "Source: OECD",
       y = "Average Wage",
       x = "GDP/Hour Worked")

is.pbalanced(Comb) #False
length(unique(Comb$Country))

Comb <- make.pbalanced(Comb, balance.type = "shared.individuals")
length(unique(Comb$Country))

f <- plm(H_wage~GDP.h, data=Comb, model="within")
summary(f)

#Create new plot
plot(f,
     pooling=F,
     within=T)

pFtest(f, ols)

#Random effects
R<- plm(H_wage~GDP.h, data=Comb, model="random")
summary(R)

#Test random vs fixed
phtest(f, R)

#Time fixed effects
ft <- plm(H_wage ~ GDP.h + factor(TIME), data=Comb, model="within")
summary(ft)
