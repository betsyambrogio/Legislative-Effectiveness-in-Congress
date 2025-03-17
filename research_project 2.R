setwd("/Users/betsyambrogio/Desktop/POLI 100X/data")

house_les <- read.csv("LES_house_105_116.csv")
senate_les <- read.csv("LES_senate_105_116.csv")

summary(house_les$les)
summary(senate_les$les)

fit_house <- lm(les ~ majority + all_bills + chair + congress + leslag + state + dem + female + votepct + dwnom1 + state_leg + cd, data=house_les)
summary (fit_house)

fit_senate <- lm(les~ majority + all_bills + chair + congress + leslag + state  + dem + female + votepct + dwnom1 + state_leg, data=senate_les)
summary(fit_senate)

house_les$house_int_term <- house_les$majority * house_les$all_bills
senate_les$senate_int_term <- senate_les$majority * senate_les$all_bills

het_fit_house <- lm(les ~ house_int_term + majority + all_bills, data = house_les )
summary(het_fit_house)

het_fit_senate <- lm(les ~ senate_int_term + majority + all_bills, data = senate_les)
summary(het_fit_senate)

plot(house_les$les,  house_les$all_bills, pch=16,main="Majority and All Bills Introduced Effect on Legislative Effectiveness Scores (House)",
     xlab="All Bills Introduced",ylab="Legislative Effectiveness Score",
     col=ifelse(house_les$majority==1,"red","blue"))

plot(senate_les$les,  senate_les$all_bills, pch=16,main="Majority and All Bills Introduced Effect on Legislative Effectiveness Scores (Senate)",
     xlab="All Bills Introduced",ylab="Legislati ve Effectiveness Score",
     col=ifelse(senate_les$majority==1,"red","blue"))
