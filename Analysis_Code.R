library(lme4)
library(haven)
library(tidyverse)
library(glmmTMB)
library(car)
library(Hmisc)
library(corrplot)
library(summarytools)

data_updated <- read_csv("")
names(data)

data2 <- (data_updated)
colnames(data2)
data2 <- data2[-c(1,5,13)]
data2 <- na.omit(data2)

data2$mcs2 <- ifelse(data2$mcs == 1, 0,
                    ifelse(data2$mcs >=2, 1, data2$mcs))

data2$foc_visits3 <- ifelse(data2$foc_visits == 0, 0,
                    ifelse(data2$foc_visits ==1, 1, 
                           ifelse(data2$foc_visits >1, 2, data2$foc_visits)))

data2$foc_visits2 <- ifelse(data2$foc_visits == 0, 0,
                            ifelse(data2$foc_visits >=1, 1, data2$foc_visits))

data2$traffic2 <- ifelse(data2$traffic >= 0 & data2$traffic <= 7, 1, 
                         ifelse(data2$traffic >= 8 & data2$traffic <= 27, 2,
                                ifelse(data2$traffic >= 28 & data2$traffic <= 69, 3,
                                       ifelse(data2$traffic >= 70, 4, data2$traffic))))

data2$trafficx <- ifelse(data2$traffic == 0, 0,
                            ifelse(data2$traffic >=1, 1, data2$fv_traffic))

data2$fv_trafficx <- ifelse(data2$fv_traffic == 0, 0,
                            ifelse(data2$fv_traffic >=1, 1, data2$fv_traffic))

data2$fv_traffic2 <- ifelse(data2$fv_traffic >= 0 & data2$fv_traffic <= 12, 1,
  ifelse(data2$fv_traffic >= 13 & data2$fv_traffic <= 41, 2,
  ifelse(data2$fv_traffic >41 , 3, data2$fv_traffic)))

data2$fv_traffic3 <- ifelse(data2$fv_traffic >= 0 & data2$fv_traffic <=9, 1,
                            ifelse(data2$fv_traffic >=10 & data2$fv_traffic <=27, 2,
                                   ifelse(data2$fv_traffic >=28 & data2$fv_traffic <=73, 3,
                                          ifelse(data2$fv_traffic >73, 4, data2$fv_traffic))))

data2$hotspot2 <- ifelse(data2$hotspot >= 0 & data2$hotspot <= 741, 1,
                           ifelse(data2$hotspot >= 742 & data2$hotspot <= 1676, 2,
                                  ifelse(data2$hotspot >1676 , 3, data2$hotspot)))

## descriptive analysis 

data3 <- dplyr::select(data2, foc_visits,traffic2, fv_trafficx, hotspot2, entry, comms, accessibility, supplies, 
                       china, corruption, compliant, mcs, psma)

summary(data3)
min <- sapply(data3, min)
min
max <- sapply(data3, max)
max
sd <- sapply(data3, sd)
sd
var <- sapply(data3, var)
var
mean <- sapply(data3, mean)
mean

M <- cor(data3[, unlist(lapply(data3, is.numeric))])
corrplot(M, method = 'number')
corrplot(M, method = 'color', order = 'alphabet')

hist.data.frame(data2)

ggplot(data2, aes(x = traffic)) +
  geom_histogram(binwidth = 10)

ggplot(data2, aes(x = hotspot)) +
  geom_histogram(binwidth = 10)

ggplot(data2, aes(x = fv_traffic)) +
  geom_histogram(binwidth = 10)

ggplot(data2, aes(x = visits)) +
  geom_histogram(binwidth = 10)

ggplot(data2, aes(x = mcs)) +
  geom_histogram(binwidth = 10)

ggplot(data2, aes(x = traffic)) +
  geom_histogram(binwidth = 10)

boxplot(data3)
qqnorm(data2$corruption)
qqline(data2$corruption)
qqPlot(data2$corruption)

freq(data2)

# removing outlier 

my_data <- data2 %>%
  mutate(z_score = scale(data2$foc_visits))

# look at data and determine threshold
z_score_threshold <- 3

my_data_filtered <- my_data %>%
  filter(abs(z_score) <= z_score_threshold)

boxplot(my_data_filtered$foc_visits)

#model <- lmer(visits ~ corruption + compliant + designated + psma + mcs (1|country), data = data)
#summary(model)

library(foreign)
library(MASS)
library(glm2)

# poisson model for country variables
m1 <- glm2(foc_visits ~ corruption + compliant + designated + psma + mcs, data = data)
summary(m1)
vif(m1)

# nb model for country variables
m2 <- glm.nb(foc_visits ~ corruption + compliant + designated + psma + mcs, data = data)
summary(m2)
vif(m2)

# nb model for country variables with reduced data
m2b <- glm.nb(foc_visits ~ corruption + compliant + designated + psma + mcs, data = data2)
summary(m2b)
vif(m2b)

# nb model for port variables - no china
m3 <- glm.nb(visits ~ traffic + fv_traffic + hotspot + entry + eta + comms + accessibility + supplies, data = data)
summary(m3)
vif(m3)

# nb model for port variables with reduced data - no china
m3b <- glm.nb(visits ~ traffic + fv_traffic + hotspot + entry + eta + comms + accessibility + supplies, data = data2)
summary(m3b)
vif(m3b)

# nb model for port variables - with china
m4 <- glm.nb(visits ~ traffic + fv_traffic + hotspot + entry + eta + comms + accessibility + supplies + china, data = data)
summary(m4)
vif(m4)

# nb model for port variables with reduced data - with china
m4b <- glm.nb(visits ~ traffic + fv_traffic + hotspot + entry + eta + comms + accessibility + supplies + china, data = data2)
summary(m4b)
vif(m4b)

# nb model for country and port variables - no china
m5 <- glm.nb(foc_visits ~ traffic + fv_traffic + hotspot + entry + comms + accessibility + supplies +
               corruption + compliant + mcs + psma, data = data)
summary(m5)
vif(m5)

# nb model for country and port variables with reduced data - with china
m6 <- glm.nb(foc_visits ~ traffic + fv_traffic + hotspot2 + entry + comms + accessibility + supplies +
               corruption + compliant + mcs + psma + china, data = data2)
summary(m6)
vif(m6)

mx <- update(m6, . ~ . - china)
anova(mx, m6)

# generalized linear mixed-effects model for negative binomial (glmer) - with china
m7 <- glmer.nb(foc_visits ~ traffic + fv_traffic + hotspot + entry + eta + comms + accessibility + supplies +
                  corruption + compliant + mcs + psma + china + (1|port), data = data)
summary(m7)

# generalized linear mixed-effects model for negative binomial (glmer) with reduced data - with china
m7b <- glmer.nb(foc_visits ~ traffic + fv_traffic + hotspot2 + entry + comms + accessibility + supplies +
                 corruption + compliant + mcs + psma + china + (1|port), data = data2)
summary(m7b)

### Used this (fm2) for manuscript  
# generalized linear mixed model (GLMM) with reduced data - with china
fm2 <- glmmTMB(foc_visits ~ 1 + traffic2 + fv_trafficx + hotspot2 + entry  + comms + accessibility + supplies +
                 dc_china + corruption + compliant + mcs + psma + (1|port), data = data2, family = nbinom2())
summary(fm2)

# generalized linear mixed model (GLMM) with reduced data - no china
fm3 <- glmmTMB(foc_visits ~ 1 + traffic + fv_traffic + hotspot + entry + comms + accessibility + supplies +
                 corruption + compliant + mcs + psma + (1|port), data = data2, family = nbinom2())
summary(fm3)

#### logit analysis ####

colnames(data2)

# logit with reduced data - with china
m8 <- glm(foc_visits2 ~ corruption + compliant + psma +
            mcs + traffic + hotspot2 + fv_traffic + accessibility +
            entry + comms + supplies + china, data = data2, family = binomial)

summary(m8)
confint(m8)
confint.default(m8)
exp(cbind(OR = coef(m8), confint(m8)))

# logit with reduced data - no china
m9 <- glm(foc_visits2 ~ corruption + compliant + psma +
            mcs + traffic + hotspot + fv_traffic + accessibility +
            entry + comms + supplies, data = data2, family = binomial)

summary(m9)
confint(m9)
confint.default(m9)
exp(cbind(OR = coef(m9), confint(m9)))

