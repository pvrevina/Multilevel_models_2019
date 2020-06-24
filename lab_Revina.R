library(ggplot2)
library(dplyr)
library(plm)
library(haven)

data <- read_dta("MSA_lab3.dta")
data <- na.omit(data)
head(data)
summary(data)

attach(data)

## LSDV model 
LSDV <- lm(healthexp ~ fh + opp + ji +pop_rural + pop_u15 + pop_over65 + mort_u5 + ln_gdp_pc + factor(countrynum))
summary(LSDV)

# Fh: в среднем расходы на здравоохранение изменяются на 0.004038 при изменении показателя свободы прессы на 1 при прочих 
# равных при условии рассмотрения временной динамики. Оценка коэффициента не значима. 

# factor(countrynum)2: в среднем расходы на здравоохранение изменяются во 2ой стране в отличие от первой страны при прочих равных
# условиях на 6.162879 

## FE vs Pooled model
fe <- plm(healthexp ~ fh + opp + ji +pop_rural + pop_u15 + pop_over65 + mort_u5 + ln_gdp_pc ,index=c("countrynum", "year"),data = data, model="within")
summary(fe)

# Fh: в среднем расходы на здравоохранение изменяются на 0.004038 при изменении показателя свободы прессы на 1 при прочих 
# равных при условии рассмотрения временной динамики. Оценка коэффициента не значима. 

ols <- plm(healthexp ~ fh + opp + ji +pop_rural + pop_u15 + pop_over65 + mort_u5 + ln_gdp_pc, data = data, model="pooling")
summary(ols)

# Fh: в среднем расходы на здравоохранение изменяются на -0.0199374 при изменении показателя свободы прессы на 1 при прочих 
# равных. Оценка коэффициента значима, связь отрицательная. 

pFtest(fe, ols)
# Большое значение F-статистики, отвержение H0 => отдаем предпочтение FE-model.

## FE vs RE model (+ содержательное)
# Breush-Pagan test
plmtest(ols, type=c("bp"))
# Re-model можно использовать. 

re <- plm(healthexp ~ fh + opp + ji +pop_rural + pop_u15 + pop_over65 + mort_u5 + ln_gdp_pc, data=data, index=c("countrynum", "year"), model="random")
summary(re)

# Fh: в среднем расходы на здравоохранение изменяются на -0.0024945 при изменении показателя свободы прессы на 1 при прочих 
# равных при условии рассмотрения временной динамики. Оценка коэффициента не значима.
phtest(fe, re)
# Отвергаем нулевую гипотезу об экзогенности => FE-model

# Предпочтительно использовать FE-model, так как выборка из Африканских стран уникальна, и мы не можем обобщить 
# результаты на более широкую совокупность

## FE-model с временными эффектами 
fe_time <- plm(healthexp ~ fh + opp + ji +pop_rural + pop_u15 + pop_over65 + mort_u5 + ln_gdp_pc, data=data, 
               index=c("countrynum", "year"), effect = "time", model = "within")
summary(fe_time)
# extract time effects
summary(fixef(fe_time))

#Fh: в среднем расходы на здравоохранение изменяются на -0.0206694 при изменении показателя свободы прессы на 1 при прочих 
# равных при условии рассмотрения пространственной/межстрановой динамики. Оценка коэффициента значима.

## two-way FE 
fe_twoways <- plm(healthexp ~ fh + opp + ji +pop_rural + pop_u15 + pop_over65 + mort_u5 + ln_gdp_pc, data=data, 
                  index=c("countrynum", "year"), effect = "twoways", model = "within")
summary(fe_twoways)

# Fh: в среднем расходы на здравоохранение изменяются на 0.0079724 при изменении показателя свободы прессы на 1 при прочих 
# равных при условии рассмотрения временной и пространственной динамики

## Диагностика
LSDV_twoways <- lm(healthexp ~ fh + opp + ji +pop_rural + pop_u15 + pop_over65 + mort_u5 + ln_gdp_pc + factor(countrynum) +
                     factor(year), data=data)

y_pred <- LSDV_twoways$fitted
panel1 <- data.frame(data, y_pred) 
# subset the data by countries and find how strongly y observed and predicted are correlated
merged <- panel1 %>% group_by(countrynum)%>% summarize(., cor(healthexp, y_pred))%>% merge(panel1, ., by="countrynum")
# test whether results are robust to excluding observations with small correlations
merged$new <- ifelse(abs(merged$`cor(healthexp, y_pred)`)<0.3,1,0)
fe_twoways_2 <- plm(healthexp ~ fh, merged[merged$new == 0,], index=c("countrynum", "year"), effect = "twoways")
summary(fe_twoways_2)
# После исключения влиятельных наблюдений оценка коэффициента при ключевом предикторе стала значимой. Модель не устойчива
# к оутлайнерам. 
# Fh:  среднем расходы на здравоохранение изменяются на 0.0228377 при изменении показателя свободы прессы на 1 при прочих 
# равных при условии рассмотрения временной и пространственной динамики

# Необходимо уточнить спецификацию и перейти к другой модели, например, к varying slopes model. 

## Поправка на гет. 
library(lmtest)
coeftest(fe_twoways_2, vcov = vcovHC, type = "HC3")



