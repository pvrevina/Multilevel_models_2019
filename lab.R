### Multivariate statistical analysis, 2019 ###
### Mixed-effects models (part 1)

### Description
### Рассмотрим пример оценивания моделей со смешанными эффектами на кросс-секционных данных из статьи [Luke D.A., Krauss M., 2004] 
### (статью можете скачать по ссылке https://yadi.sk/i/w_dgDbu8blukXA). 
### Авторы изучают факторы голосования членов Конгресса США в интересах табачной индустрии. 
### В фокусе внимания - различия во взаимосвязи размера финансирования комитетами политического действия (PAC) 
### от табачных корпораций и голосования в интересах табачной индустрии. 
### В частности, авторы тестируют, есть ли различия в этой взаимосвязи между представителями Демократической и Республиканской партий.

### Краткое описание данных:
# state - штат (группирующая переменная)
# lastname - член Конгресса
## Переменные на первом уровне:
# votepct - доля голосов, отданных членом Конгресса в поддержку табачной индустрии (зависимая переменная)
# party - дамми-переменная: представитель Демократической или Республиканской партии (1 - Республиканская партия)
# money - размер финансирования от PAC табачных корпораций
## Предиктор на втором уровне:
# acres - площадь табачных плантаций

# y = money, x = votepct

install.packages("haven")
install.packages("psych")
install.packages("arm")
install.packages("multilevel")
install.packages("lattice")
install.packages("sjPlot")
install.packages("influence.ME")
install.packages("ggplot2")
install.packages("lmerTest")

library(haven)
library(psych)
library(arm)
library(multilevel)
library(lattice)
library(sjPlot)
library(influence.ME)
library(ggplot2)
library(lmerTest)

# set a working directory
# open your data (MSA_lab4.dta)
ME <- read_dta(file.choose())

# ID for each state (the second level): transform a nominal variable into a factor one
state_id <- as.factor(ME$state) 
ME <- na.omit(data.frame(ME, state_id))

# examine descriptive statistics
head(ME)
describe(ME)

### Preliminary visualization
ggplot(ME, aes(x=money, y=votepct, color=state_id)) +
  geom_smooth(method=lm, se=FALSE)

# run a null-model
# в скобках случайный эффект и групп переменная = пространственные единицы на втором уровне 
# REML = метод 
null <- lmer(votepct ~ 1 + (1|state_id), REML = FALSE, data = ME)
summary(null)
# residual = ошибка на индивидуальном уровне 

# it is better to calculate ICC manually since the group sizes are unequal
0.03534 / (0.03534+0.09264) 
# нужно исп такую модель, которая учитывает неоднородность 

# extra option: ICC using anova model (only when group sizes are equal)
anovaicc <- aov(votepct ~ as.factor(state_id), data = ME) 
summary(anovaicc)
ICC1(anovaicc)

# see whether the group variance is equally distributed
# если бы наблюдали однородный массив данных 
# отклонения от жирной линии = есть межгрупповые разлчия
# только для нескольких групп есть значимые отклонения => не можем делать выводов (влиятельные наблюдения)

graph.ran.mean(ME$votepct, ME$state_id, nreps=1000, bootci=TRUE)
# в дополнение к исс делаем вывод, что межгрупп вариация является значимая => модели учитывающие неоднородность 

# show random effects
ranef(null)
# предсказанные значения отклонения в доли голосовании для конкретного штата в отличие от среднего по всей выборке 

# снижаем вариацию на первом и втором уровнях

# run a model with fixed effects for individual-level indicators
# добавляем финансирование 
# вз финанс и доли голосования в среднем по всей выборке, т.е. без учета межгрупповых различий 
model1 <- lmer(votepct ~ money + (1|state_id), REML = FALSE, data = ME)
summary(model1)

# show random effects (BLUP)
ranef(model1)
# visualize random effects with confidence intervals
# Are random effects significant?
dotplot(ranef(model1, condVar=TRUE))
# как значение для константы отклоняются по штатам 
# доверительные интервалы накрывают ноль => отклонения доли голосования по штатам незначимо отличаются от среднего по всей выборке 

# get intercept for each state
# unique intercept
model1_intercept <- coef(model1)$state_id[,1] # 1 - выводим для константы 
model1_intercept
# make sure that we estimated the same slope for each state
model1_slope <- coef(model1)$state_id[,2] # суммируем по наклонам 
model1_slope # одно значение, тк мы не моделировали разные наклоны 

# without repeated observations (duplicates)
state_unique <- unique(state_id)
data.frame(state_unique, model1_intercept, model1_slope)

# compare models
anova(null, model1)
# рассмотрим менее экономную модель 

# add individual-level predictors
model2 <- lmer(votepct ~ money + party + (1|state_id), REML = FALSE, data = ME)
summary(model2)
anova(model1, model2)

# add fixed effects for state-level predictors (acres)
model3 <- lmer(votepct ~ money + party + acres + (1|state_id), REML = FALSE, data = ME)
summary(model3)
anova(model2, model3)

# add random effects
model4.1 <- lmer(votepct ~ money + party + (1 + party|state_id), REML = FALSE, data = ME)
summary(model4.1) 
ranef(model4.1) 
dotplot(ranef(model4.1, condVar=TRUE))
anova(model2, model4.1)

# the effect of lnmoney does not vary significantly across states
model4.2 <- lmer(votepct ~ money + party + (1 + money|state_id), REML = FALSE, data = ME)
summary(model4.2) # random effects are correlated
ranef(model4.2) 
dotplot(ranef(model4.2, condVar=TRUE))

###################################################################################################################################################
# optional: Why does the effect of party vary across states? 
# However, the effect of party does NOT vary significantly across states. 
model5 <- lmer(votepct ~ money + party + acres + party*acres + (1 + party|state_id), REML = FALSE, data = ME)
summary(model5)

### diagnostics for variance components
model <- lmer(votepct ~  money + party  + (1+party|state_id), REML = FALSE, data = ME)
plot_model(model, type = "diag")[1]
plot_model(model, type = "diag")[2]
plot_model(model, type = "diag")[3]
plot_model(model, type = "diag")[4]

### influential observations
inf <- influence(model, group = "state_id", data = ME)
cooks.distance.estex(inf, sort=TRUE)
plot(inf, which = "cook", xlab = "COOK'S MEASURE", cutoff = 4/50)  
dfbetas.estex(inf)
plot(inf, which = "dfbetas", xlab = "DFBETAS", cutoff = sqrt(4/50))  
