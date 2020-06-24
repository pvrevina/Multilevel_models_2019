library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plm)
library(stargazer)

women <- read.csv("CIRI Data.csv")
polity <- read_excel("p4v2018.xls")
cel_phones <- read.csv("cel_phones.csv")
unempl_rate <- read.csv("unemployment.csv")
infl_rate <- read.csv("inflation.csv")

fdi_flow_curr <- read.csv("fdi_curr.csv")
names(fdi_flow_curr) <- c("Country", "Country.Code",  "Indicator", "X2001", "2002", "2003", "2004", "2005", "2006", "2007", 
                          "2008", "2009", "2010", "2011") 
fdi_flow_curr <- fdi_flow_curr %>% select(-"X2001") 

fdi <- fdi_flow_curr %>% gather(year, FDI, 4:13) %>% select(-Indicator) 
fdi <- filter(fdi, FDI > 0) 
fdi$log_fdi <- log(fdi$FDI) 
fdi <- filter(fdi, !(is.na(log_fdi)))
fdi <- fdi %>% filter(!is.na(FDI))

unique(fdi$year)

women2 <- women %>% select(CTRY, YEAR, COW, POLITY, WECON, WOPOL, PHYSINT, WORKER)
names(women2) <- c("Country", "year", "Cow", "Polity.Code", "W.Ec", "W.Pol", "physh", "worker")

names(polity)[4] <- "Country"
names(polity)[3] <- "Country.Code"
polity$year <- as.integer(polity$year)

polity2 <- filter(polity, year > 2001)
polity2 <- filter(polity2, year < 2012)
polity2 <- polity2 %>% select(ccode, Country.Code, Country, year, polity, polity2)
names(polity2)[1] <- "Polity.Code"

polit_wom <- merge(women2, polity2, by = c("Polity.Code", "year"))
names(polit_wom)[10] <- "Country_old"
names(polit_wom)[3] <- "Country"

polit_wom_fdi <- merge(polit_wom, fdi, by = c("Country", "year"))

names(cel_phones) <- c("Country", "Country.Code", "Indicator", "I.Code", "2002", "2003", "2004", "2005", "2006", 
                       "2007", "2008", "2009", "2010", "2011")
cel_phones <- cel_phones %>% select(Country, Country.Code, 
                                    '2002', '2003', '2004', 
                                    '2005', '2006', '2007', '2008', '2009', '2010', '2011')

names(unempl_rate) <- c("Country", "Country.Code", "Indicator", "I.Code", "2002", "2003", "2004", "2005", "2006", 
                        "2007", "2008", "2009", "2010", "2011")
unempl_rate <- unempl_rate %>% select(Country, Country.Code, 
                                      '2002', '2003', '2004', 
                                      '2005', '2006', '2007', '2008', '2009', '2010', '2011')

names(infl_rate) <- c("Country", "Country.Code", "Indicator", "I.Code", "2002", "2003", "2004", "2005", "2006", 
                      "2007", "2008", "2009", "2010", "2011")
infl_rate <- infl_rate %>% select(Country, Country.Code, 
                                  '2002', '2003', '2004', 
                                  '2005', '2006', '2007', '2008', '2009', '2010', '2011')
celphone <- cel_phones %>% gather(year, cel_phone, 3:12)
inflation <- infl_rate %>% gather(year, inflation, 3:12)
unempl <- unempl_rate %>% gather(year, unempl, 3:12)

c_in <- merge(celphone, inflation, by = c("Country", "year"))
c_in_un <- merge(c_in, unempl, by = c("Country", "year"))
c_in_un <- c_in_un %>% select(-Country.Code.x) %>% select(-Country.Code.y)

data <- merge(polit_wom_fdi, c_in_un, by = c("Country", "year")) %>% select(-Country.Code, 
                                                                            -Cow, - Polity.Code)
data$year_state <- paste(data$year, data$Country, sep = "_")
data <- data[!duplicated(data$year_state),] %>% select(-polity)

data$W.Ec[data$W.Ec == -999] <- NA
data$W.Ec[data$W.Ec == -77] <- NA
data$W.Ec[data$W.Ec == -66] <- NA

data$W.Pol[data$W.Pol == -999] <- NA
data$W.Pol[data$W.Ec == -77] <- NA
data$W.Pol[data$W.Ec == -66] <- NA

data$phys[data$phys == -999] <- NA
data$phys[data$phys == -77] <- NA
data$phys[data$phys == -66] <- NA

data$worker[data$worker == -999] <- NA
data$worker[data$worker == -77] <- NA
data$worker[data$worker == -66] <- NA

data <- data %>% filter(!is.na(W.Ec), !is.na(W.Pol), cel_phone > 0)
data$log.cel_phone <- log(data$cel_phone)

school <- read.csv("school.csv")
names(school) <- c("Country", "Country.Code",  "Indicator", "X2001", "2002", "2003", "2004", "2005", "2006", "2007", 
                   "2008", "2009", "2010", "2011") 
school <- school %>% select(-"X2001")
school<- school %>% gather(year, school_teriary, 4:13) %>% select(-Indicator) %>% filter(!is.na(school_teriary))
school_data <- merge(school, data, by = c("Country", "year"), all.y = TRUE)

school_2 <- read.csv("school_2.csv")
names(school_2) <- c("Country", "Country.Code",  "Indicator", "X2001", "2002", "2003", "2004", "2005", "2006", "2007", 
                     "2008", "2009", "2010", "2011") 
school_2 <- school_2 %>% select(-"X2001")
school_2<- school_2 %>% gather(year, school_secondary, 4:13) %>% select(-Indicator) %>% filter(!is.na(school_secondary))

data_school2 <- merge(school_data, school_2, by = c("Country", "year"), all.x = TRUE)

summary(data_school2)

# women's ec rights 
ols <- plm(log_fdi ~ W.Ec + polity2 + phys + worker + log.cel_phone + inflation + unempl + phys, 
          data = data_school2, index = c("Country", "year"), model = "pooling")
summary(ols)

fe <- plm(log_fdi ~ W.Ec + phys + worker + log.cel_phone + inflation + unempl + phys, 
            index = c("Country", "year"), model = "within", data = data_school2)
summary(fe)
pFtest(fe, ols)

fefact <- plm(log_fdi ~ factor(W.Ec) + phys + worker + log.cel_phone + inflation + unempl + phys, 
          index = c("Country", "year"), model = "within", data = data_school2)
summary(fefact)

stargazer(fefact)

LSDV <- lm(log_fdi ~ W.Ec + phys + worker + log.cel_phone + inflation + unempl + phys + polity2 + Country, 
           data = na.omit(data_school2))

y_pred <- LSDV$fitted
panel1 <- data.frame(na.omit(data_school2), y_pred) 
ggplot(panel1, aes(x = log_fdi, y = y_pred, color = Country.Code.x))+geom_smooth(method = lm) + xlim(0, 30) + ylim(10, 20)

# мк
library(car)
vif(LSDV)

plot(dfbetas(LSDV)[, 1], ylab = "normalized measure of the effect of observations on the estimated regression coefficients", xlab = "Observation")
head(cooks.distance(LSDV))
# RE vs pooled 
re <- plm(log_fdi ~ W.Ec + phys + worker + log.cel_phone + inflation + unempl + phys, 
          data=data_school2, index=c("Country", "year"), model="random")

summary(re)
plmtest(ols, type=c("bp"))
# RE vs FE 
phtest(fe, re)

# протестируем модель на устойчивость 
library(lmtest)
merged <- panel1 %>% group_by(Country)%>% summarize(., cor(log_fdi, y_pred))%>% merge(panel1, ., by="Country")
merged$new <- ifelse(abs(merged$`cor(log_fdi, y_pred)`)<0.3,1,0)
fe_twoways_2 <- plm(log_fdi ~ W.Ec + polity2 + phys + worker + log.cel_phone + inflation + unempl + phys, merged[merged$new == 0,], index=c("Country", "year"), model = "within")

#поправка на гет.
bptest(fe, studentize = F)
n <- coeftest(fe, vcov = vcovHC, type = "HC3")
stargazer(n)

# модели 2 и 3
a <- plm(log_fdi ~ school_teriary + polity2 + phys + worker + log.cel_phone + inflation + unempl + phys, 
         index = c("Country", "year"), model = "within", data = school_data)

summary(a)

LSDV_a <- lm(log_fdi ~ school_teriary + polity2 + phys + worker + log.cel_phone + inflation + unempl + phys + Country, 
             data = na.omit(school_data))
ols <- plm(log_fdi ~ school_teriary + polity2 + phys + worker + log.cel_phone + inflation + unempl + phys, 
           index = c("Country", "year"), model = "pooling", data = na.omit(data_school2))
pFtest(a, ols)

re <- plm(log_fdi ~ school_teriary + polity2 + phys + worker + log.cel_phone + inflation + unempl + phys, 
          data=data_school2, index=c("Country", "year"), model="random")

summary(re)
plmtest(ols, type=c("bp"))
# RE vs FE 
phtest(a, re)

y_pred <- LSDV_a$fitted
panel1 <- data.frame(na.omit(school_data), y_pred) 
ggplot(panel1, aes(x = log_fdi, y = y_pred, color = Country.Code.x))+geom_smooth(method = lm) + xlim(0, 30) + ylim(10, 20)
merged <- panel1 %>% group_by(Country)%>% summarize(., cor(log_fdi, y_pred))%>% merge(panel1, ., by="Country")
merged$new <- ifelse(abs(merged$`cor(log_fdi, y_pred)`)<0.3,1,0)
fe_twoways_2 <- plm(log_fdi ~ school_teriary + polity2 + phys + worker + log.cel_phone + inflation + unempl + phys, merged[merged$new == 0,], index=c("Country", "year"), model = "within")
summary(fe_twoways_2) # ничего не изменилось 


b <- plm(log_fdi ~ school_secondary + polity2 + phys + worker + log.cel_phone + inflation + unempl + phys, 
         index = c("Country", "year"), model = "within", data = na.omit(data_school2))

summary(b)

LSDV_b <- lm(log_fdi ~ school_secondary + polity2 + phys + worker + log.cel_phone + inflation + unempl + phys + Country, 
             data = na.omit(data_school2))
ols <- plm(log_fdi ~ school_secondary + polity2 + phys + worker + log.cel_phone + inflation + unempl + phys, 
           index = c("Country", "year"), model = "pooling", data = na.omit(data_school2))
pFtest(b, ols)

re <- plm(log_fdi ~ school_secondary + polity2 + phys + worker + log.cel_phone + inflation + unempl + phys, 
          data=na.omit(data_school2), index=c("Country", "year"), model="random")

summary(re)
plmtest(ols, type=c("bp"))
# RE vs FE 
phtest(b, re)

y_pred <- LSDV_b$fitted
panel1 <- data.frame(na.omit(data_school2), y_pred) 
ggplot(panel1, aes(x = log_fdi, y = y_pred, color = Country.Code.x))+geom_smooth(method = lm) + xlim(0, 30) + ylim(10, 20)
merged <- panel1 %>% group_by(Country)%>% summarize(., cor(log_fdi, y_pred))%>% merge(panel1, ., by="Country")
merged$new <- ifelse(abs(merged$`cor(log_fdi, y_pred)`)<0.3,1,0)
fe_twoways_2b <- plm(log_fdi ~ school_teriary + polity2 + phys + worker + log.cel_phone + inflation + unempl + phys, merged[merged$new == 0,], index=c("Country", "year"), model = "within")
summary(fe_twoways_2b) # ничего не изменилось 

#поправка на гет.
bptest(fe, studentize = F)
n2 <- coeftest(a, vcov = vcovHC, type = "HC3")
n3 <- coeftest(b, vcov = vcovHC, type = "HC3")

stargazer(fe_twoways_2, fe_twoways_2b)
stargazer(n2, n3)

fdi <- read.csv("FDI_stock.csv")
stock <- fdi[fdi$MEASURE == "MLN_USD", ] 
stock <- filter(stock, TIME > 2001)
stock <- filter(stock, TIME < 2012)
stock <- stock[stock$SUBJECT == "INWARD", ]
names(stock)[1] <- "Country.Code"
names(stock)[6] <- "year"
stock <- stock %>% select(Country.Code, year, Value)
names(stock)[3] <- "FDI"
head(stock)

stock$log_stock <- log(stock$FDI) 
stock_new <- merge(stock, data_school2, by = c("Country.Code", "year"), all.y = TRUE)
names(data_school2)[3] <- "Country.Code"
new_stock <-plm(log_stock ~ W.Ec + polity2 + phys + worker + log.cel_phone + inflation + unempl + phys + Country, 
                index = c("Country", "year"), model = "within", data = stock_new) 
new_stock2 <-plm(log_stock ~ school_teriary + polity2 + phys + worker + log.cel_phone + inflation + unempl + phys + Country, 
                 index = c("Country", "year"), model = "within", data = stock_new) 
new_stock3 <-plm(log_stock ~ school_secondary + polity2 + phys + worker + log.cel_phone + inflation + unempl + phys + Country, 
                 index = c("Country", "year"), model = "within", data = stock_new) 
stargazer(new_stock, new_stock2, new_stock3)