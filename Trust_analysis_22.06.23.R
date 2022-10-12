library(readxl)
library(dplyr)
library(sqldf)
library(DescTools)
library(stringr)
trust_data = read_excel(file.choose())
hco_data = read_excel(file.choose())

names(hco_data)[names(hco_data) == 'HCO/trust'] <- 'hco_trust'


exclusion = select(original_data, row, year, 'Unclear results', action)

hco_data2 = left_join(hco_data, exlusion, by = c("row", "year"))

table(hco_data2$action)


hco_data2$action[is.na(hco_data2$action)] = "good"
table(hco_data2$'Unclear results')
hco_data2 <- subset(hco_data2,action!="remove")
hco_data2 <- subset(hco_data2,action!="unclear")


table(hco_data2$action)

trust_test = select(trust_data, row, year)
trust_test$good = "1"

hco_data2 = left_join(hco_data2, trust_test, by = c("row", "year"))

hco_data2_trusts = subset(hco_data2,hco_trust="trust")
hco_data2_trusts <- hco_data2[ which(hco_data2$hco_trust=='trust'),]
hco_data2_trusts$good[is.na(hco_data2_trusts$good)] = "0"
table(hco_data2_trusts$good)

hco_data2_trusts <- hco_data2_trusts[ which(hco_data2_trusts$good=='0'),]
exlusion = select(hco_data2_trusts, row, year)
exlusion$action = "remove"

hco_data2 = left_join(hco_data2, exlusion, by = c("row", "year"))
hco_data2$action.y[is.na(hco_data2$action.y)] = "keep"

hco_data2<-hco_data2[!(hco_data2$action.y=="remove"),]

hco_data2$good = NULL
hco_data = hco_data2
remove(hco_data2)
remove(exlusion)
remove(exlusion)


###### HCO cleanings

all_data = read_excel(file.choose())

all_data_action = select(original_data, row,year,action)

all_data = merge(all_data, all_data_action, by = c("row", "year"))

table(hco_nonengland$Region)
sum(is.na(all_data$Region))

all_data$Region[is.na(all_data$Region)] = "Non-geographic"

hco_nonengland <- all_data[(all_data$Region=="Channel Islands" | all_data$Region=="Ireland" | all_data$Region=="Isle of Man" | all_data$Region=="Non-geographic" | all_data$Region=="Northern Ireland" | all_data$Region=="Scotland" | all_data$Region=="Wales" | all_data$Region=="Non-geographic"),]
hco_england <- all_data[!(all_data$Region=="Channel Islands" | all_data$Region=="Ireland" | all_data$Region=="Isle of Man" | all_data$Region=="Non-geographic" | all_data$Region=="Northern Ireland" | all_data$Region=="Scotland" | all_data$Region=="Wales" | all_data$Region=="Non-geographic"),]




hco_trust = hco_england[which(hco_england$`HCO/trust` == "trust"), ]
hco_notrust = hco_england[which(hco_england$`HCO/trust` == "HCO"), ]



hco_trust$action[is.na(hco_trust$action)] = "no action"

hco_removed = hco_trust[which(hco_trust$action == "remove" | hco_trust$action == "unclear"), ]
hco_removed$`HCO/trust`[hco_removed$`HCO/trust` == "trust"] = "HCO"

hco_notrust = rbind(hco_notrust, hco_removed)

hco_trust = hco_trust[!(hco_trust$action == "remove"), ]
hco_trust = hco_trust[!(hco_trust$action == "unclear"), ]



test = select(trust_data, row, year, Region)

test = left_join(hco_trust, test, by = c("row", "year"))

test = test[which(is.na(test$Region.y)), ]

test$Region.y = NULL

test = rename(test, Region = Region.x)
test$`HCO/trust`[test$`HCO/trust` == 'trust'] = "HCO"

hco_notrust = rbind(hco_notrust, test)


test = select(trust_data, row, year, Region)

hco_trust = left_join(hco_trust, test, by = c("row", "year"))

hco_trust = hco_trust[!(is.na(hco_trust$Region.y)), ]

hco_trust$Region.y = NULL

####### Summary all HCOs

table_hco<-data.frame(Year= character(0) , Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

b<-c("2015","2016","2017","2018")

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_england%>%filter(year %in% b[i])
  table_hco$Number_p[table_hco$Year==b[i]]<-sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_england%>%filter(year %in% b[i])
  table_hco$Value_p[table_hco$Year==b[i]]<-sqldf("select sum(final_tov) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  
  
table_hco = table_hco[-(5:13),]
  
  

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_england%>%filter(year %in% b[i])
  table_hco$Mean[table_hco$Year==b[i]]<-sqldf("select avg(final_tov) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=mean)  

table_hco = table_hco[-(5:9),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_england%>%filter(year %in% b[i])
  table_hco$St_dev[table_hco$Year==b[i]]<-sqldf("select STDEV(final_tov) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_hco = table_hco[-(5:8),]
  


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_england%>%filter(year %in% b[i])
  table_hco$Median[table_hco$Year==b[i]]<-sqldf("select median(final_tov) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_hco = table_hco[-(5:8),]




  
for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_england%>%filter(year %in% b[i])
  table_hco$Q1[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_england%>%filter(year %in% b[i])
  table_hco$Q3[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:12),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)


table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)

table_hco$median_iqr <- paste(table_hco$Median,"(",table_hco$Q1, "-",table_hco$Q3,")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))



JonckheereTerpstraTest(hco_england$final_tov, hco_england$year, nperm = 1000, alternative = "increasing")

pwt_hco = pairwise.wilcox.test(hco_england$final_tov, hco_england$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_hco = left_join(table_hco, pwt_hco, by = "Year")

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

summary(hco_england$final_tov)
sum(hco_england$final_tov)
sd(hco_england$final_tov)


#### Total payment to HCOs in England

table_hco<-data.frame(Trust_number = character(0), Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_england
  table_hco$Trust_number<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_england
  table_hco$Value_p<- sum(seged_2$final_tov) 
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_england
  table_hco$Number_p<-length(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_england
  table_hco$Mean<-mean(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_england
  table_hco$St_dev<-sd(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_england
  table_hco$Median<-median(seged_2$final_tov)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]





for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_england
  table_hco$Q1<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_england
  table_hco$Q3<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)

table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))



####### Summary  trusts in England

table_hco<-data.frame(Year= character(0) , Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

b<-c("2015","2016","2017","2018")

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(year %in% b[i])
  table_hco$Number_p[table_hco$Year==b[i]]<-sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(year %in% b[i])
  table_hco$Value_p[table_hco$Year==b[i]]<-sqldf("select sum(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:13),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(year %in% b[i])
  table_hco$Mean[table_hco$Year==b[i]]<-sqldf("select avg(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:9),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(year %in% b[i])
  table_hco$St_dev[table_hco$Year==b[i]]<-sqldf("select STDEV(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(year %in% b[i])
  table_hco$Median[table_hco$Year==b[i]]<-sqldf("select median(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:8),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(year %in% b[i])
  table_hco$Q1[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(year %in% b[i])
  table_hco$Q3[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:12),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)



table_hco$median_iqr <- paste(table_hco$Median, "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


JonckheereTerpstraTest(trust_data$final_tov, trust_data$year, nperm = 1000, alternative = "decreasing")

pwt_hco = pairwise.wilcox.test(trust_data$final_tov, trust_data$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_hco = left_join(table_hco, pwt_hco, by = "Year")

summary(trust_data$final_tov)
sum(trust_data$final_tov)
sd(trust_data$final_tov)



#### Total of trusts in England

table_hco<-data.frame(Trust_number = character(0), Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_data
  table_hco$Trust_number<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_data
  table_hco$Value_p<- sum(seged_2$final_tov) 
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_data
  table_hco$Number_p<-length(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_data
  table_hco$Mean<-mean(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_acute
  table_hco$St_dev<-sd(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_data
  table_hco$Median<-median(seged_2$final_tov)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]





for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_data
  table_hco$Q1<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_data
  table_hco$Q3<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)

table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


####### Summary trust payments in England aggregated at trust level 

table_hco<-data.frame(Year= character(0) , Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

b<-c("2015","2016","2017","2018")

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov%>%filter(year %in% b[i])
  table_hco$Number_p[table_hco$Year==b[i]]<-length(seged_2$final_tov)
  rm(seged_2)
  
}


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov%>%filter(year %in% b[i])
  table_hco$Value_p[table_hco$Year==b[i]]<-sqldf("select sum(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:13),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov%>%filter(year %in% b[i])
  table_hco$Mean[table_hco$Year==b[i]]<-sqldf("select avg(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:9),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov%>%filter(year %in% b[i])
  table_hco$St_dev[table_hco$Year==b[i]]<-sqldf("select STDEV(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov%>%filter(year %in% b[i])
  table_hco$Median[table_hco$Year==b[i]]<-sqldf("select median(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:8),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov%>%filter(year %in% b[i])
  table_hco$Q1[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov%>%filter(year %in% b[i])
  table_hco$Q3[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:12),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)



table_hco$median_iqr <- paste(table_hco$Median, "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


JonckheereTerpstraTest(trust_sumtov$final_tov, trust_sumtov$year, nperm = 1000, alternative = "decreasing")

pwt_hco = pairwise.wilcox.test(trust_sumtov$final_tov, trust_sumtov$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_hco = left_join(table_hco, pwt_hco, by = "Year")

summary(trust_data$final_tov)
sum(trust_data$final_tov)
sd(trust_data$final_tov)



#### Total of trusts in England

table_hco<-data.frame(Trust_number = character(0), Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov
  table_hco$Trust_number<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov
  table_hco$Value_p<- sum(seged_2$final_tov) 
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov
  table_hco$Number_p<-length(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov
  table_hco$Mean<-mean(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov
  table_hco$St_dev<-sd(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov
  table_hco$Median<-median(seged_2$final_tov)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]





for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov
  table_hco$Q1<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov
  table_hco$Q3<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)

table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))





####### Summary HCO without trusts in England

table_hco<-data.frame(Year= character(0) , Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

b<-c("2015","2016","2017","2018")

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-england_notrust%>%filter(year %in% b[i])
  table_hco$Number_p[table_hco$Year==b[i]]<-sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-england_notrust%>%filter(year %in% b[i])
  table_hco$Value_p[table_hco$Year==b[i]]<-sqldf("select sum(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:13),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-england_notrust%>%filter(year %in% b[i])
  table_hco$Mean[table_hco$Year==b[i]]<-sqldf("select avg(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:9),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-england_notrust%>%filter(year %in% b[i])
  table_hco$St_dev[table_hco$Year==b[i]]<-sqldf("select STDEV(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-england_notrust%>%filter(year %in% b[i])
  table_hco$Median[table_hco$Year==b[i]]<-sqldf("select median(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:8),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-england_notrust%>%filter(year %in% b[i])
  table_hco$Q1[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-england_notrust%>%filter(year %in% b[i])
  table_hco$Q3[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:12),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)
table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)


table_hco$median_iqr <- paste(table_hco$Median, "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

JonckheereTerpstraTest(hco_notrust$final_tov, hco_notrust$year, nperm = 1000, alternative = "increasing")

pwt_hco = pairwise.wilcox.test(hco_notrust$final_tov, hco_notrust$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_hco = left_join(table_hco, pwt_hco, by = "Year")

summary(hco_notrust$final_tov)
sum(hco_notrust$final_tov)
sd(hco_notrust$final_tov)


####### TOTAL HCO without trusts in England

table_hco<-data.frame(Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))



for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_notrust
  table_hco$Number_p<-length(seged_2$final_tov)
  rm(seged_2)
  
}


for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_notrust
  table_hco$Value_p<-sum(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_notrust
  table_hco$Mean<-mean(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:9),]



for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_notrust
  table_hco$St_dev<-sd(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:8),]



for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_notrust
  table_hco$Median<-median(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:8),]





for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_notrust
  table_hco$Q1<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:19),]


for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-hco_notrust
  table_hco$Q3<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:12),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)



table_hco$median_iqr <- paste(table_hco$Median, "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


########## PAYMENT TYPE COMPARISON ######################################################################
############################################################################################################################################
############################################################################################################################################

trust_2015 <- trust_data[ which(trust_data$year=='2015'),]
trust_2016 <- trust_data[ which(trust_data$year=='2016'),]
trust_2017 <- trust_data[ which(trust_data$year=='2017'),]
trust_2018 <- trust_data[ which(trust_data$year=='2018'),]

trust_jointwork <- trust_data[ which(trust_data$infl_vat_adj_jointworking!=0),]
trust_donation <- trust_data[ which(trust_data$infl_vat_adj_donations!=0),]
trust_service <- trust_data[ which(trust_data$infl_vat_adj_service!=0),]
trust_events <- trust_data[ which(trust_data$infl_vat_adj_costsofevents!=0),]


#### Jointwork

table_ptype<-data.frame(Number_p = character(0) , Value_p = character(0) ,  Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

b<-c("2015","2016","2017","2018")

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_jointwork%>%filter(year %in% b[i])
  table_ptype$Number_p[table_ptype$Year==b[i]]<- length(which(seged_2$infl_vat_adj_jointworking != 0))
  rm(seged_2)
  
}

table_ptype = unique(table_ptype)


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_jointwork%>%filter(year %in% b[i])
  table_ptype$Value_p[table_ptype$Year==b[i]]<-sqldf("select sum(infl_vat_adj_jointworking) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_jointwork%>%filter(year %in% b[i])
  table_ptype$Mean[table_ptype$Year==b[i]]<-sqldf("select avg(infl_vat_adj_jointworking) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=mean)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_jointwork%>%filter(year %in% b[i])
  table_ptype$St_dev[table_ptype$Year==b[i]]<-sqldf("select STDEV(infl_vat_adj_jointworking) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_jointwork%>%filter(year %in% b[i])
  table_ptype$Median[table_ptype$Year==b[i]]<- median(seged_2$infl_vat_adj_jointworking)
  rm(seged_2)
  
}



aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_ptype = table_ptype[-(5:9),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_jointwork%>%filter(year %in% b[i])
  table_ptype$Q1[table_ptype$Year==b[i]]<- quantile(seged_2$infl_vat_adj_jointworking, 0.25)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_jointwork%>%filter(year %in% b[i])
  table_ptype$Q3[table_ptype$Year==b[i]]<- quantile(seged_2$infl_vat_adj_jointworking, 0.75)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(5:12),]



options(digits=10)



table_ptype$Year = as.numeric(table_ptype$Year)
table_ptype$Value_p = as.numeric(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = as.numeric(table_ptype$Number_p, digits = 2)
table_ptype$Mean = as.numeric(table_ptype$Mean, digits = 2)
table_ptype$St_dev = as.numeric(table_ptype$St_dev, digits = 2)
table_ptype$Median = as.numeric(table_ptype$Median, digits = 2)
table_ptype$Q1 = as.numeric(table_ptype$Q1, digits = 2)
table_ptype$Q3 = as.numeric(table_ptype$Q3, digits = 2)

table_ptype$Value_p = paste('£',formatC(table_ptype$Value_p, big.mark=',', format = 'f', digits = 2))
table_ptype$Mean = paste('£',formatC(table_ptype$Mean, big.mark=',', format = 'f', digits = 2))
table_ptype$St_dev = paste('£',formatC(table_ptype$St_dev, big.mark=',', format = 'f', digits = 2))
table_ptype$Median = paste('£',formatC(table_ptype$Median, big.mark=',', format = 'f', digits = 2))
table_ptype$Q1 = paste('£',formatC(table_ptype$Q1, big.mark=',', format = 'f', digits = 2))
table_ptype$Q3 = paste('£',formatC(table_ptype$Q3, big.mark=',', format = 'f', digits = 2))
table_ptype$Number_p = prettyNum(table_ptype$Number_p, big.mark = ",", scientific = FALSE)



table_ptype$median_iqr <- paste(table_ptype$Median, "(",   table_ptype$Q1, "-", table_ptype$Q3, ")")

table_ptype = select(table_ptype, -c("Median", "Q1", "Q3"))


JonckheereTerpstraTest(trust_jointwork$infl_vat_adj_jointworking, trust_jointwork$year, nperm = 1000, alternative = "decreasing")

pwt_hco = pairwise.wilcox.test(trust_jointwork$infl_vat_adj_jointworking, trust_jointwork$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_ptype = left_join(table_ptype, pwt_hco, by = "Year")

table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

summary(trust_data$infl_vat_adj_jointworking)
sum(trust_data$infl_vat_adj_jointworking)
sd(trust_data$infl_vat_adj_jointworking)




##### Jointwork Total



table_hco<-data.frame(Number_p = character(0) , Value_p = character(0) ,  Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))


for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_jointwork
  table_hco$Number_p <- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_jointwork
  table_hco$Value_p<- sum(seged_2$final_tov) 
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]





for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_jointwork
  table_hco$Mean<-mean(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_jointwork
  table_hco$St_dev<-sd(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_jointwork
  table_hco$Median<-median(seged_2$final_tov)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]





for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_jointwork
  table_hco$Q1<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_jointwork
  table_hco$Q3<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)

table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))






################### DONATIONS

table_ptype<-data.frame(Number_p = character(0) , Value_p = character(0) ,  Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

b<-c("2015","2016","2017","2018")


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_donation%>%filter(year %in% b[i])
  table_ptype$Number_p[table_ptype$Year==b[i]]<- length(which(seged_2$infl_vat_adj_donations != 0))
  rm(seged_2)
  
}

table_ptype = unique(table_ptype)


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_donation%>%filter(year %in% b[i])
  table_ptype$Value_p[table_ptype$Year==b[i]]<-sqldf("select sum(infl_vat_adj_donations) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_donation%>%filter(year %in% b[i])
  table_ptype$Mean[table_ptype$Year==b[i]]<-sqldf("select avg(infl_vat_adj_donations) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=mean)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_donation%>%filter(year %in% b[i])
  table_ptype$St_dev[table_ptype$Year==b[i]]<-sqldf("select STDEV(infl_vat_adj_donations) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_donation%>%filter(year %in% b[i])
  table_ptype$Median[table_ptype$Year==b[i]]<- median(seged_2$infl_vat_adj_donations)
  rm(seged_2)
  
}



aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_ptype = table_ptype[-(5:9),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_donation%>%filter(year %in% b[i])
  table_ptype$Q1[table_ptype$Year==b[i]]<- quantile(seged_2$infl_vat_adj_donations, 0.25)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_donation%>%filter(year %in% b[i])
  table_ptype$Q3[table_ptype$Year==b[i]]<- quantile(seged_2$infl_vat_adj_donations, 0.75)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(5:12),]



options(digits=10)



table_ptype$Year = as.numeric(table_ptype$Year)
table_ptype$Value_p = as.numeric(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = as.numeric(table_ptype$Number_p, digits = 2)
table_ptype$Mean = as.numeric(table_ptype$Mean, digits = 2)
table_ptype$St_dev = as.numeric(table_ptype$St_dev, digits = 2)
table_ptype$Median = as.numeric(table_ptype$Median, digits = 2)
table_ptype$Q1 = as.numeric(table_ptype$Q1, digits = 2)
table_ptype$Q3 = as.numeric(table_ptype$Q3, digits = 2)

table_ptype$Value_p = paste('£',formatC(table_ptype$Value_p, big.mark=',', format = 'f', digits = 2))
table_ptype$Mean = paste('£',formatC(table_ptype$Mean, big.mark=',', format = 'f', digits = 2))
table_ptype$St_dev = paste('£',formatC(table_ptype$St_dev, big.mark=',', format = 'f', digits = 2))
table_ptype$Median = paste('£',formatC(table_ptype$Median, big.mark=',', format = 'f', digits = 2))
table_ptype$Q1 = paste('£',formatC(table_ptype$Q1, big.mark=',', format = 'f', digits = 2))
table_ptype$Q3 = paste('£',formatC(table_ptype$Q3, big.mark=',', format = 'f', digits = 2))
table_ptype$Number_p = prettyNum(table_ptype$Number_p, big.mark = ",", scientific = FALSE)



table_ptype$median_iqr <- paste(table_ptype$Median, "(",   table_ptype$Q1, "-", table_ptype$Q3, ")")

table_ptype = select(table_ptype, -c("Median", "Q1", "Q3"))


JonckheereTerpstraTest(trust_donation$infl_vat_adj_donations, trust_donation$year, nperm = 1000, alternative = "decreasing")

pwt_hco = pairwise.wilcox.test(trust_donation$infl_vat_adj_donations, trust_donation$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_ptype = left_join(table_ptype, pwt_hco, by = "Year")

summary(trust_data$infl_vat_adj_donations)
sum(trust_data$infl_vat_adj_donations)
sd(trust_data$infl_vat_adj_donations)


table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))





##### Donations Total



table_hco<-data.frame(Number_p = character(0) , Value_p = character(0) ,  Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))


for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_donation
  table_hco$Number_p <- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_donation
  table_hco$Value_p<- sum(seged_2$final_tov) 
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]





for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_donation
  table_hco$Mean<-mean(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_donation
  table_hco$St_dev<-sd(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_donation
  table_hco$Median<-median(seged_2$final_tov)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]





for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_donation
  table_hco$Q1<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_donation
  table_hco$Q3<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)

table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))





################### COST OF EVENTS

table_ptype<-data.frame(Number_p = character(0) , Value_p = character(0) ,  Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

b<-c("2015","2016","2017","2018")


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_events%>%filter(year %in% b[i])
  table_ptype$Number_p[table_ptype$Year==b[i]]<- length(which(seged_2$infl_vat_adj_costsofevents != 0))
  rm(seged_2)
  
}

table_ptype = unique(table_ptype)


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_events%>%filter(year %in% b[i])
  table_ptype$Value_p[table_ptype$Year==b[i]]<-sqldf("select sum(infl_vat_adj_costsofevents) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_events%>%filter(year %in% b[i])
  table_ptype$Mean[table_ptype$Year==b[i]]<-sqldf("select avg(infl_vat_adj_costsofevents) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=mean)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_events%>%filter(year %in% b[i])
  table_ptype$St_dev[table_ptype$Year==b[i]]<-sqldf("select STDEV(infl_vat_adj_costsofevents) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_events%>%filter(year %in% b[i])
  table_ptype$Median[table_ptype$Year==b[i]]<- median(seged_2$infl_vat_adj_costsofevents)
  rm(seged_2)
  
}



aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_ptype = table_ptype[-(5:9),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_events%>%filter(year %in% b[i])
  table_ptype$Q1[table_ptype$Year==b[i]]<- quantile(seged_2$infl_vat_adj_costsofevents, 0.25)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_events%>%filter(year %in% b[i])
  table_ptype$Q3[table_ptype$Year==b[i]]<- quantile(seged_2$infl_vat_adj_costsofevents, 0.75)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(5:12),]



options(digits=10)



table_ptype$Year = as.numeric(table_ptype$Year)
table_ptype$Value_p = as.numeric(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = as.numeric(table_ptype$Number_p, digits = 2)
table_ptype$Mean = as.numeric(table_ptype$Mean, digits = 2)
table_ptype$St_dev = as.numeric(table_ptype$St_dev, digits = 2)
table_ptype$Median = as.numeric(table_ptype$Median, digits = 2)
table_ptype$Q1 = as.numeric(table_ptype$Q1, digits = 2)
table_ptype$Q3 = as.numeric(table_ptype$Q3, digits = 2)

table_ptype$Value_p = paste('£',formatC(table_ptype$Value_p, big.mark=',', format = 'f', digits = 2))
table_ptype$Mean = paste('£',formatC(table_ptype$Mean, big.mark=',', format = 'f', digits = 2))
table_ptype$St_dev = paste('£',formatC(table_ptype$St_dev, big.mark=',', format = 'f', digits = 2))
table_ptype$Median = paste('£',formatC(table_ptype$Median, big.mark=',', format = 'f', digits = 2))
table_ptype$Q1 = paste('£',formatC(table_ptype$Q1, big.mark=',', format = 'f', digits = 2))
table_ptype$Q3 = paste('£',formatC(table_ptype$Q3, big.mark=',', format = 'f', digits = 2))
table_ptype$Number_p = prettyNum(table_ptype$Number_p, big.mark = ",", scientific = FALSE)


table_ptype$median_iqr <- paste(table_ptype$Median, "(",   table_ptype$Q1, "-", table_ptype$Q3, ")")

table_ptype = select(table_ptype, -c("Median", "Q1", "Q3"))


JonckheereTerpstraTest(trust_events$infl_vat_adj_costsofevents, trust_events$year, nperm = 1000, alternative = "decreasing")

pwt_hco = pairwise.wilcox.test(trust_events$infl_vat_adj_costsofevents, trust_events$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_ptype = left_join(table_ptype, pwt_hco, by = "Year")


table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

summary(trust_data$infl_vat_adj_donations)
sum(trust_data$infl_vat_adj_donations)
sd(trust_data$infl_vat_adj_donations)






##### Events Total



table_hco<-data.frame(Number_p = character(0) , Value_p = character(0) ,  Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))


for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_events
  table_hco$Number_p <- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_events
  table_hco$Value_p<- sum(seged_2$final_tov) 
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]





for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_events
  table_hco$Mean<-mean(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_events
  table_hco$St_dev<-sd(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_events
  table_hco$Median<-median(seged_2$final_tov)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]





for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_events
  table_hco$Q1<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_events
  table_hco$Q3<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)

table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))



################### SERVICE

table_ptype<-data.frame(Number_p = character(0) , Value_p = character(0) ,  Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

b<-c("2015","2016","2017","2018")


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_service%>%filter(year %in% b[i])
  table_ptype$Number_p[table_ptype$Year==b[i]]<- length(which(seged_2$infl_vat_adj_service != 0))
  rm(seged_2)
  
}

table_ptype = unique(table_ptype)


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_service%>%filter(year %in% b[i])
  table_ptype$Value_p[table_ptype$Year==b[i]]<-sqldf("select sum(infl_vat_adj_service) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_service%>%filter(year %in% b[i])
  table_ptype$Mean[table_ptype$Year==b[i]]<-sqldf("select avg(infl_vat_adj_service) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=mean)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_service%>%filter(year %in% b[i])
  table_ptype$St_dev[table_ptype$Year==b[i]]<-sqldf("select STDEV(infl_vat_adj_service) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_service%>%filter(year %in% b[i])
  table_ptype$Median[table_ptype$Year==b[i]]<- median(seged_2$infl_vat_adj_service)
  rm(seged_2)
  
}



aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_ptype = table_ptype[-(5:9),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_service%>%filter(year %in% b[i])
  table_ptype$Q1[table_ptype$Year==b[i]]<- quantile(seged_2$infl_vat_adj_service, 0.25)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_service%>%filter(year %in% b[i])
  table_ptype$Q3[table_ptype$Year==b[i]]<- quantile(seged_2$infl_vat_adj_service, 0.75)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(5:12),]



options(digits=10)



table_ptype$Year = as.numeric(table_ptype$Year)
table_ptype$Value_p = as.numeric(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = as.numeric(table_ptype$Number_p, digits = 2)
table_ptype$Mean = as.numeric(table_ptype$Mean, digits = 2)
table_ptype$St_dev = as.numeric(table_ptype$St_dev, digits = 2)
table_ptype$Median = as.numeric(table_ptype$Median, digits = 2)
table_ptype$Q1 = as.numeric(table_ptype$Q1, digits = 2)
table_ptype$Q3 = as.numeric(table_ptype$Q3, digits = 2)


table_ptype$Value_p = paste('£',formatC(table_ptype$Value_p, big.mark=',', format = 'f', digits = 2))
table_ptype$Mean = paste('£',formatC(table_ptype$Mean, big.mark=',', format = 'f', digits = 2))
table_ptype$St_dev = paste('£',formatC(table_ptype$St_dev, big.mark=',', format = 'f', digits = 2))
table_ptype$Median = paste('£',formatC(table_ptype$Median, big.mark=',', format = 'f', digits = 2))
table_ptype$Q1 = paste('£',formatC(table_ptype$Q1, big.mark=',', format = 'f', digits = 2))
table_ptype$Q3 = paste('£',formatC(table_ptype$Q3, big.mark=',', format = 'f', digits = 2))
table_ptype$Number_p = prettyNum(table_ptype$Number_p, big.mark = ",", scientific = FALSE)



table_ptype$median_iqr <- paste(table_ptype$Median, "(",   table_ptype$Q1, "-", table_ptype$Q3, ")")

table_ptype = select(table_ptype, -c("Median", "Q1", "Q3"))


JonckheereTerpstraTest(trust_service$infl_vat_adj_service, trust_service$year, nperm = 1000, alternative = "decreasing")

pwt_hco = pairwise.wilcox.test(trust_service$infl_vat_adj_service, trust_service$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_ptype = left_join(table_ptype, pwt_hco, by = "Year")

table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

summary(trust_data$infl_vat_adj_donations)
sum(trust_data$infl_vat_adj_donations)
sd(trust_data$infl_vat_adj_donations)




##### Service Total



table_hco<-data.frame(Number_p = character(0) , Value_p = character(0) ,  Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))


for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_service
  table_hco$Number_p <- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_service
  table_hco$Value_p<- sum(seged_2$final_tov) 
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]





for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_service
  table_hco$Mean<-mean(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_service
  table_hco$St_dev<-sd(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_service
  table_hco$Median<-median(seged_2$final_tov)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]





for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_service
  table_hco$Q1<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


for(i in 1){
  
  seged_1<-data.frame(Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_service
  table_hco$Q3<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)

table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))







##################################################################################################################
##################################################################################################################
################### PAYMENT TYPE BY YEAR

trust_long = select(trust_data, -c("final_tov"))

library(tidyr)
trust_long <- trust_long %>% gather(payment_type, value, -c("row", "trust", "company", "year", "Merged trust name", "Region", "Postcode", "Standardised trust postcode", "vat", "binary_vat", "trust_type", "foundation_trust"))


trust_long_2015 <- trust_long[ which(trust_long$year=='2015'),]
trust_long_2016 <- trust_long[ which(trust_long$year=='2016'),]
trust_long_2017 <- trust_long[ which(trust_long$year=='2017'),]
trust_long_2018 <- trust_long[ which(trust_long$year=='2018'),]

trust_long_2015 <- trust_long_2015[ which(trust_long_2015$value!=0),]
trust_long_2016 <- trust_long_2016[ which(trust_long_2016$value!=0),]
trust_long_2017 <- trust_long_2017[ which(trust_long_2017$value!=0),]
trust_long_2018 <- trust_long_2018[ which(trust_long_2018$value!=0),]

trust_long <- trust_long[ which(trust_long$value!=0),]



table_ptype<-data.frame(Payment_type = character(0), Number_p = character(0) , Value_p = character(0) ,  Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))


b<-c("infl_vat_adj_costsofevents","infl_vat_adj_donations","infl_vat_adj_jointworking","infl_vat_adj_service")




for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2015%>%filter(payment_type %in% b[i])
  table_ptype$Number_p[table_ptype$Payment_type==b[i]]<- length(which(seged_2$value != 0))
  rm(seged_2)
  
}

table_ptype = unique(table_ptype)


for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2015%>%filter(payment_type %in% b[i])
  table_ptype$Value_p[table_ptype$Payment_type==b[i]]<-sqldf("select sum(value) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2015%>%filter(payment_type %in% b[i])
  table_ptype$Mean[table_ptype$Payment_type==b[i]]<-sqldf("select avg(value) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=mean)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2015%>%filter(payment_type %in% b[i])
  table_ptype$St_dev[table_ptype$Payment_type==b[i]]<-sqldf("select STDEV(value) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2015%>%filter(payment_type %in% b[i])
  table_ptype$Median[table_ptype$Payment_type==b[i]]<- median(seged_2$value)
  rm(seged_2)
  
}



aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_ptype = table_ptype[-(5:9),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2015%>%filter(payment_type %in% b[i])
  table_ptype$Q1[table_ptype$Payment_type==b[i]]<- quantile(seged_2$value, 0.25)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2015%>%filter(payment_type %in% b[i])
  table_ptype$Q3[table_ptype$Payment_type==b[i]]<- quantile(seged_2$value, 0.75)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(5:12),]





options(digits=10)



table_ptype$Value_p = as.numeric(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = as.numeric(table_ptype$Number_p, digits = 2)
table_ptype$Mean = as.numeric(table_ptype$Mean, digits = 2)
table_ptype$St_dev = as.numeric(table_ptype$St_dev, digits = 2)
table_ptype$Median = as.numeric(table_ptype$Median, digits = 2)
table_ptype$Q1 = as.numeric(table_ptype$Q1, digits = 2)
table_ptype$Q3 = as.numeric(table_ptype$Q3, digits = 2)


table_ptype$Value_p = paste('£',formatC(table_ptype$Value_p, big.mark=',', format = 'f', digits = 2))
table_ptype$Mean = paste('£',formatC(table_ptype$Mean, big.mark=',', format = 'f', digits = 2))
table_ptype$St_dev = paste('£',formatC(table_ptype$St_dev, big.mark=',', format = 'f', digits = 2))
table_ptype$Median = paste('£',formatC(table_ptype$Median, big.mark=',', format = 'f', digits = 2))
table_ptype$Q1 = paste('£',formatC(table_ptype$Q1, big.mark=',', format = 'f', digits = 2))
table_ptype$Q3 = paste('£',formatC(table_ptype$Q3, big.mark=',', format = 'f', digits = 2))
table_ptype$Number_p = prettyNum(table_ptype$Number_p, big.mark = ",", scientific = FALSE)



table_ptype$median_iqr <- paste(table_ptype$Median, "(",   table_ptype$Q1, "-", table_ptype$Q3, ")")

table_ptype = select(table_ptype, -c("Median", "Q1", "Q3"))


pwt_hco = pairwise.wilcox.test(trust_long_2015$value, trust_long_2015$payment_type, p.adjust.method = "bonf")

pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Payment_type <- row.names(pwt_hco) 


table_ptype = left_join(table_ptype, pwt_hco, by = "Payment_type")

table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

summary(trust_data$infl_vat_adj_donations)
sum(trust_data$infl_vat_adj_donations)
sd(trust_data$infl_vat_adj_donations)


#### 2016


table_ptype<-data.frame(Payment_type = character(0), Number_p = character(0) , Value_p = character(0) ,  Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))


b<-c("infl_vat_adj_costsofevents","infl_vat_adj_donations","infl_vat_adj_jointworking","infl_vat_adj_service")




for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2016%>%filter(payment_type %in% b[i])
  table_ptype$Number_p[table_ptype$Payment_type==b[i]]<- length(which(seged_2$value != 0))
  rm(seged_2)
  
}

table_ptype = unique(table_ptype)


for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2016%>%filter(payment_type %in% b[i])
  table_ptype$Value_p[table_ptype$Payment_type==b[i]]<-sqldf("select sum(value) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2016%>%filter(payment_type %in% b[i])
  table_ptype$Mean[table_ptype$Payment_type==b[i]]<-sqldf("select avg(value) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=mean)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2016%>%filter(payment_type %in% b[i])
  table_ptype$St_dev[table_ptype$Payment_type==b[i]]<-sqldf("select STDEV(value) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2016%>%filter(payment_type %in% b[i])
  table_ptype$Median[table_ptype$Payment_type==b[i]]<- median(seged_2$value)
  rm(seged_2)
  
}



aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_ptype = table_ptype[-(5:9),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2016%>%filter(payment_type %in% b[i])
  table_ptype$Q1[table_ptype$Payment_type==b[i]]<- quantile(seged_2$value, 0.25)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2016%>%filter(payment_type %in% b[i])
  table_ptype$Q3[table_ptype$Payment_type==b[i]]<- quantile(seged_2$value, 0.75)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(5:12),]





options(digits=10)



table_ptype$Value_p = as.numeric(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = as.numeric(table_ptype$Number_p, digits = 2)
table_ptype$Mean = as.numeric(table_ptype$Mean, digits = 2)
table_ptype$St_dev = as.numeric(table_ptype$St_dev, digits = 2)
table_ptype$Median = as.numeric(table_ptype$Median, digits = 2)
table_ptype$Q1 = as.numeric(table_ptype$Q1, digits = 2)
table_ptype$Q3 = as.numeric(table_ptype$Q3, digits = 2)


table_ptype$Value_p = round(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = round(table_ptype$Number_p, digits = 2)
table_ptype$Mean = round(table_ptype$Mean, digits = 2)
table_ptype$St_dev = round(table_ptype$St_dev, digits = 2)
table_ptype$Median = round(table_ptype$Median, digits = 2)
table_ptype$Q1 = round(table_ptype$Q1, digits = 2)
table_ptype$Q3 = round(table_ptype$Q3, digits = 2)



table_ptype$median_iqr <- paste(table_ptype$Median, "(",   table_ptype$Q1, "-", table_ptype$Q3, ")")

table_ptype = select(table_ptype, -c("Median", "Q1", "Q3"))


pwt_hco = pairwise.wilcox.test(trust_long_2016$value, trust_long_2016$payment_type, p.adjust.method = "bonf")

pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Payment_type <- row.names(pwt_hco) 


table_ptype = left_join(table_ptype, pwt_hco, by = "Payment_type")

table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

summary(trust_data$infl_vat_adj_donations)
sum(trust_data$infl_vat_adj_donations)
sd(trust_data$infl_vat_adj_donations)


sum(trust_long_2016$value != 0)
sum(trust_long_2015$value != 0)
sum(trust_long_2017$value != 0)

#### 2017


table_ptype<-data.frame(Payment_type = character(0), Number_p = character(0) , Value_p = character(0) ,  Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))


b<-c("infl_vat_adj_costsofevents","infl_vat_adj_donations","infl_vat_adj_jointworking","infl_vat_adj_service")




for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2017%>%filter(payment_type %in% b[i])
  table_ptype$Number_p[table_ptype$Payment_type==b[i]]<- length(which(seged_2$value != 0))
  rm(seged_2)
  
}

table_ptype = unique(table_ptype)


for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2017%>%filter(payment_type %in% b[i])
  table_ptype$Value_p[table_ptype$Payment_type==b[i]]<-sqldf("select sum(value) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2017%>%filter(payment_type %in% b[i])
  table_ptype$Mean[table_ptype$Payment_type==b[i]]<-sqldf("select avg(value) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=mean)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2017%>%filter(payment_type %in% b[i])
  table_ptype$St_dev[table_ptype$Payment_type==b[i]]<-sqldf("select STDEV(value) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2017%>%filter(payment_type %in% b[i])
  table_ptype$Median[table_ptype$Payment_type==b[i]]<- median(seged_2$value)
  rm(seged_2)
  
}



aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_ptype = table_ptype[-(5:9),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2017%>%filter(payment_type %in% b[i])
  table_ptype$Q1[table_ptype$Payment_type==b[i]]<- quantile(seged_2$value, 0.25)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2017%>%filter(payment_type %in% b[i])
  table_ptype$Q3[table_ptype$Payment_type==b[i]]<- quantile(seged_2$value, 0.75)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(5:12),]





options(digits=10)



table_ptype$Value_p = as.numeric(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = as.numeric(table_ptype$Number_p, digits = 2)
table_ptype$Mean = as.numeric(table_ptype$Mean, digits = 2)
table_ptype$St_dev = as.numeric(table_ptype$St_dev, digits = 2)
table_ptype$Median = as.numeric(table_ptype$Median, digits = 2)
table_ptype$Q1 = as.numeric(table_ptype$Q1, digits = 2)
table_ptype$Q3 = as.numeric(table_ptype$Q3, digits = 2)


table_ptype$Value_p = round(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = round(table_ptype$Number_p, digits = 2)
table_ptype$Mean = round(table_ptype$Mean, digits = 2)
table_ptype$St_dev = round(table_ptype$St_dev, digits = 2)
table_ptype$Median = round(table_ptype$Median, digits = 2)
table_ptype$Q1 = round(table_ptype$Q1, digits = 2)
table_ptype$Q3 = round(table_ptype$Q3, digits = 2)



table_ptype$median_iqr <- paste(table_ptype$Median, "(",   table_ptype$Q1, "-", table_ptype$Q3, ")")

table_ptype = select(table_ptype, -c("Median", "Q1", "Q3"))


pwt_hco = pairwise.wilcox.test(trust_long_2017$value, trust_long_2017$payment_type, p.adjust.method = "bonf")

pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Payment_type <- row.names(pwt_hco) 


table_ptype = left_join(table_ptype, pwt_hco, by = "Payment_type")

summary(trust_data$infl_vat_adj_donations)
sum(trust_data$infl_vat_adj_donations)
sd(trust_data$infl_vat_adj_donations)






#### 2018


table_ptype<-data.frame(Payment_type = character(0), Number_p = character(0) , Value_p = character(0) ,  Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))


b<-c("infl_vat_adj_costsofevents","infl_vat_adj_donations","infl_vat_adj_jointworking","infl_vat_adj_service")




for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2018%>%filter(payment_type %in% b[i])
  table_ptype$Number_p[table_ptype$Payment_type==b[i]]<- length(which(seged_2$value != 0))
  rm(seged_2)
  
}

table_ptype = unique(table_ptype)


for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2018%>%filter(payment_type %in% b[i])
  table_ptype$Value_p[table_ptype$Payment_type==b[i]]<-sqldf("select sum(value) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2018%>%filter(payment_type %in% b[i])
  table_ptype$Mean[table_ptype$Payment_type==b[i]]<-sqldf("select avg(value) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=mean)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2018%>%filter(payment_type %in% b[i])
  table_ptype$St_dev[table_ptype$Payment_type==b[i]]<-sqldf("select STDEV(value) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2018%>%filter(payment_type %in% b[i])
  table_ptype$Median[table_ptype$Payment_type==b[i]]<- median(seged_2$value)
  rm(seged_2)
  
}



aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_ptype = table_ptype[-(5:9),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2018%>%filter(payment_type %in% b[i])
  table_ptype$Q1[table_ptype$Payment_type==b[i]]<- quantile(seged_2$value, 0.25)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2018%>%filter(payment_type %in% b[i])
  table_ptype$Q3[table_ptype$Payment_type==b[i]]<- quantile(seged_2$value, 0.75)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(5:12),]





options(digits=10)



table_ptype$Value_p = as.numeric(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = as.numeric(table_ptype$Number_p, digits = 2)
table_ptype$Mean = as.numeric(table_ptype$Mean, digits = 2)
table_ptype$St_dev = as.numeric(table_ptype$St_dev, digits = 2)
table_ptype$Median = as.numeric(table_ptype$Median, digits = 2)
table_ptype$Q1 = as.numeric(table_ptype$Q1, digits = 2)
table_ptype$Q3 = as.numeric(table_ptype$Q3, digits = 2)


table_ptype$Value_p = round(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = round(table_ptype$Number_p, digits = 2)
table_ptype$Mean = round(table_ptype$Mean, digits = 2)
table_ptype$St_dev = round(table_ptype$St_dev, digits = 2)
table_ptype$Median = round(table_ptype$Median, digits = 2)
table_ptype$Q1 = round(table_ptype$Q1, digits = 2)
table_ptype$Q3 = round(table_ptype$Q3, digits = 2)



table_ptype$median_iqr <- paste(table_ptype$Median, "(",   table_ptype$Q1, "-", table_ptype$Q3, ")")

table_ptype = select(table_ptype, -c("Median", "Q1", "Q3"))


pwt_hco = pairwise.wilcox.test(trust_long_2018$value, trust_long_2018$payment_type, p.adjust.method = "bonf")

pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Payment_type <- row.names(pwt_hco) 


table_ptype = left_join(table_ptype, pwt_hco, by = "Payment_type")

summary(trust_data$infl_vat_adj_donations)
sum(trust_data$infl_vat_adj_donations)
sd(trust_data$infl_vat_adj_donations)




#### ALL YEARS


table_ptype<-data.frame(Payment_type = character(0), Number_p = character(0) , Value_p = character(0) ,  Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))


b<-c("infl_vat_adj_costsofevents","infl_vat_adj_donations","infl_vat_adj_jointworking","infl_vat_adj_service")




for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long%>%filter(payment_type %in% b[i])
  table_ptype$Number_p[table_ptype$Payment_type==b[i]]<- length(which(seged_2$value != 0))
  rm(seged_2)
  
}

table_ptype = unique(table_ptype)


for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long%>%filter(payment_type %in% b[i])
  table_ptype$Value_p[table_ptype$Payment_type==b[i]]<-sqldf("select sum(value) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long%>%filter(payment_type %in% b[i])
  table_ptype$Mean[table_ptype$Payment_type==b[i]]<-sqldf("select avg(value) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=mean)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long%>%filter(payment_type %in% b[i])
  table_ptype$St_dev[table_ptype$Payment_type==b[i]]<-sqldf("select STDEV(value) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_ptype = table_ptype[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long%>%filter(payment_type %in% b[i])
  table_ptype$Median[table_ptype$Payment_type==b[i]]<- median(seged_2$value)
  rm(seged_2)
  
}



aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_ptype = table_ptype[-(5:9),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long%>%filter(payment_type %in% b[i])
  table_ptype$Q1[table_ptype$Payment_type==b[i]]<- quantile(seged_2$value, 0.25)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Payment_type=b[i], Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long%>%filter(payment_type %in% b[i])
  table_ptype$Q3[table_ptype$Payment_type==b[i]]<- quantile(seged_2$value, 0.75)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(5:12),]





options(digits=10)



table_ptype$Value_p = as.numeric(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = as.numeric(table_ptype$Number_p, digits = 2)
table_ptype$Mean = as.numeric(table_ptype$Mean, digits = 2)
table_ptype$St_dev = as.numeric(table_ptype$St_dev, digits = 2)
table_ptype$Median = as.numeric(table_ptype$Median, digits = 2)
table_ptype$Q1 = as.numeric(table_ptype$Q1, digits = 2)
table_ptype$Q3 = as.numeric(table_ptype$Q3, digits = 2)


table_ptype$Value_p = round(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = round(table_ptype$Number_p, digits = 2)
table_ptype$Mean = round(table_ptype$Mean, digits = 2)
table_ptype$St_dev = round(table_ptype$St_dev, digits = 2)
table_ptype$Median = round(table_ptype$Median, digits = 2)
table_ptype$Q1 = round(table_ptype$Q1, digits = 2)
table_ptype$Q3 = round(table_ptype$Q3, digits = 2)



table_ptype$median_iqr <- paste(table_ptype$Median, "(",   table_ptype$Q1, "-", table_ptype$Q3, ")")

table_ptype = select(table_ptype, -c("Median", "Q1", "Q3"))


pwt_hco = pairwise.wilcox.test(trust_long$value, trust_long$payment_type, p.adjust.method = "bonf")

pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Payment_type <- row.names(pwt_hco) 


table_ptype = left_join(table_ptype, pwt_hco, by = "Payment_type")

summary(trust_data$infl_vat_adj_donations)
sum(trust_data$infl_vat_adj_donations)
sd(trust_data$infl_vat_adj_donations)





######### testing the trust table

test_trust1 = select(hco_trust, row,year,Region)
test_trust1 = select(trust_data, row,year,Region)



test_trust = left_join(test_trust1, test_trust2, by = c("row", "year"))
library(writexl)
write_xlsx(trust_data_panel_sum, "/Users/esztersaghy/Desktop/2022\ NHS\ Trusts/2022\ Data\\trust_data_panel_sum.xlsx")

table(test_trust$Region.y)

original_data = read_excel(file.choose())



test_trust = left_join(test_trust1, test_trust2, by = c("row", "year"))


extra_rows <- test_trust[ which(is.na(test_trust$Region.y)),]

test_trust2 = left_join(extra_rows, original_data, by = c("row", "year"))






######### Payment value categorisation

hco_data$jointwork_cat = ifelse(hco_data$infl_vat_adj_jointworking == 0, "£0",
                             ifelse(trust_data$infl_vat_adj_jointworking > 0  & hco_data$infl_vat_adj_jointworking < 100, "<£100",
                                ifelse(hco_data$infl_vat_adj_jointworking >= 100 & hco_data$infl_vat_adj_jointworking < 1000, "£100 to £1,000",
                                       ifelse(hco_data$infl_vat_adj_jointworking >= 1000 & hco_data$infl_vat_adj_jointworking < 10000, "£1000 to £10,000",
                                              ifelse(hco_data$infl_vat_adj_jointworking >= 10000 & hco_data$infl_vat_adj_jointworking < 100000, "£10,000 to £100,000",
                                                     ifelse(hco_data$infl_vat_adj_jointworking >= 100000 & hco_data$infl_vat_adj_jointworking < 500000, "£100,000 to £500,000",
                                                            ifelse(hco_data$infl_vat_adj_jointworking >= 500000, "≥£500,000", "other")))))))


hco_data$donations_cat = ifelse(hco_data$infl_vat_adj_donations == 0, "£0",
                                ifelse(trust_data$infl_vat_adj_donations > 0  & hco_data$infl_vat_adj_donations < 100, "<£100",
                                       ifelse(hco_data$infl_vat_adj_donations >= 100 & hco_data$infl_vat_adj_donations < 1000, "£100 to £1,000",
                                              ifelse(hco_data$infl_vat_adj_donations >= 1000 & hco_data$infl_vat_adj_donations < 10000, "£1000 to £10,000",
                                                     ifelse(hco_data$infl_vat_adj_donations >= 10000 & hco_data$infl_vat_adj_donations < 100000, "£10,000 to £100,000",
                                                            ifelse(hco_data$infl_vat_adj_donations >= 100000 & hco_data$infl_vat_adj_donations < 500000, "£100,000 to £500,000",
                                                                   ifelse(hco_data$infl_vat_adj_donations >= 500000, "≥£500,000", "other")))))))


hco_data$events_cat = ifelse(hco_data$infl_vat_adj_costsofevents == 0, "£0",
                                ifelse(trust_data$infl_vat_adj_costsofevents > 0  & hco_data$infl_vat_adj_costsofevents < 100, "<£100",
                                       ifelse(hco_data$infl_vat_adj_costsofevents >= 100 & hco_data$infl_vat_adj_costsofevents < 1000, "£100 to £1,000",
                                              ifelse(hco_data$infl_vat_adj_costsofevents >= 1000 & hco_data$infl_vat_adj_costsofevents < 10000, "£1000 to £10,000",
                                                     ifelse(hco_data$infl_vat_adj_costsofevents >= 10000 & hco_data$infl_vat_adj_costsofevents < 100000, "£10,000 to £100,000",
                                                            ifelse(hco_data$infl_vat_adj_costsofevents >= 100000 & hco_data$infl_vat_adj_costsofevents < 500000, "£100,000 to £500,000",
                                                                   ifelse(hco_data$infl_vat_adj_costsofevents >= 500000, "≥£500,000", "other")))))))


hco_data$service_cat = ifelse(hco_data$infl_vat_adj_service == 0, "£0",
                             ifelse(trust_data$infl_vat_adj_service > 0  & hco_data$infl_vat_adj_service < 100, "<£100",
                                    ifelse(hco_data$infl_vat_adj_service >= 100 & hco_data$infl_vat_adj_service < 1000, "£100 to £1,000",
                                           ifelse(hco_data$infl_vat_adj_service >= 1000 & hco_data$infl_vat_adj_service < 10000, "£1000 to £10,000",
                                                  ifelse(hco_data$infl_vat_adj_service >= 10000 & hco_data$infl_vat_adj_service < 100000, "£10,000 to £100,000",
                                                         ifelse(hco_data$infl_vat_adj_service >= 100000 & hco_data$infl_vat_adj_service < 500000, "£100,000 to £500,000",
                                                                ifelse(hco_data$infl_vat_adj_service >= 500000, "≥£500,000", "other")))))))



hco_data$final_tov_cat = ifelse(hco_data$final_tov == 0, "£0",
                              ifelse(hco_data$final_tov > 0  & hco_data$final_tov < 100, "<£100",
                                     ifelse(hco_data$final_tov >= 100 & hco_data$final_tov < 1000, "£100 to £1,000",
                                            ifelse(hco_data$final_tov >= 1000 & hco_data$final_tov < 10000, "£1000 to £10,000",
                                                   ifelse(hco_data$final_tov >= 10000 & hco_data$final_tov < 100000, "£10,000 to £100,000",
                                                          ifelse(hco_data$final_tov >= 100000 & hco_data$final_tov < 500000, "£100,000 to £500,000",
                                                                 ifelse(hco_data$final_tov >= 500000, "≥£500,000", "other")))))))





trust_data$jointwork_cat = ifelse(trust_data$infl_vat_adj_jointworking == 0, "£0",
                                ifelse(trust_data$infl_vat_adj_jointworking > 0  & trust_data$infl_vat_adj_jointworking < 100, "<£100",
                                       ifelse(trust_data$infl_vat_adj_jointworking >= 100 & trust_data$infl_vat_adj_jointworking < 1000, "£100 to £1,000",
                                              ifelse(trust_data$infl_vat_adj_jointworking >= 1000 & trust_data$infl_vat_adj_jointworking < 10000, "£1000 to £10,000",
                                                     ifelse(trust_data$infl_vat_adj_jointworking >= 10000 & trust_data$infl_vat_adj_jointworking < 100000, "£10,000 to £100,000",
                                                            ifelse(trust_data$infl_vat_adj_jointworking >= 100000 & trust_data$infl_vat_adj_jointworking < 500000, "£100,000 to £500,000",
                                                                   ifelse(trust_data$infl_vat_adj_jointworking >= 500000, "≥£500,000", "other")))))))


trust_data$donations_cat = ifelse(trust_data$infl_vat_adj_donations == 0, "£0",
                                ifelse(trust_data$infl_vat_adj_donations > 0  & trust_data$infl_vat_adj_donations < 100, "<£100",
                                       ifelse(trust_data$infl_vat_adj_donations >= 100 & trust_data$infl_vat_adj_donations < 1000, "£100 to £1,000",
                                              ifelse(trust_data$infl_vat_adj_donations >= 1000 & trust_data$infl_vat_adj_donations < 10000, "£1000 to £10,000",
                                                     ifelse(trust_data$infl_vat_adj_donations >= 10000 & trust_data$infl_vat_adj_donations < 100000, "£10,000 to £100,000",
                                                            ifelse(trust_data$infl_vat_adj_donations >= 100000 & trust_data$infl_vat_adj_donations < 500000, "£100,000 to £500,000",
                                                                   ifelse(trust_data$infl_vat_adj_donations >= 500000, "≥£500,000", "other")))))))


trust_data$events_cat = ifelse(trust_data$infl_vat_adj_costsofevents == 0, "£0",
                             ifelse(trust_data$infl_vat_adj_costsofevents > 0  & trust_data$infl_vat_adj_costsofevents < 100, "<£100",
                                    ifelse(trust_data$infl_vat_adj_costsofevents >= 100 & trust_data$infl_vat_adj_costsofevents < 1000, "£100 to £1,000",
                                           ifelse(trust_data$infl_vat_adj_costsofevents >= 1000 & trust_data$infl_vat_adj_costsofevents < 10000, "£1000 to £10,000",
                                                  ifelse(trust_data$infl_vat_adj_costsofevents >= 10000 & trust_data$infl_vat_adj_costsofevents < 100000, "£10,000 to £100,000",
                                                         ifelse(trust_data$infl_vat_adj_costsofevents >= 100000 & trust_data$infl_vat_adj_costsofevents < 500000, "£100,000 to £500,000",
                                                                ifelse(trust_data$infl_vat_adj_costsofevents >= 500000, "≥£500,000", "other")))))))


trust_data$service_cat = ifelse(trust_data$infl_vat_adj_service == 0, "£0",
                              ifelse(trust_data$infl_vat_adj_service > 0  &  trust_data$infl_vat_adj_service < 100, "<£100",
                                     ifelse(trust_data$infl_vat_adj_service >= 100 & trust_data$infl_vat_adj_service < 1000, "£100 to £1,000",
                                            ifelse(trust_data$infl_vat_adj_service >= 1000 & trust_data$infl_vat_adj_service < 10000, "£1000 to £10,000",
                                                   ifelse(trust_data$infl_vat_adj_service >= 10000 & trust_data$infl_vat_adj_service < 100000, "£10,000 to £100,000",
                                                          ifelse(trust_data$infl_vat_adj_service >= 100000 & trust_data$infl_vat_adj_service < 500000, "£100,000 to £500,000",
                                                                 ifelse(trust_data$infl_vat_adj_service >= 500000, "≥£500,000", "other")))))))



trust_data$final_tov_cat = ifelse(trust_data$final_tov == 0, "£0",
                                ifelse(trust_data$final_tov > 0  &  trust_data$final_tov < 100, "<£100",
                                       ifelse(trust_data$final_tov >= 100 & trust_data$final_tov < 1000, "£100 to £1,000",
                                              ifelse(trust_data$final_tov >= 1000 & trust_data$final_tov < 10000, "£1000 to £10,000",
                                                     ifelse(trust_data$final_tov >= 10000 & trust_data$final_tov < 100000, "£10,000 to £100,000",
                                                            ifelse(trust_data$final_tov >= 100000 & trust_data$final_tov < 500000, "£100,000 to £500,000",
                                                                   ifelse(trust_data$final_tov >= 500000, "≥£500,000", "other")))))))








############## TABLE FOR NUMBER OF  PAYMENT CATEGORIES DONATIONS


table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("£0","<£100","£100 to £1,000","£1000 to £10,000","£10,000 to £100,000","£100,000 to £500,000","≥£500,000")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(donations_cat %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(donations_cat %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(donations_cat %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(donations_cat %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(8:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(donations_cat %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- nrow(seged_2)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(8:20),]

table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015perc = as.numeric(table_ptype$X2015perc)
table_ptype$X2016perc =  as.numeric(table_ptype$X2016perc)
table_ptype$X2017perc =  as.numeric(table_ptype$X2017perc)
table_ptype$X2018perc = as.numeric(table_ptype$X2018perc)
table_ptype$Total_perc = as.numeric(table_ptype$Total_perc)

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)

table_ptype$X2015 = prettyNum(table_ptype$X2015, big.mark = ",", scientific = FALSE)
table_ptype$X2016 =  prettyNum(table_ptype$X2016, big.mark = ",", scientific = FALSE)
table_ptype$X2017 =  prettyNum(table_ptype$X2017, big.mark = ",", scientific = FALSE)
table_ptype$X2018 = prettyNum(table_ptype$X2018, big.mark = ",", scientific = FALSE)
table_ptype$Total = prettyNum(table_ptype$Total, big.mark = ",", scientific = FALSE)



table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


#####################£ SUM VALUE BY PAYMENT CATEGORY DONATIONS


table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("£0","<£100","£100 to £1,000","£1000 to £10,000","£10,000 to £100,000","£100,000 to £500,000","≥£500,000")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(donations_cat %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_donations)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(donations_cat %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_donations)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(9:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(donations_cat %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_donations)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(donations_cat %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_donations)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(8:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(donations_cat %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_donations)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(8:20),]

table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015 = round(table_ptype$X2015, digits = 2)
table_ptype$X2016 =  round(table_ptype$X2016, digits = 2)
table_ptype$X2017 =  round(table_ptype$X2017, digits = 2)
table_ptype$X2018 = round(table_ptype$X2018, digits = 2)
table_ptype$Total = round(table_ptype$Total, digits = 2)

table_ptype$X2015 = paste('£',formatC(table_ptype$X2015, big.mark=',', format = 'f', digits = 2))
table_ptype$X2016 =  paste('£',formatC(table_ptype$X2016, big.mark=',', format = 'f', digits = 2))
table_ptype$X2017 =  paste('£',formatC(table_ptype$X2017, big.mark=',', format = 'f', digits = 2))
table_ptype$X2018 = paste('£',formatC(table_ptype$X2018, big.mark=',', format = 'f', digits = 2))
table_ptype$Total = paste('£',formatC(table_ptype$Total, big.mark=',', format = 'f', digits = 2))

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)



table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))




table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

sum(trust_2015$infl_vat_adj_donations)
sum(trust_2016$infl_vat_adj_donations)
sum(trust_2017$infl_vat_adj_donations)
sum(trust_2018$infl_vat_adj_donations)
sum(trust_data$infl_vat_adj_donations)



############## TABLE FOR NUMBER OF  PAYMENT CATEGORIES


table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("£0","<£100","£100 to £1,000","£1000 to £10,000","£10,000 to £100,000","£100,000 to £500,000","≥£500,000")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(jointwork_cat %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(jointwork_cat %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(jointwork_cat %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(jointwork_cat %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(8:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(jointwork_cat %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- nrow(seged_2)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(8:20),]

table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)

table_ptype$X2015 = prettyNum(table_ptype$X2015, big.mark = ",", scientific = FALSE)
table_ptype$X2016 =  prettyNum(table_ptype$X2016, big.mark = ",", scientific = FALSE)
table_ptype$X2017 =  prettyNum(table_ptype$X2017, big.mark = ",", scientific = FALSE)
table_ptype$X2018 = prettyNum(table_ptype$X2018, big.mark = ",", scientific = FALSE)
table_ptype$Total = prettyNum(table_ptype$Total, big.mark = ",", scientific = FALSE)

table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


#####################£ SUM VALUE BY PAYMENT CATEGORY JOINTWORK


table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("£0","<£100","£100 to £1,000","£1000 to £10,000","£10,000 to £100,000","£100,000 to £500,000","≥£500,000")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(jointwork_cat %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_jointworking)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(jointwork_cat %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_jointworking)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(9:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(jointwork_cat %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_jointworking)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(jointwork_cat %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_jointworking)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(8:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(jointwork_cat %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_jointworking)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(8:20),]

table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015 = round(table_ptype$X2015, digits = 2)
table_ptype$X2016 =  round(table_ptype$X2016, digits = 2)
table_ptype$X2017 =  round(table_ptype$X2017, digits = 2)
table_ptype$X2018 = round(table_ptype$X2018, digits = 2)
table_ptype$Total = round(table_ptype$Total, digits = 2)

table_ptype$X2015 = paste('£',formatC(table_ptype$X2015, big.mark=',', format = 'f', digits = 2))
table_ptype$X2016 =  paste('£',formatC(table_ptype$X2016, big.mark=',', format = 'f', digits = 2))
table_ptype$X2017 =  paste('£',formatC(table_ptype$X2017, big.mark=',', format = 'f', digits = 2))
table_ptype$X2018 = paste('£',formatC(table_ptype$X2018, big.mark=',', format = 'f', digits = 2))
table_ptype$Total = paste('£',formatC(table_ptype$Total, big.mark=',', format = 'f', digits = 2))

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)




table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))




table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

sum(trust_2015$infl_vat_adj_jointworking)
sum(trust_2016$infl_vat_adj_jointworking)
sum(trust_2017$infl_vat_adj_jointworking)
sum(trust_2018$infl_vat_adj_jointworking)
sum(trust_data$infl_vat_adj_jointworking)



############## TABLE FOR NUMBER OF  PAYMENT CATEGORIES EVENTS


table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("£0","<£100","£100 to £1,000","£1000 to £10,000","£10,000 to £100,000","£100,000 to £500,000","≥£500,000")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(events_cat %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(events_cat %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(events_cat %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(events_cat %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(8:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(events_cat %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- nrow(seged_2)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(8:20),]

table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)

table_ptype$X2015 = prettyNum(table_ptype$X2015, big.mark = ",", scientific = FALSE)
table_ptype$X2016 =  prettyNum(table_ptype$X2016, big.mark = ",", scientific = FALSE)
table_ptype$X2017 =  prettyNum(table_ptype$X2017, big.mark = ",", scientific = FALSE)
table_ptype$X2018 = prettyNum(table_ptype$X2018, big.mark = ",", scientific = FALSE)
table_ptype$Total = prettyNum(table_ptype$Total, big.mark = ",", scientific = FALSE)


table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


#####################£ SUM VALUE BY PAYMENT CATEGORY EVENTS


table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("£0","<£100","£100 to £1,000","£1000 to £10,000","£10,000 to £100,000","£100,000 to £500,000","≥£500,000")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(events_cat %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_costsofevents)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(events_cat %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_costsofevents)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(9:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(events_cat %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_costsofevents)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(events_cat %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_costsofevents)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(8:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(events_cat %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_costsofevents)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(8:20),]

table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015 = round(table_ptype$X2015, digits = 2)
table_ptype$X2016 =  round(table_ptype$X2016, digits = 2)
table_ptype$X2017 =  round(table_ptype$X2017, digits = 2)
table_ptype$X2018 = round(table_ptype$X2018, digits = 2)
table_ptype$Total = round(table_ptype$Total, digits = 2)

table_ptype$X2015 = paste('£',formatC(table_ptype$X2015, big.mark=',', format = 'f', digits = 2))
table_ptype$X2016 =  paste('£',formatC(table_ptype$X2016, big.mark=',', format = 'f', digits = 2))
table_ptype$X2017 =  paste('£',formatC(table_ptype$X2017, big.mark=',', format = 'f', digits = 2))
table_ptype$X2018 = paste('£',formatC(table_ptype$X2018, big.mark=',', format = 'f', digits = 2))
table_ptype$Total = paste('£',formatC(table_ptype$Total, big.mark=',', format = 'f', digits = 2))

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)




table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))




table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

sum(trust_2015$infl_vat_adj_costsofevents)
sum(trust_2016$infl_vat_adj_costsofevents)
sum(trust_2017$infl_vat_adj_costsofevents)
sum(trust_2018$infl_vat_adj_costsofevents)
sum(trust_data$infl_vat_adj_costsofevents)




############## TABLE FOR NUMBER OF  PAYMENT CATEGORIES SERVICE


table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("£0","<£100","£100 to £1,000","£1000 to £10,000","£10,000 to £100,000","£100,000 to £500,000","≥£500,000")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(service_cat %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(service_cat %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(service_cat %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(service_cat %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(8:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(service_cat %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- nrow(seged_2)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(8:20),]

table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)

table_ptype$X2015 = prettyNum(table_ptype$X2015, big.mark = ",", scientific = FALSE)
table_ptype$X2016 =  prettyNum(table_ptype$X2016, big.mark = ",", scientific = FALSE)
table_ptype$X2017 =  prettyNum(table_ptype$X2017, big.mark = ",", scientific = FALSE)
table_ptype$X2018 = prettyNum(table_ptype$X2018, big.mark = ",", scientific = FALSE)
table_ptype$Total = prettyNum(table_ptype$Total, big.mark = ",", scientific = FALSE)

table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


#####################£ SUM VALUE BY PAYMENT CATEGORY EVENTS


table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("£0","<£100","£100 to £1,000","£1000 to £10,000","£10,000 to £100,000","£100,000 to £500,000","≥£500,000")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(service_cat %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_service)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(service_cat %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_service)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(9:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(service_cat %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_service)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(service_cat %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_service)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(8:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(service_cat %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_service)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(8:20),]

table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015 = round(table_ptype$X2015, digits = 2)
table_ptype$X2016 =  round(table_ptype$X2016, digits = 2)
table_ptype$X2017 =  round(table_ptype$X2017, digits = 2)
table_ptype$X2018 = round(table_ptype$X2018, digits = 2)
table_ptype$Total = round(table_ptype$Total, digits = 2)

table_ptype$X2015 = paste('£',formatC(table_ptype$X2015, big.mark=',', format = 'f', digits = 2))
table_ptype$X2016 =  paste('£',formatC(table_ptype$X2016, big.mark=',', format = 'f', digits = 2))
table_ptype$X2017 =  paste('£',formatC(table_ptype$X2017, big.mark=',', format = 'f', digits = 2))
table_ptype$X2018 = paste('£',formatC(table_ptype$X2018, big.mark=',', format = 'f', digits = 2))
table_ptype$Total = paste('£',formatC(table_ptype$Total, big.mark=',', format = 'f', digits = 2))

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)




table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))




table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

sum(trust_2015$infl_vat_adj_service)
sum(trust_2016$infl_vat_adj_service)
sum(trust_2017$infl_vat_adj_service)
sum(trust_2018$infl_vat_adj_service)
sum(trust_data$infl_vat_adj_service)






############## TRUST TYPE COMPARISON ############## ############## ############## ############## ############## 
############## ############## ############## ############## ############## ############## ############## ############## 


trust_acute <- trust_data[ which(trust_data$trust_type=='1'),]
trust_mental <- trust_data[ which(trust_data$trust_type=='2'),]
trust_ambulam <- trust_data[ which(trust_data$trust_type=='3'),]
trust_community <- trust_data[ which(trust_data$trust_type=='4'),]
trust_integrated <- trust_data[ which(trust_data$trust_type=='5'),]



##### Acute trusts

table_hco<-data.frame(Year= character(0) , Trust_number = character(0), Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

b<-c("2015","2016","2017","2018")

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_acute%>%filter(year %in% b[i])
  table_hco$Trust_number[table_hco$Year==b[i]]<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_acute%>%filter(year %in% b[i])
  table_hco$Value_p[table_hco$Year==b[i]]<-sqldf("select sum(final_tov) from seged_2")
  rm(seged_2)
  
}
for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_acute%>%filter(year %in% b[i])
  table_hco$Number_p[table_hco$Year==b[i]]<-sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(5:13),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_acute%>%filter(year %in% b[i])
  table_hco$Mean[table_hco$Year==b[i]]<-sqldf("select avg(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:9),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_acute%>%filter(year %in% b[i])
  table_hco$St_dev[table_hco$Year==b[i]]<-sqldf("select STDEV(final_tov) from seged_2")
  rm(seged_2)
  
}

  

table_hco = table_hco[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_acute%>%filter(year %in% b[i])
  table_hco$Median[table_hco$Year==b[i]]<-sqldf("select median(final_tov) from seged_2")
  rm(seged_2)
  
}


table_hco = table_hco[-(5:8),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_acute%>%filter(year %in% b[i])
  table_hco$Q1[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_acute%>%filter(year %in% b[i])
  table_hco$Q3[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:12),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)


table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


JonckheereTerpstraTest(trust_acute$final_tov, trust_acute$year, nperm = 1000, alternative = "decreasing")

pwt_hco = pairwise.wilcox.test(trust_acute$final_tov, trust_acute$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_hco = left_join(table_hco, pwt_hco, by = "Year")

summary(trust_acute$final_tov)
sum(trust_acute$final_tov)
sd(trust_acute$final_tov)


#### Totals


table_hco<-data.frame(Trust_number = character(0), Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_acute
  table_hco$Trust_number<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_acute
  table_hco$Value_p<- sum(seged_2$final_tov) 
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_acute
  table_hco$Number_p<-length(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_acute
  table_hco$Mean<-mean(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_acute
  table_hco$St_dev<-sd(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_acute
  table_hco$Median<-median(seged_2$final_tov)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]





for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_acute
  table_hco$Q1<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_acute
  table_hco$Q3<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)

table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))




##### Integrated trusts

table_hco<-data.frame(Year= character(0) , Trust_number = character(0), Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

b<-c("2015","2016","2017","2018")

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_integrated%>%filter(year %in% b[i])
  table_hco$Trust_number[table_hco$Year==b[i]]<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_integrated%>%filter(year %in% b[i])
  table_hco$Value_p[table_hco$Year==b[i]]<-sqldf("select sum(final_tov) from seged_2")
  rm(seged_2)
  
}
for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_integrated%>%filter(year %in% b[i])
  table_hco$Number_p[table_hco$Year==b[i]]<-sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(5:13),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_integrated%>%filter(year %in% b[i])
  table_hco$Mean[table_hco$Year==b[i]]<-sqldf("select avg(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:9),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_integrated%>%filter(year %in% b[i])
  table_hco$St_dev[table_hco$Year==b[i]]<-sqldf("select STDEV(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_integrated%>%filter(year %in% b[i])
  table_hco$Median[table_hco$Year==b[i]]<-sqldf("select median(final_tov) from seged_2")
  rm(seged_2)
  
}


table_hco = table_hco[-(5:8),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_integrated%>%filter(year %in% b[i])
  table_hco$Q1[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_integrated%>%filter(year %in% b[i])
  table_hco$Q3[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:12),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)


table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


JonckheereTerpstraTest(trust_integrated$final_tov, trust_integrated$year, nperm = 1000, alternative = "decreasing")

pwt_hco = pairwise.wilcox.test(trust_integrated$final_tov, trust_integrated$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_hco = left_join(table_hco, pwt_hco, by = "Year")



#### Totals


table_hco<-data.frame(Trust_number = character(0), Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_integrated
  table_hco$Trust_number<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_integrated
  table_hco$Value_p<- sum(seged_2$final_tov) 
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_integrated
  table_hco$Number_p<-length(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_integrated
  table_hco$Mean<-mean(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_integrated
  table_hco$St_dev<-sd(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_integrated
  table_hco$Median<-median(seged_2$final_tov)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]





for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_integrated
  table_hco$Q1<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_integrated
  table_hco$Q3<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)

table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))






##### Community trusts

table_hco<-data.frame(Year= character(0) , Trust_number = character(0), Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

b<-c("2015","2016","2017","2018")

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_community%>%filter(year %in% b[i])
  table_hco$Trust_number[table_hco$Year==b[i]]<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_community%>%filter(year %in% b[i])
  table_hco$Value_p[table_hco$Year==b[i]]<-sqldf("select sum(final_tov) from seged_2")
  rm(seged_2)
  
}
for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_community%>%filter(year %in% b[i])
  table_hco$Number_p[table_hco$Year==b[i]]<-sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(5:13),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_community%>%filter(year %in% b[i])
  table_hco$Mean[table_hco$Year==b[i]]<-sqldf("select avg(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:9),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_community%>%filter(year %in% b[i])
  table_hco$St_dev[table_hco$Year==b[i]]<-sqldf("select STDEV(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_community%>%filter(year %in% b[i])
  table_hco$Median[table_hco$Year==b[i]]<-sqldf("select median(final_tov) from seged_2")
  rm(seged_2)
  
}


table_hco = table_hco[-(5:8),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_community%>%filter(year %in% b[i])
  table_hco$Q1[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_community%>%filter(year %in% b[i])
  table_hco$Q3[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:12),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)

table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


JonckheereTerpstraTest(trust_community$final_tov, trust_community$year, nperm = 1000, alternative = "decreasing")

pwt_hco = pairwise.wilcox.test(trust_community$final_tov, trust_community$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_hco = left_join(table_hco, pwt_hco, by = "Year")



#### Totals


table_hco<-data.frame(Trust_number = character(0), Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_community
  table_hco$Trust_number<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_community
  table_hco$Value_p<- sum(seged_2$final_tov) 
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_community
  table_hco$Number_p<-length(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_community
  table_hco$Mean<-mean(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_community
  table_hco$St_dev<-sd(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_community
  table_hco$Median<-median(seged_2$final_tov)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]





for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_community
  table_hco$Q1<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_community
  table_hco$Q3<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)

table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))






##### Mental health trusts

table_hco<-data.frame(Year= character(0) , Trust_number = character(0), Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

b<-c("2015","2016","2017","2018")

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_mental%>%filter(year %in% b[i])
  table_hco$Trust_number[table_hco$Year==b[i]]<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_mental%>%filter(year %in% b[i])
  table_hco$Value_p[table_hco$Year==b[i]]<-sqldf("select sum(final_tov) from seged_2")
  rm(seged_2)
  
}
for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_mental%>%filter(year %in% b[i])
  table_hco$Number_p[table_hco$Year==b[i]]<-sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(5:13),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_mental%>%filter(year %in% b[i])
  table_hco$Mean[table_hco$Year==b[i]]<-sqldf("select avg(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:9),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_mental%>%filter(year %in% b[i])
  table_hco$St_dev[table_hco$Year==b[i]]<-sqldf("select STDEV(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_mental%>%filter(year %in% b[i])
  table_hco$Median[table_hco$Year==b[i]]<-sqldf("select median(final_tov) from seged_2")
  rm(seged_2)
  
}


table_hco = table_hco[-(5:8),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_mental%>%filter(year %in% b[i])
  table_hco$Q1[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_mental%>%filter(year %in% b[i])
  table_hco$Q3[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:12),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)

table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


JonckheereTerpstraTest(trust_mental$final_tov, trust_mental$year, nperm = 1000, alternative = "decreasing")

pwt_hco = pairwise.wilcox.test(trust_mental$final_tov, trust_mental$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_hco = left_join(table_hco, pwt_hco, by = "Year")



#### Totals


table_hco<-data.frame(Trust_number = character(0), Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_mental
  table_hco$Trust_number<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_mental
  table_hco$Value_p<- sum(seged_2$final_tov) 
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_mental
  table_hco$Number_p<-length(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_mental
  table_hco$Mean<-mean(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_mental
  table_hco$St_dev<-sd(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_mental
  table_hco$Median<-median(seged_2$final_tov)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]





for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_mental
  table_hco$Q1<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_mental
  table_hco$Q3<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)

table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))






##### Ambulam trusts

table_hco<-data.frame(Year= character(0) , Trust_number = character(0), Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

b<-c("2015","2016","2017","2018")

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_ambulam%>%filter(year %in% b[i])
  table_hco$Trust_number[table_hco$Year==b[i]]<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_ambulam%>%filter(year %in% b[i])
  table_hco$Value_p[table_hco$Year==b[i]]<-sqldf("select sum(final_tov) from seged_2")
  rm(seged_2)
  
}
for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_ambulam%>%filter(year %in% b[i])
  table_hco$Number_p[table_hco$Year==b[i]]<-sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(5:13),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_ambulam%>%filter(year %in% b[i])
  table_hco$Mean[table_hco$Year==b[i]]<-sqldf("select avg(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:9),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_ambulam%>%filter(year %in% b[i])
  table_hco$St_dev[table_hco$Year==b[i]]<-sqldf("select STDEV(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_ambulam%>%filter(year %in% b[i])
  table_hco$Median[table_hco$Year==b[i]]<-sqldf("select median(final_tov) from seged_2")
  rm(seged_2)
  
}


table_hco = table_hco[-(5:8),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_ambulam%>%filter(year %in% b[i])
  table_hco$Q1[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_ambulam%>%filter(year %in% b[i])
  table_hco$Q3[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:12),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)

table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


JonckheereTerpstraTest(trust_ambulam$final_tov, trust_ambulam$year, nperm = 1000, alternative = "decreasing")

pwt_hco = pairwise.wilcox.test(trust_ambulam$final_tov, trust_ambulam$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_hco = left_join(table_hco, pwt_hco, by = "Year")



#### Totals


table_hco<-data.frame(Trust_number = character(0), Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_ambulam
  table_hco$Trust_number<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_ambulam
  table_hco$Value_p<- sum(seged_2$final_tov) 
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_ambulam
  table_hco$Number_p<-length(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_ambulam
  table_hco$Mean<-mean(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_ambulam
  table_hco$St_dev<-sd(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_ambulam
  table_hco$Median<-median(seged_2$final_tov)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]





for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_ambulam
  table_hco$Q1<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_ambulam
  table_hco$Q3<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)

table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))







##################################################################################################################
##################################################################################################################
################### TRUST TYPE BY YEAR



###### 2015


table_ptype<-data.frame(Trust_type = character(0), Trust_number = character(0), Number_p = character(0) , Value_p = character(0) ,  Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))


b<-c("1","5","4","2", "3")




for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(trust_type %in% b[i])
  table_ptype$Trust_number[table_ptype$Trust_type==b[i]]<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_ptype = unique(table_ptype)

for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(trust_type %in% b[i])
  table_ptype$Number_p[table_ptype$Trust_type==b[i]]<- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype = table_ptype[-(6:10),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(trust_type %in% b[i])
  table_ptype$Value_p[table_ptype$Trust_type==b[i]]<-sum(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(trust_2015$final_tov, by=list(Category=trust_2015$trust_type), FUN=sum)  

table_ptype = table_ptype[-(6:18),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(trust_type %in% b[i])
  table_ptype$Mean[table_ptype$Trust_type==b[i]]<-mean(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=mean)  

table_ptype = table_ptype[-(6:18),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(trust_type %in% b[i])
  table_ptype$St_dev[table_ptype$Trust_type==b[i]]<-sd(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_ptype = table_ptype[-(6:18),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(trust_type %in% b[i])
  table_ptype$Median[table_ptype$Trust_type==b[i]]<- median(seged_2$final_tov)
  rm(seged_2)
  
}



aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_ptype = table_ptype[-(6:19),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(trust_type %in% b[i])
  table_ptype$Q1[table_ptype$Trust_type==b[i]]<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(6:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(trust_type %in% b[i])
  table_ptype$Q3[table_ptype$Trust_type==b[i]]<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(6:12),]





options(digits=10)



table_ptype$Value_p = as.numeric(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = as.numeric(table_ptype$Number_p, digits = 2)
table_ptype$Mean = as.numeric(table_ptype$Mean, digits = 2)
table_ptype$St_dev = as.numeric(table_ptype$St_dev, digits = 2)
table_ptype$Median = as.numeric(table_ptype$Median, digits = 2)
table_ptype$Q1 = as.numeric(table_ptype$Q1, digits = 2)
table_ptype$Q3 = as.numeric(table_ptype$Q3, digits = 2)


table_ptype$Value_p = round(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = round(table_ptype$Number_p, digits = 2)
table_ptype$Mean = round(table_ptype$Mean, digits = 2)
table_ptype$St_dev = round(table_ptype$St_dev, digits = 2)
table_ptype$Median = round(table_ptype$Median, digits = 2)
table_ptype$Q1 = round(table_ptype$Q1, digits = 2)
table_ptype$Q3 = round(table_ptype$Q3, digits = 2)



table_ptype$median_iqr <- paste(table_ptype$Median, "(",   table_ptype$Q1, "-", table_ptype$Q3, ")")

table_ptype = select(table_ptype, -c("Median", "Q1", "Q3"))


pwt_hco = pairwise.wilcox.test(trust_2015$final_tov, trust_2015$trust_type, p.adjust.method = "bonf")

pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Trust_type <- row.names(pwt_hco) 


table_ptype = left_join(table_ptype, pwt_hco, by = "Trust_type")

table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))





########## 2016



table_ptype<-data.frame(Trust_type = character(0), Trust_number = character(0), Number_p = character(0) , Value_p = character(0) ,  Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))


b<-c("1","5","4","2", "3")




for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(trust_type %in% b[i])
  table_ptype$Trust_number[table_ptype$Trust_type==b[i]]<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_ptype = unique(table_ptype)

for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(trust_type %in% b[i])
  table_ptype$Number_p[table_ptype$Trust_type==b[i]]<- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype = table_ptype[-(6:10),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(trust_type %in% b[i])
  table_ptype$Value_p[table_ptype$Trust_type==b[i]]<-sum(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(trust_2016$final_tov, by=list(Category=trust_2016$trust_type), FUN=sum)  

table_ptype = table_ptype[-(6:18),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(trust_type %in% b[i])
  table_ptype$Mean[table_ptype$Trust_type==b[i]]<-mean(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(trust_2016$final_tov, by=list(Category=trust_2016$year), FUN=mean)  

table_ptype = table_ptype[-(6:18),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(trust_type %in% b[i])
  table_ptype$St_dev[table_ptype$Trust_type==b[i]]<-sd(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_ptype = table_ptype[-(6:18),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(trust_type %in% b[i])
  table_ptype$Median[table_ptype$Trust_type==b[i]]<- median(seged_2$final_tov)
  rm(seged_2)
  
}



aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_ptype = table_ptype[-(6:19),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(trust_type %in% b[i])
  table_ptype$Q1[table_ptype$Trust_type==b[i]]<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(6:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(trust_type %in% b[i])
  table_ptype$Q3[table_ptype$Trust_type==b[i]]<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(6:12),]





options(digits=10)



table_ptype$Value_p = as.numeric(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = as.numeric(table_ptype$Number_p, digits = 2)
table_ptype$Mean = as.numeric(table_ptype$Mean, digits = 2)
table_ptype$St_dev = as.numeric(table_ptype$St_dev, digits = 2)
table_ptype$Median = as.numeric(table_ptype$Median, digits = 2)
table_ptype$Q1 = as.numeric(table_ptype$Q1, digits = 2)
table_ptype$Q3 = as.numeric(table_ptype$Q3, digits = 2)


table_ptype$Value_p = round(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = round(table_ptype$Number_p, digits = 2)
table_ptype$Mean = round(table_ptype$Mean, digits = 2)
table_ptype$St_dev = round(table_ptype$St_dev, digits = 2)
table_ptype$Median = round(table_ptype$Median, digits = 2)
table_ptype$Q1 = round(table_ptype$Q1, digits = 2)
table_ptype$Q3 = round(table_ptype$Q3, digits = 2)



table_ptype$median_iqr <- paste(table_ptype$Median, "(",   table_ptype$Q1, "-", table_ptype$Q3, ")")

table_ptype = select(table_ptype, -c("Median", "Q1", "Q3"))


pwt_hco = pairwise.wilcox.test(trust_2016$final_tov, trust_2016$trust_type, p.adjust.method = "bonf")

pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Trust_type <- row.names(pwt_hco) 


table_ptype = left_join(table_ptype, pwt_hco, by = "Trust_type")

table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))






########## 2017



table_ptype<-data.frame(Trust_type = character(0), Trust_number = character(0), Number_p = character(0) , Value_p = character(0) ,  Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))


b<-c("1","5","4","2", "3")




for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(trust_type %in% b[i])
  table_ptype$Trust_number[table_ptype$Trust_type==b[i]]<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_ptype = unique(table_ptype)

for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(trust_type %in% b[i])
  table_ptype$Number_p[table_ptype$Trust_type==b[i]]<- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype = table_ptype[-(6:10),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(trust_type %in% b[i])
  table_ptype$Value_p[table_ptype$Trust_type==b[i]]<-sum(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(trust_2016$final_tov, by=list(Category=trust_2016$trust_type), FUN=sum)  

table_ptype = table_ptype[-(6:18),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(trust_type %in% b[i])
  table_ptype$Mean[table_ptype$Trust_type==b[i]]<-mean(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(trust_2016$final_tov, by=list(Category=trust_2016$year), FUN=mean)  

table_ptype = table_ptype[-(6:18),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(trust_type %in% b[i])
  table_ptype$St_dev[table_ptype$Trust_type==b[i]]<-sd(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_ptype = table_ptype[-(6:18),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(trust_type %in% b[i])
  table_ptype$Median[table_ptype$Trust_type==b[i]]<- median(seged_2$final_tov)
  rm(seged_2)
  
}



aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_ptype = table_ptype[-(6:19),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(trust_type %in% b[i])
  table_ptype$Q1[table_ptype$Trust_type==b[i]]<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(6:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(trust_type %in% b[i])
  table_ptype$Q3[table_ptype$Trust_type==b[i]]<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(6:12),]





options(digits=10)



table_ptype$Value_p = as.numeric(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = as.numeric(table_ptype$Number_p, digits = 2)
table_ptype$Mean = as.numeric(table_ptype$Mean, digits = 2)
table_ptype$St_dev = as.numeric(table_ptype$St_dev, digits = 2)
table_ptype$Median = as.numeric(table_ptype$Median, digits = 2)
table_ptype$Q1 = as.numeric(table_ptype$Q1, digits = 2)
table_ptype$Q3 = as.numeric(table_ptype$Q3, digits = 2)


table_ptype$Value_p = round(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = round(table_ptype$Number_p, digits = 2)
table_ptype$Mean = round(table_ptype$Mean, digits = 2)
table_ptype$St_dev = round(table_ptype$St_dev, digits = 2)
table_ptype$Median = round(table_ptype$Median, digits = 2)
table_ptype$Q1 = round(table_ptype$Q1, digits = 2)
table_ptype$Q3 = round(table_ptype$Q3, digits = 2)



table_ptype$median_iqr <- paste(table_ptype$Median, "(",   table_ptype$Q1, "-", table_ptype$Q3, ")")

table_ptype = select(table_ptype, -c("Median", "Q1", "Q3"))


pwt_hco = pairwise.wilcox.test(trust_2017$final_tov, trust_2017$trust_type, p.adjust.method = "bonf")

pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Trust_type <- row.names(pwt_hco) 


table_ptype = left_join(table_ptype, pwt_hco, by = "Trust_type")

table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))







########## 2018



table_ptype<-data.frame(Trust_type = character(0), Trust_number = character(0), Number_p = character(0) , Value_p = character(0) ,  Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))


b<-c("1","5","4","2", "3")




for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(trust_type %in% b[i])
  table_ptype$Trust_number[table_ptype$Trust_type==b[i]]<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_ptype = unique(table_ptype)

for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(trust_type %in% b[i])
  table_ptype$Number_p[table_ptype$Trust_type==b[i]]<- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype = table_ptype[-(6:10),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(trust_type %in% b[i])
  table_ptype$Value_p[table_ptype$Trust_type==b[i]]<-sum(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(trust_2016$final_tov, by=list(Category=trust_2016$trust_type), FUN=sum)  

table_ptype = table_ptype[-(6:18),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(trust_type %in% b[i])
  table_ptype$Mean[table_ptype$Trust_type==b[i]]<-mean(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(trust_2016$final_tov, by=list(Category=trust_2016$year), FUN=mean)  

table_ptype = table_ptype[-(6:18),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(trust_type %in% b[i])
  table_ptype$St_dev[table_ptype$Trust_type==b[i]]<-sd(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_ptype = table_ptype[-(6:18),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(trust_type %in% b[i])
  table_ptype$Median[table_ptype$Trust_type==b[i]]<- median(seged_2$final_tov)
  rm(seged_2)
  
}



aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_ptype = table_ptype[-(6:19),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(trust_type %in% b[i])
  table_ptype$Q1[table_ptype$Trust_type==b[i]]<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(6:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(trust_type %in% b[i])
  table_ptype$Q3[table_ptype$Trust_type==b[i]]<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(6:12),]





options(digits=10)



table_ptype$Value_p = as.numeric(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = as.numeric(table_ptype$Number_p, digits = 2)
table_ptype$Mean = as.numeric(table_ptype$Mean, digits = 2)
table_ptype$St_dev = as.numeric(table_ptype$St_dev, digits = 2)
table_ptype$Median = as.numeric(table_ptype$Median, digits = 2)
table_ptype$Q1 = as.numeric(table_ptype$Q1, digits = 2)
table_ptype$Q3 = as.numeric(table_ptype$Q3, digits = 2)


table_ptype$Value_p = round(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = round(table_ptype$Number_p, digits = 2)
table_ptype$Mean = round(table_ptype$Mean, digits = 2)
table_ptype$St_dev = round(table_ptype$St_dev, digits = 2)
table_ptype$Median = round(table_ptype$Median, digits = 2)
table_ptype$Q1 = round(table_ptype$Q1, digits = 2)
table_ptype$Q3 = round(table_ptype$Q3, digits = 2)



table_ptype$median_iqr <- paste(table_ptype$Median, "(",   table_ptype$Q1, "-", table_ptype$Q3, ")")

table_ptype = select(table_ptype, -c("Median", "Q1", "Q3"))


pwt_hco = pairwise.wilcox.test(trust_2018$final_tov, trust_2018$trust_type, p.adjust.method = "bonf")

pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Trust_type <- row.names(pwt_hco) 


table_ptype = left_join(table_ptype, pwt_hco, by = "Trust_type")

table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))






########## All years



table_ptype<-data.frame(Trust_type = character(0), Trust_number = character(0), Number_p = character(0) , Value_p = character(0) ,  Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))


b<-c("1","5","4","2", "3")




for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(trust_type %in% b[i])
  table_ptype$Trust_number[table_ptype$Trust_type==b[i]]<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_ptype = unique(table_ptype)

for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(trust_type %in% b[i])
  table_ptype$Number_p[table_ptype$Trust_type==b[i]]<- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype = table_ptype[-(6:10),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(trust_type %in% b[i])
  table_ptype$Value_p[table_ptype$Trust_type==b[i]]<-sum(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(trust_2016$final_tov, by=list(Category=trust_2016$trust_type), FUN=sum)  

table_ptype = table_ptype[-(6:18),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(trust_type %in% b[i])
  table_ptype$Mean[table_ptype$Trust_type==b[i]]<-mean(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(trust_2016$final_tov, by=list(Category=trust_2016$year), FUN=mean)  

table_ptype = table_ptype[-(6:18),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(trust_type %in% b[i])
  table_ptype$St_dev[table_ptype$Trust_type==b[i]]<-sd(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_ptype = table_ptype[-(6:18),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(trust_type %in% b[i])
  table_ptype$Median[table_ptype$Trust_type==b[i]]<- median(seged_2$final_tov)
  rm(seged_2)
  
}



aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_ptype = table_ptype[-(6:19),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(trust_type %in% b[i])
  table_ptype$Q1[table_ptype$Trust_type==b[i]]<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(6:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Trust_type=b[i], Trust_number =0, Number_p=0, Value_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(trust_type %in% b[i])
  table_ptype$Q3[table_ptype$Trust_type==b[i]]<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_ptype = table_ptype[-(6:12),]





options(digits=10)



table_ptype$Value_p = as.numeric(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = as.numeric(table_ptype$Number_p, digits = 2)
table_ptype$Mean = as.numeric(table_ptype$Mean, digits = 2)
table_ptype$St_dev = as.numeric(table_ptype$St_dev, digits = 2)
table_ptype$Median = as.numeric(table_ptype$Median, digits = 2)
table_ptype$Q1 = as.numeric(table_ptype$Q1, digits = 2)
table_ptype$Q3 = as.numeric(table_ptype$Q3, digits = 2)


table_ptype$Value_p = round(table_ptype$Value_p, digits = 2)
table_ptype$Number_p = round(table_ptype$Number_p, digits = 2)
table_ptype$Mean = round(table_ptype$Mean, digits = 2)
table_ptype$St_dev = round(table_ptype$St_dev, digits = 2)
table_ptype$Median = round(table_ptype$Median, digits = 2)
table_ptype$Q1 = round(table_ptype$Q1, digits = 2)
table_ptype$Q3 = round(table_ptype$Q3, digits = 2)



table_ptype$median_iqr <- paste(table_ptype$Median, "(",   table_ptype$Q1, "-", table_ptype$Q3, ")")

table_ptype = select(table_ptype, -c("Median", "Q1", "Q3"))


pwt_hco = pairwise.wilcox.test(trust_data$final_tov, trust_data$trust_type, p.adjust.method = "bonf")

pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Trust_type <- row.names(pwt_hco) 


table_ptype = left_join(table_ptype, pwt_hco, by = "Trust_type")

table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))





##### Foundation/non-foundation trusts per year

##### Foundation trusts

trust_foundation = trust_data[ which(trust_data$foundation_trust=='1'),]
nrust_notfound = trust_data[ which(trust_data$foundation_trust=='0'),]

table_hco<-data.frame(Year= character(0) , Trust_number = character(0), Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

b<-c("2015","2016","2017","2018")

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_foundation%>%filter(year %in% b[i])
  table_hco$Trust_number[table_hco$Year==b[i]]<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_foundation%>%filter(year %in% b[i])
  table_hco$Value_p[table_hco$Year==b[i]]<-sqldf("select sum(final_tov) from seged_2")
  rm(seged_2)
  
}
for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_foundation%>%filter(year %in% b[i])
  table_hco$Number_p[table_hco$Year==b[i]]<-sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(5:13),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_foundation%>%filter(year %in% b[i])
  table_hco$Mean[table_hco$Year==b[i]]<-sqldf("select avg(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:9),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_foundation%>%filter(year %in% b[i])
  table_hco$St_dev[table_hco$Year==b[i]]<-sqldf("select STDEV(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_foundation%>%filter(year %in% b[i])
  table_hco$Median[table_hco$Year==b[i]]<-sqldf("select median(final_tov) from seged_2")
  rm(seged_2)
  
}


table_hco = table_hco[-(5:8),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_foundation%>%filter(year %in% b[i])
  table_hco$Q1[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_foundation%>%filter(year %in% b[i])
  table_hco$Q3[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:12),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)


table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


JonckheereTerpstraTest(trust_foundation$final_tov, trust_foundation$year, nperm = 1000, alternative = "decreasing")


pwt_hco = pairwise.wilcox.test(trust_foundation$final_tov, trust_foundation$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_hco = left_join(table_hco, pwt_hco, by = "Year")

summary(trust_acute$final_tov)
sum(trust_acute$final_tov)
sd(trust_acute$final_tov)


#### Totals


table_hco<-data.frame(Trust_number = character(0), Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_foundation
  table_hco$Trust_number<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_foundation
  table_hco$Value_p<- sum(seged_2$final_tov) 
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_foundation
  table_hco$Number_p<-length(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_foundation
  table_hco$Mean<-mean(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_foundation
  table_hco$St_dev<-sd(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_foundation
  table_hco$Median<-median(seged_2$final_tov)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]





for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_foundation
  table_hco$Q1<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_foundation
  table_hco$Q3<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)


table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))




##### Foundation/non-foundation trusts per year

##### Non-foundation trusts

trust_foundation = trust_data[ which(trust_data$foundation_trust=='1'),]
nrust_notfound = trust_data[ which(trust_data$foundation_trust=='0'),]

table_hco<-data.frame(Year= character(0) , Trust_number = character(0), Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

b<-c("2015","2016","2017","2018")

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-nrust_notfound%>%filter(year %in% b[i])
  table_hco$Trust_number[table_hco$Year==b[i]]<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-nrust_notfound%>%filter(year %in% b[i])
  table_hco$Value_p[table_hco$Year==b[i]]<-sqldf("select sum(final_tov) from seged_2")
  rm(seged_2)
  
}
for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-nrust_notfound%>%filter(year %in% b[i])
  table_hco$Number_p[table_hco$Year==b[i]]<-sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(5:13),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-nrust_notfound%>%filter(year %in% b[i])
  table_hco$Mean[table_hco$Year==b[i]]<-sqldf("select avg(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:9),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-nrust_notfound%>%filter(year %in% b[i])
  table_hco$St_dev[table_hco$Year==b[i]]<-sqldf("select STDEV(final_tov) from seged_2")
  rm(seged_2)
  
}



table_hco = table_hco[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-nrust_notfound%>%filter(year %in% b[i])
  table_hco$Median[table_hco$Year==b[i]]<-sqldf("select median(final_tov) from seged_2")
  rm(seged_2)
  
}


table_hco = table_hco[-(5:8),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-nrust_notfound%>%filter(year %in% b[i])
  table_hco$Q1[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-nrust_notfound%>%filter(year %in% b[i])
  table_hco$Q3[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:12),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)


table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


JonckheereTerpstraTest(nrust_notfound$final_tov, nrust_notfound$year, nperm = 1000, alternative = "decreasing")



pwt_hco = pairwise.wilcox.test(nrust_notfound$final_tov, nrust_notfound$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_hco = left_join(table_hco, pwt_hco, by = "Year")

summary(trust_acute$final_tov)
sum(trust_acute$final_tov)
sd(trust_acute$final_tov)


#### Totals


table_hco<-data.frame(Trust_number = character(0), Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-nrust_notfound
  table_hco$Trust_number<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-nrust_notfound
  table_hco$Value_p<- sum(seged_2$final_tov) 
  rm(seged_2)
  
}

table_hco = table_hco[-(2:13),]

for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-nrust_notfound
  table_hco$Number_p<-length(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-nrust_notfound
  table_hco$Mean<-mean(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-nrust_notfound
  table_hco$St_dev<-sd(seged_2$final_tov)
  rm(seged_2)
  
}



table_hco = table_hco[-(2:13),]



for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-nrust_notfound
  table_hco$Median<-median(seged_2$final_tov)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]





for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-nrust_notfound
  table_hco$Q1<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


for(i in 1){
  
  seged_1<-data.frame(Trust_number=0, Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-nrust_notfound
  table_hco$Q3<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:13),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)


table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)

table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))




##### share of funding from the top donors
library(tidyr)
company_tov = aggregate(trust_data$final_tov, by=list(Category=trust_data$company), FUN=sum)
company_tov <- company_tov[with(company_tov,order(-x)),]


bayer <- trust_data[trust_data$company == 'Bayer Plc', ]
novartis <- trust_data[trust_data$company == 'Novartis Pharmaceuticals UK Ltd', ]
pfizer <- trust_data[trust_data$company == 'Pfizer Ltd', ]
astra <- trust_data[trust_data$company == 'AstraZeneca', ]
sanofi <- trust_data[trust_data$company == 'Sanofi Aventis', ]
abbvie <- trust_data[trust_data$company == 'AbbVie Limited', ]
gilead <- trust_data[trust_data$company == 'Gilead', ]
janssen <- trust_data[trust_data$company == 'Janssen-Cilag Ltd', ]
roche <- trust_data[trust_data$company == 'Roche Products Limited', ]
biogen <- trust_data[trust_data$company == 'Biogen Idec Ltd', ]

top_company = rbind(bayer,novartis,pfizer,astra,sanofi,abbvie,gilead,janssen,roche,biogen)

table_company = top_company %>% group_by(year) %>%tally()
table_company = aggregate(final_tov ~ year, data=top_company, FUN=sum)

table_company = trust_data %>% group_by(year) %>%tally()
table_company = aggregate(final_tov ~ year, data=trust_data, FUN=sum)



table_hco = hco_data %>% group_by(year) %>%tally()
table_hco = aggregate(final_tov ~ year, data=hco_data, FUN=sum)


table(all_data$year)

table_hco = aggregate(final_tov ~ year, data=all_data, FUN=sum)


############## TABLE FOR NUMBER OF PAYMENT CATEGORIES to TRUSTS




table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("£0","<£100","£100 to £1,000","£1000 to £10,000","£10,000 to £100,000","£100,000 to £500,000","≥£500,000")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- sqldf("select count(distinct row) from seged_2")
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(8:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(final_tov_cat %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- nrow(seged_2)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(8:20),]

table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)



table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))



#### TABLE FOR THE NUMBER OF TRUSTS RECEIVING PAYMENTS BY PAYMENT VALUE CATEGORY

table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("£0","<£100","£100 to £1,000","£1000 to £10,000","£10,000 to £100,000","£100,000 to £500,000","≥£500,000")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(8:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(final_tov_cat %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- length(unique(seged_2$trust))
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(8:20),]

table_ptype$X2015perc = table_ptype$X2015 / 214 *100
table_ptype$X2016perc = table_ptype$X2016 / 220 *100
table_ptype$X2017perc = table_ptype$X2017 / 214 *100
table_ptype$X2018perc = table_ptype$X2018 / 210 *100
table_ptype$Total_perc = table_ptype$Total / 234 *100

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)


options(digits=10)



table_ptype$X2015 = as.numeric(table_ptype$X2015, digits = 2)
table_ptype$X2016 = as.numeric(table_ptype$X2016, digits = 2)
table_ptype$X2017 = as.numeric(table_ptype$X2017, digits = 2)
table_ptype$X2018 = as.numeric(table_ptype$X2018, digits = 2)
table_ptype$Total = as.numeric(table_ptype$Total, digits = 2)

table_hco$X2015 = prettyNum(table_hco$X2015, big.mark = ",", scientific = FALSE)
table_hco$X2016 = prettyNum(table_hco$X2016, big.mark = ",", scientific = FALSE)
table_hco$X2017 = prettyNum(table_hco$X2017, big.mark = ",", scientific = FALSE)
table_hco$X2018 = prettyNum(table_hco$X2018, big.mark = ",", scientific = FALSE)
table_hco$Total = prettyNum(table_hco$Total, big.mark = ",", scientific = FALSE)





table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

sum(trust_2015$final_tov)
sum(trust_2016$final_tov)
sum(trust_2017$final_tov)
sum(trust_2018$final_tov)
sum(trust_data$final_tov)



#### TABLE FOR THE SUM VALUE OF PAYMENT BY VALUE CATEGORY TO TRUSTS

table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("£0","<£100","£100 to £1,000","£1000 to £10,000","£10,000 to £100,000","£100,000 to £500,000","≥£500,000")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(8:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(final_tov_cat %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(8:20),]

table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)


options(digits=10)



table_ptype$X2015 = as.numeric(table_ptype$X2015, digits = 2)
table_ptype$X2016 = as.numeric(table_ptype$X2016, digits = 2)
table_ptype$X2017 = as.numeric(table_ptype$X2017, digits = 2)
table_ptype$X2018 = as.numeric(table_ptype$X2018, digits = 2)
table_ptype$Total = as.numeric(table_ptype$Total, digits = 2)


table_ptype$X2015 = paste('£',formatC(table_ptype$X2015, big.mark=',', format = 'f', digits = 2))
table_ptype$X2016 = paste('£',formatC(table_ptype$X2016, big.mark=',', format = 'f', digits = 2))
table_ptype$X2017 = paste('£',formatC(table_ptype$X2017, big.mark=',', format = 'f', digits = 2))
table_ptype$X2018 = paste('£',formatC(table_ptype$X2018, big.mark=',', format = 'f', digits = 2))
table_ptype$Total = paste('£',formatC(table_ptype$Total, big.mark=',', format = 'f', digits = 2))




table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


#####################£ SUM VALUE BY PAYMENT CATEGORY DONATIONS


table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("£0","<£100","£100 to £1,000","£1000 to £10,000","£10,000 to £100,000","£100,000 to £500,000","≥£500,000")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_donations)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_donations)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(9:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_donations)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_donations)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(8:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(final_tov_cat %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- sum(seged_2$infl_vat_adj_donations)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(8:20),]

table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015 = round(table_ptype$X2015, digits = 2)
table_ptype$X2016 =  round(table_ptype$X2016, digits = 2)
table_ptype$X2017 =  round(table_ptype$X2017, digits = 2)
table_ptype$X2018 = round(table_ptype$X2018, digits = 2)
table_ptype$Total = round(table_ptype$Total, digits = 2)

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)




table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))




table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

sum(trust_2015$infl_vat_adj_donations)
sum(trust_2016$infl_vat_adj_donations)
sum(trust_2017$infl_vat_adj_donations)
sum(trust_2018$infl_vat_adj_donations)
sum(trust_data$infl_vat_adj_donations)



###### SUm payment per trust per year - payment category

trust_sumtov = aggregate(final_tov ~ trust + year, data=trust_data, FUN=sum)
trust_sumtov_2015 = trust_sumtov[which(trust_sumtov$year==2015), ]
trust_sumtov_2016 = trust_sumtov[which(trust_sumtov$year==2016), ]
trust_sumtov_2017 = trust_sumtov[which(trust_sumtov$year==2017), ]
trust_sumtov_2018 = trust_sumtov[which(trust_sumtov$year==2018), ]
trust_sumtov = aggregate(final_tov ~ trust, data=trust_data, FUN=sum)


trust_sumtov$final_tov_cat = ifelse(trust_sumtov$final_tov == 0, "£0",
                                ifelse(trust_sumtov$final_tov > 0  & trust_sumtov$final_tov < 100, "<£100",
                                       ifelse(trust_sumtov$final_tov >= 100 & trust_sumtov$final_tov < 1000, "£100 to £1,000",
                                              ifelse(trust_sumtov$final_tov >= 1000 & trust_sumtov$final_tov < 10000, "£1000 to £10,000",
                                                     ifelse(trust_sumtov$final_tov >= 10000 & trust_sumtov$final_tov < 100000, "£10,000 to £100,000",
                                                            ifelse(trust_sumtov$final_tov >= 100000 & trust_sumtov$final_tov < 500000, "£100,000 to £500,000",
                                                                   ifelse(trust_sumtov$final_tov >= 500000, "≥£500,000", "other")))))))







table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("£0","<£100","£100 to £1,000","£1000 to £10,000","£10,000 to £100,000","£100,000 to £500,000","≥£500,000")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov_2015%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov_cat)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov_2016%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov_cat)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov_2017%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov_cat)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov_2018%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov_cat)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(8:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov%>%filter(final_tov_cat %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov_cat)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(8:20),]

table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)



table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))




###### PAYMENT CATEGORY PER YEAR - NUMBER OF TRUSTS

trust_long_ptype = aggregate(value ~ trust + year + payment_type, data=trust_long, FUN=sum)
trust_long_ptype_2015 = trust_long_ptype[which(trust_long_ptype$year==2015), ]
trust_long_ptype_2016 = trust_long_ptype[which(trust_long_ptype$year==2016), ]
trust_long_ptype_2017 = trust_long_ptype[which(trust_long_ptype$year==2017), ]
trust_long_ptype_2018 = trust_long_ptype[which(trust_long_ptype$year==2018), ]

table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0))


b<-c("infl_vat_adj_donations","infl_vat_adj_costsofevents","infl_vat_adj_service","infl_vat_adj_jointworking")






for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_ptype_2015%>%filter(payment_type %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- length(which(seged_2$value != 0))
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_ptype_2016%>%filter(payment_type %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- length(which(seged_2$value != 0))
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_ptype_2017%>%filter(payment_type %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- length(which(seged_2$value != 0))
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_ptype_2018%>%filter(payment_type %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- length(which(seged_2$value != 0))
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)



table_ptype = table_ptype[-(5:20),]

table_ptype$X2015perc = table_ptype$X2015 / 214 *100
table_ptype$X2016perc = table_ptype$X2016 / 220 *100
table_ptype$X2017perc = table_ptype$X2017 / 214 *100
table_ptype$X2018perc = table_ptype$X2018 / 210 *100


table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)




table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")


table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc"))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))




trust_long_ptype = aggregate(value ~ trust + year + payment_type, data=trust_long, FUN=sum)


trust_long_ptype = trust_long_ptype[which(trust_long_ptype$value!=0), ]

trust_long_ptype <- trust_long_ptype %>% group_by(trust, payment_type) %>% mutate(N=n())



library(writexl)
write_xlsx(trust_long_ptype, "/Users/esztersaghy/Desktop/Disclosure\ UK/2021\ work/NHS\ Trust\\trust_long_ptype.xlsx")

trust_long_jointwork = trust_long_ptype[which(trust_long_ptype$payment_type=="infl_vat_adj_jointworking"), ]
trust_long_donation = trust_long_ptype[which(trust_long_ptype$payment_type=="infl_vat_adj_donations"), ]
trust_long_event = trust_long_ptype[which(trust_long_ptype$payment_type=="infl_vat_adj_costsofevents"), ]
trust_long_service = trust_long_ptype[which(trust_long_ptype$payment_type=="infl_vat_adj_service"), ]

trust_long_jointwork = trust_long_jointwork[!duplicated(trust_long_jointwork[,c('trust')]),]
trust_long_donation = trust_long_donation[!duplicated(trust_long_donation[,c('trust')]),]
trust_long_event = trust_long_event[!duplicated(trust_long_event[,c('trust')]),]
trust_long_service = trust_long_service[!duplicated(trust_long_service[,c('trust')]),]


trust_long_jointwork = trust_long_jointwork[which(trust_long_jointwork$N==4), ]
trust_long_donation = trust_long_donation[which(trust_long_donation$N==4), ]
trust_long_event = trust_long_event[which(trust_long_event$N==4), ]
trust_long_service = trust_long_service[which(trust_long_service$N==4), ]



########################################
##############   PAYMENTS TO TRUSTS AGGREGATED AT THE TRUST LEVEL SUMMARY ##########################

trust_sumtov = aggregate(final_tov ~ trust + year, data=trust_data, FUN=sum)

table_hco<-data.frame(Year= character(0) , Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

b<-c("2015","2016","2017","2018")

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov%>%filter(year %in% b[i])
  table_hco$Value_p[table_hco$Year==b[i]]<-sum(seged_2$final_tov)
  rm(seged_2)
  
}


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov%>%filter(year %in% b[i])
  table_hco$Number_p[table_hco$Year==b[i]]<-length(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(5:13),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov%>%filter(year %in% b[i])
  table_hco$Mean[table_hco$Year==b[i]]<-mean(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(trust_sumtov$final_tov, by=list(Category=trust_sumtov$year), FUN=mean)  

table_hco = table_hco[-(5:9),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov%>%filter(year %in% b[i])
  table_hco$St_dev[table_hco$Year==b[i]]<-sd(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_hco = table_hco[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov%>%filter(year %in% b[i])
  table_hco$Median[table_hco$Year==b[i]]<-median(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_hco = table_hco[-(5:8),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov%>%filter(year %in% b[i])
  table_hco$Q1[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov%>%filter(year %in% b[i])
  table_hco$Q3[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:12),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)



table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


JonckheereTerpstraTest(trust_sumtov$final_tov, trust_sumtov$year, nperm = 1000, alternative = "decreasing")

pwt_hco = pairwise.wilcox.test(trust_sumtov$final_tov, trust_sumtov$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_hco = left_join(table_hco, pwt_hco, by = "Year")

summary(hco_data$final_tov)
sum(hco_data$final_tov)
sd(hco_data$final_tov)





#### Total

table_hco<-data.frame(Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))


for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov
  table_hco$Value_p<-sum(seged_2$final_tov)
  rm(seged_2)
  
}


for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov
  table_hco$Number_p<-length(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(5:13),]



for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov
  table_hco$Mean<-mean(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(trust_sumtov$final_tov, by=list(Category=trust_sumtov$year), FUN=mean)  

table_hco = table_hco[-(5:9),]



for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov
  table_hco$St_dev<-sd(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_hco = table_hco[-(5:8),]



for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov
  table_hco$Median<-median(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_hco = table_hco[-(5:8),]





for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov
  table_hco$Q1<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:19),]


for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov
  table_hco$Q3<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:12),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)



table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


JonckheereTerpstraTest(trust_sumtov$final_tov, trust_sumtov$year, nperm = 1000, alternative = "decreasing")

pwt_hco = pairwise.wilcox.test(trust_sumtov$final_tov, trust_sumtov$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_hco = left_join(table_hco, pwt_hco, by = "Year")

summary(hco_data$final_tov)
sum(hco_data$final_tov)
sd(hco_data$final_tov)




########################################################################################################################
########################################################################################################################
#### Top 10 trusts

library(tidyr)
trust_sumtov_all = aggregate(trust_data$final_tov, by=list(Category=trust_data$trust), FUN=sum)
trust_sumtov_all <- trust_sumtov_all[with(trust_sumtov_all,order(-x)),]


bayer <- trust_data[trust_data$trust == 'Manchester University NHS Foundation Trust', ]
novartis <- trust_data[trust_data$trust == "Guy's and St Thomas' NHS Foundation Trust", ]
pfizer <- trust_data[trust_data$trust == 'University College London Hospitals NHS Foundation Trust', ]
astra <- trust_data[trust_data$trust == 'Oxford University Hospitals NHS Foundation Trust', ]
sanofi <- trust_data[trust_data$trust == 'University Hospitals Birmingham NHS Foundation Trust', ]
abbvie <- trust_data[trust_data$trust == "King's College Hospital NHS Foundation Trust", ]
gilead <- trust_data[trust_data$trust == 'University Hospitals Of Leicester NHS Trust', ]
janssen <- trust_data[trust_data$trust == 'Imperial College Healthcare NHS Trust', ]
roche <- trust_data[trust_data$trust == 'Royal Brompton and Harefield NHS Foundation Trust', ]
biogen <- trust_data[trust_data$trust == 'Leeds Teaching Hospitals NHS Trust', ]


top_trust = rbind(bayer,novartis,pfizer,astra,sanofi,abbvie,gilead,janssen,roche,biogen)

table_trust = top_trust %>% group_by(year) %>%tally()
table_trust = aggregate(final_tov ~ year, data=top_trust, FUN=sum)

table_trust = trust_data %>% group_by(year) %>%tally()
table_trust = aggregate(final_tov ~ year, data=trust_data, FUN=sum)



table_hco = hco_data %>% group_by(year) %>%tally()
table_hco = aggregate(final_tov ~ year, data=hco_data, FUN=sum)





length(top_trust$final_tov)

length(trust_data$final_tov)




########################################################################################################################
########################################################################################################################
#### Regional breakdown


trust_data$Region[trust_data$Region == 'East England'] = "East of England"


east_england <- trust_data[which(trust_data$Region == "East of England"), ]
east_midlands <- trust_data[trust_data$Region == "East Midlands", ]
london <- trust_data[trust_data$Region == 'Greater London', ]
north_east <- trust_data[trust_data$Region == 'North East', ]
north_west <- trust_data[trust_data$Region == 'North West', ]
south_east <- trust_data[trust_data$Region == "South East", ]
south_west <- trust_data[trust_data$Region == "South West", ]
west_midlands <- trust_data[trust_data$Region == 'West Midlands', ]




table_hco<-data.frame(Year= character(0) , Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))

b<-c("2015","2016","2017","2018")

for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-south_west%>%filter(year %in% b[i])
  table_hco$Value_p[table_hco$Year==b[i]]<-sum(seged_2$final_tov)
  rm(seged_2)
  
}


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-south_west%>%filter(year %in% b[i])
  table_hco$Number_p[table_hco$Year==b[i]]<-length(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(5:13),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-south_west%>%filter(year %in% b[i])
  table_hco$Mean[table_hco$Year==b[i]]<-mean(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(trust_sumtov$final_tov, by=list(Category=trust_sumtov$year), FUN=mean)  

table_hco = table_hco[-(5:9),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-south_west%>%filter(year %in% b[i])
  table_hco$St_dev[table_hco$Year==b[i]]<-sd(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_hco = table_hco[-(5:8),]



for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-south_west%>%filter(year %in% b[i])
  table_hco$Median[table_hco$Year==b[i]]<-median(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_hco = table_hco[-(5:8),]





for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-south_west%>%filter(year %in% b[i])
  table_hco$Q1[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:19),]


for(i in 1:length(b)){
  
  seged_1<-data.frame(Year=b[i], Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-south_west%>%filter(year %in% b[i])
  table_hco$Q3[table_hco$Year==b[i]]<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:12),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)


table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


JonckheereTerpstraTest(south_west$final_tov, south_west$year, nperm = 1000, alternative = "decreasing")

pwt_hco = pairwise.wilcox.test(south_west$final_tov, south_west$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_hco = left_join(table_hco, pwt_hco, by = "Year")

aggregate(data = west_midlands, trust ~ year,function(x) length(unique(x)))

length(unique(west_midlands$trust))


#### Total

table_hco<-data.frame(Value_p = character(0) , Number_p = character(0) , Mean=character(0), St_dev=character(0), Median=character(0), Q1=character(0), Q3=character(0))


for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-south_west
  table_hco$Value_p<-sum(seged_2$final_tov)
  rm(seged_2)
  
}


for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-south_west
  table_hco$Number_p<-length(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sum)  

table_hco = table_hco[-(5:13),]



for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-south_west
  table_hco$Mean<-mean(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(trust_sumtov$final_tov, by=list(Category=trust_sumtov$year), FUN=mean)  

table_hco = table_hco[-(5:9),]



for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-south_west
  table_hco$St_dev<-sd(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=sd)  

table_hco = table_hco[-(5:8),]



for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-south_west
  table_hco$Median<-median(seged_2$final_tov)
  rm(seged_2)
  
}

aggregate(hco_data$final_tov, by=list(Category=hco_data$year), FUN=median)  

table_hco = table_hco[-(5:8),]





for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-south_west
  table_hco$Q1<- quantile(seged_2$final_tov, 0.25)
  rm(seged_2)
  
}


table_hco = table_hco[-(5:19),]


for(i in 1){
  
  seged_1<-data.frame(Value_p=0, Number_p=0, Mean=0, St_dev=0, Median=0, Q1=0, Q3=0)
  table_hco<-rbind(table_hco,seged_1)
  rm(seged_1)
  seged_2<-south_west
  table_hco$Q3<- quantile(seged_2$final_tov, 0.75)
  rm(seged_2)
  
}


table_hco = table_hco[-(2:12),]


options(digits=10)



table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = as.numeric(table_hco$Value_p, digits = 2)
table_hco$Number_p = as.numeric(table_hco$Number_p, digits = 2)
table_hco$Mean = as.numeric(table_hco$Mean, digits = 2)
table_hco$St_dev = as.numeric(table_hco$St_dev, digits = 2)
table_hco$Median = as.numeric(table_hco$Median, digits = 2)
table_hco$Q1 = as.numeric(table_hco$Q1, digits = 2)
table_hco$Q3 = as.numeric(table_hco$Q3, digits = 2)

table_hco$Year = as.numeric(table_hco$Year)
table_hco$Value_p = round(table_hco$Value_p, digits = 2)
table_hco$Number_p = round(table_hco$Number_p, digits = 2)
table_hco$Mean = round(table_hco$Mean, digits = 2)
table_hco$St_dev = round(table_hco$St_dev, digits = 2)
table_hco$Median = round(table_hco$Median, digits = 2)
table_hco$Q1 = round(table_hco$Q1, digits = 2)
table_hco$Q3 = round(table_hco$Q3, digits = 2)

table_hco$Value_p = paste('£',formatC(table_hco$Value_p, big.mark=',', format = 'f', digits = 2))
table_hco$Mean = paste('£',formatC(table_hco$Mean, big.mark=',', format = 'f', digits = 2))
table_hco$St_dev = paste('£',formatC(table_hco$St_dev, big.mark=',', format = 'f', digits = 2))
table_hco$Median = paste('£',formatC(table_hco$Median, big.mark=',', format = 'f', digits = 2))
table_hco$Q1 = paste('£',formatC(table_hco$Q1, big.mark=',', format = 'f', digits = 2))
table_hco$Q3 = paste('£',formatC(table_hco$Q3, big.mark=',', format = 'f', digits = 2))
table_hco$Number_p = prettyNum(table_hco$Number_p, big.mark = ",", scientific = FALSE)

table_hco$median_iqr <- paste(table_hco$Median, " ", "(",   table_hco$Q1, "-", table_hco$Q3, ")")

table_hco = select(table_hco, -c("Median", "Q1", "Q3"))

table_hco = table_hco %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

JonckheereTerpstraTest(east_england$final_tov, east_england$year, nperm = 1000, alternative = "decreasing")

pwt_hco = pairwise.wilcox.test(east_midlannds$final_tov, east_midlannds$year, p.adjust.method = "bonf")
pwt_hco = pwt_hco$p.value
pwt_hco = as.data.frame(pwt_hco)
pwt_hco = select(pwt_hco, 1)

pwt_hco$Year <- row.names(pwt_hco) 
pwt_hco$Year <- as.numeric(pwt_hco$Year) 

table_hco = left_join(table_hco, pwt_hco, by = "Year")

summary(hco_data$final_tov)
sum(hco_data$final_tov)
sd(hco_data$final_tov)





#### Descriptive of all data

all_data = read_excel(file.choose())

prefix = read_excel(file.choose())

all_data$Prefix = substr(all_data$Postcode, start = 1, stop = 2)
all_data$Prefix = sub("^([[:alpha:]]*).*", "\\1", all_data$Prefix)


all_data = left_join(all_data, prefix, by = "Prefix")


table(all_data$Region)

#### Select rows outside England from HCO data
hco_nonengland <- all_data[(all_data$Region=="Channel Islands" | all_data$Region=="Ireland" | all_data$Region=="Isle of Man" | all_data$Region=="Non-geographic" | all_data$Region=="Northern Ireland" | all_data$Region=="Scotland" | all_data$Region=="Wales" | all_data$Region=="wrong_postcode"),]
hco_england <- all_data[!(all_data$Region=="Channel Islands" | all_data$Region=="Ireland" | all_data$Region=="Isle of Man" | all_data$Region=="Non-geographic" | all_data$Region=="Northern Ireland" | all_data$Region=="Scotland" | all_data$Region=="Wales" | all_data$Region=="wrong_postcode"),]


table_nonengland = hco_data %>% group_by(year) %>%tally()
table_nonengland = aggregate(final_tov ~ year, data=all_data, FUN=sum)

all_data_noregion = all_data[ which(is.na(all_data$Region)), ]

hco_notrust$Region[is.na(hco_notrust$Region)] = "wrong_postcode"
hco_england$Region[is.na(hco_england$Region)] = "wrong_postcode"


england_trust = hco_england[which(hco_england$HCO_trust == "FALSE"), ]
england_notrust = hco_england[which(hco_england$HCO_trust == "TRUE"), ]



aggregate(all_data$final_tov, by=list(Category=all_data$year), FUN=sum)
sum(hco_england$final_tov)

table(hco_england$year)

aggregate(hco_nonengland$final_tov, by=list(Category=hco_nonengland$year), FUN=sum)
sum(hco_england$final_tov)

table(hco_nonengland$year)




########## Payment value category per year





table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("£0","<£100","£100 to £1,000","£1000 to £10,000","£10,000 to £100,000","£100,000 to £500,000","≥£500,000")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov_cat)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(9:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov_cat)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov_cat)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(8:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(final_tov_cat %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov_cat)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(8:20),]

table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015 = round(table_ptype$X2015, digits = 2)
table_ptype$X2016 =  round(table_ptype$X2016, digits = 2)
table_ptype$X2017 =  round(table_ptype$X2017, digits = 2)
table_ptype$X2018 = round(table_ptype$X2018, digits = 2)
table_ptype$Total = round(table_ptype$Total, digits = 2)

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)




table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))




table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))




###### SUm payment per trust per year - payment category

trust_sumtov = aggregate(final_tov ~ trust + year, data=trust_data, FUN=sum)
trust_sumtov_2015 = trust_sumtov[which(trust_sumtov$year==2015), ]
trust_sumtov_2016 = trust_sumtov[which(trust_sumtov$year==2016), ]
trust_sumtov_2017 = trust_sumtov[which(trust_sumtov$year==2017), ]
trust_sumtov_2018 = trust_sumtov[which(trust_sumtov$year==2018), ]
trust_sumtov = aggregate(final_tov ~ trust, data=trust_data, FUN=sum)


trust_sumtov$final_tov_cat = ifelse(trust_sumtov$final_tov == 0, "£0",
                                    ifelse(trust_sumtov$final_tov > 0  & trust_sumtov$final_tov < 100, "<£100",
                                           ifelse(trust_sumtov$final_tov >= 100 & trust_sumtov$final_tov < 1000, "£100 to £1,000",
                                                  ifelse(trust_sumtov$final_tov >= 1000 & trust_sumtov$final_tov < 10000, "£1000 to £10,000",
                                                         ifelse(trust_sumtov$final_tov >= 10000 & trust_sumtov$final_tov < 100000, "£10,000 to £100,000",
                                                                ifelse(trust_sumtov$final_tov >= 100000 & trust_sumtov$final_tov < 500000, "£100,000 to £500,000",
                                                                       ifelse(trust_sumtov$final_tov >= 500000, "≥£500,000", "other")))))))







table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("£0","<£100","£100 to £1,000","£1000 to £10,000","£10,000 to £100,000","£100,000 to £500,000","≥£500,000")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov_2015%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov_cat)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov_2016%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov_cat)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov_2017%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov_cat)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov_2018%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov_cat)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(8:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_sumtov%>%filter(final_tov_cat %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov_cat)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(8:20),]

table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)



table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))




###### PAYMENT CATEGORY PER YEAR - NUMBER OF TRUSTS

trust_long_ptype = aggregate(value ~ trust + year + payment_type, data=trust_long, FUN=sum)
trust_long_ptype_2015 = trust_long_ptype[which(trust_long_ptype$year==2015), ]
trust_long_ptype_2016 = trust_long_ptype[which(trust_long_ptype$year==2016), ]
trust_long_ptype_2017 = trust_long_ptype[which(trust_long_ptype$year==2017), ]
trust_long_ptype_2018 = trust_long_ptype[which(trust_long_ptype$year==2018), ]

table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0))


b<-c("infl_vat_adj_donations","infl_vat_adj_costsofevents","infl_vat_adj_service","infl_vat_adj_jointworking")






for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_ptype_2015%>%filter(payment_type %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- length(which(seged_2$value != 0))
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_ptype_2016%>%filter(payment_type %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- length(which(seged_2$value != 0))
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_ptype_2017%>%filter(payment_type %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- length(which(seged_2$value != 0))
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_ptype_2018%>%filter(payment_type %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- length(which(seged_2$value != 0))
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)



table_ptype = table_ptype[-(5:20),]

table_ptype$X2015perc = table_ptype$X2015 / 214 *100
table_ptype$X2016perc = table_ptype$X2016 / 220 *100
table_ptype$X2017perc = table_ptype$X2017 / 214 *100
table_ptype$X2018perc = table_ptype$X2018 / 210 *100


table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)




table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")


table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc"))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))




trust_long_ptype = aggregate(value ~ trust + year + payment_type, data=trust_long, FUN=sum)


trust_long_ptype = trust_long_ptype[which(trust_long_ptype$value!=0), ]

trust_long_ptype <- trust_long_ptype %>% group_by(trust, payment_type) %>% mutate(N=n())







###################################################################################
####### Mean by payment type over the years





table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2016"=character(0), "2017"=character(0),"2018"=character(0),Total=character(0))


b<-c("infl_vat_adj_donations","infl_vat_adj_costsofevents","infl_vat_adj_service","infl_vat_adj_jointworking")

seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2015%>%filter(payment_type %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- mean(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2016%>%filter(payment_type %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- mean(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(9:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2017%>%filter(payment_type %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- mean(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2018%>%filter(payment_type %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- mean(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(8:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long%>%filter(payment_type %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- mean(seged_2$value)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(5:20),]



table_ptype$X2015 = round(table_ptype$X2015, digits = 2)
table_ptype$X2016 =  round(table_ptype$X2016, digits = 2)
table_ptype$X2017 =  round(table_ptype$X2017, digits = 2)
table_ptype$X2018 = round(table_ptype$X2018, digits = 2)
table_ptype$Total = round(table_ptype$Total, digits = 2)


table_ptype$X2015 = paste('£',formatC(table_ptype$X2015, big.mark=',', format = 'f', digits = 2))
table_ptype$X2016 = paste('£',formatC(table_ptype$X2016, big.mark=',', format = 'f', digits = 2))
table_ptype$X2017 = paste('£',formatC(table_ptype$X2017, big.mark=',', format = 'f', digits = 2))
table_ptype$X2018 = paste('£',formatC(table_ptype$X2018, big.mark=',', format = 'f', digits = 2))
table_ptype$Total = paste('£',formatC(table_ptype$Total, big.mark=',', format = 'f', digits = 2))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

mean(trust_long_2015$value)
mean(trust_long_2016$value)
mean(trust_long_2017$value)
mean(trust_long_2018$value)
mean(trust_long$value)





###################################################################################
####### Standard deviation by payment type over the years





table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2016"=character(0), "2017"=character(0),"2018"=character(0),Total=character(0))


b<-c("infl_vat_adj_donations","infl_vat_adj_costsofevents","infl_vat_adj_service","infl_vat_adj_jointworking")

seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2015%>%filter(payment_type %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- sd(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2016%>%filter(payment_type %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- sd(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(9:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2017%>%filter(payment_type %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- sd(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2018%>%filter(payment_type %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- sd(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(8:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long%>%filter(payment_type %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- sd(seged_2$value)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(5:20),]



table_ptype$X2015 = round(table_ptype$X2015, digits = 2)
table_ptype$X2016 =  round(table_ptype$X2016, digits = 2)
table_ptype$X2017 =  round(table_ptype$X2017, digits = 2)
table_ptype$X2018 = round(table_ptype$X2018, digits = 2)
table_ptype$Total = round(table_ptype$Total, digits = 2)


table_ptype$X2015 = paste('£',formatC(table_ptype$X2015, big.mark=',', format = 'f', digits = 2))
table_ptype$X2016 = paste('£',formatC(table_ptype$X2016, big.mark=',', format = 'f', digits = 2))
table_ptype$X2017 = paste('£',formatC(table_ptype$X2017, big.mark=',', format = 'f', digits = 2))
table_ptype$X2018 = paste('£',formatC(table_ptype$X2018, big.mark=',', format = 'f', digits = 2))
table_ptype$Total = paste('£',formatC(table_ptype$Total, big.mark=',', format = 'f', digits = 2))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

sd(trust_long_2015$value)
sd(trust_long_2016$value)
sd(trust_long_2017$value)
sd(trust_long_2018$value)
sd(trust_long$value)









###################################################################################
####### Median and IQR by payment type over the years





table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0),"2015_Q1"=character(0), "2015_Q3"=character(0),  "2016"=character(0),"2016_Q1"=character(0), "2016_Q3"=character(0),  "2017"=character(0), "2017_Q1"=character(0), "2017_Q3"=character(0),  "2018"=character(0),"2018_Q1"=character(0), "2018_Q3"=character(0),  Total=character(0), "Total_Q1"=character(0), "Total_Q3"=character(0))


b<-c("infl_vat_adj_donations","infl_vat_adj_costsofevents","infl_vat_adj_service","infl_vat_adj_jointworking")

seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2015_Q1"=0,"2015_Q3"=0,"2016"=0,"2016_Q1"=0,"2016_Q3"=0, "2017"=0,"2017_Q1"=0,"2017_Q3"=0,"2018"=0,"2018_Q1"=0,"2018_Q3"=0,Total=0,"Total_Q1"=0,"Total_Q3"=0)



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2015_Q1"=0,"2015_Q3"=0,"2016"=0,"2016_Q1"=0,"2016_Q3"=0, "2017"=0,"2017_Q1"=0,"2017_Q3"=0,"2018"=0,"2018_Q1"=0,"2018_Q3"=0,Total=0,"Total_Q1"=0,"Total_Q3"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2015%>%filter(payment_type %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- median(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2015_Q1"=0,"2015_Q3"=0,"2016"=0,"2016_Q1"=0,"2016_Q3"=0, "2017"=0,"2017_Q1"=0,"2017_Q3"=0,"2018"=0,"2018_Q1"=0,"2018_Q3"=0,Total=0,"Total_Q1"=0,"Total_Q3"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2015%>%filter(payment_type %in% b[i])
  table_ptype$"X2015_Q1"[table_ptype$Payment_value==b[i]]<- quantile(seged_2$value, 0.25)
  rm(seged_2)
  
}

table_ptype$X2015_Q1 = as.numeric(table_ptype$X2015_Q1)

for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2015_Q1"=0,"2015_Q3"=0,"2016"=0,"2016_Q1"=0,"2016_Q3"=0, "2017"=0,"2017_Q1"=0,"2017_Q3"=0,"2018"=0,"2018_Q1"=0,"2018_Q3"=0,Total=0,"Total_Q1"=0,"Total_Q3"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2015%>%filter(payment_type %in% b[i])
  table_ptype$"X2015_Q3"[table_ptype$Payment_value==b[i]]<- quantile(seged_2$value, 0.75)
  rm(seged_2)
  
}

table_ptype$X2015_Q3 = as.numeric(table_ptype$X2015_Q3)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2015_Q1"=0,"2015_Q3"=0,"2016"=0,"2016_Q1"=0,"2016_Q3"=0, "2017"=0,"2017_Q1"=0,"2017_Q3"=0,"2018"=0,"2018_Q1"=0,"2018_Q3"=0,Total=0,"Total_Q1"=0,"Total_Q3"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2016%>%filter(payment_type %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- median(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(5:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2015_Q1"=0,"2015_Q3"=0,"2016"=0,"2016_Q1"=0,"2016_Q3"=0, "2017"=0,"2017_Q1"=0,"2017_Q3"=0,"2018"=0,"2018_Q1"=0,"2018_Q3"=0,Total=0,"Total_Q1"=0,"Total_Q3"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2016%>%filter(payment_type %in% b[i])
  table_ptype$"X2016_Q1"[table_ptype$Payment_value==b[i]]<- quantile(seged_2$value, 0.25)
  rm(seged_2)
  
}

table_ptype$X2016_Q1 = as.numeric(table_ptype$X2016_Q1)

for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2015_Q1"=0,"2015_Q3"=0,"2016"=0,"2016_Q1"=0,"2016_Q3"=0, "2017"=0,"2017_Q1"=0,"2017_Q3"=0,"2018"=0,"2018_Q1"=0,"2018_Q3"=0,Total=0,"Total_Q1"=0,"Total_Q3"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2016%>%filter(payment_type %in% b[i])
  table_ptype$"X2016_Q3"[table_ptype$Payment_value==b[i]]<- quantile(seged_2$value, 0.75)
  rm(seged_2)
  
}

table_ptype$X2016_Q3 = as.numeric(table_ptype$X2016_Q3)

for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2015_Q1"=0,"2015_Q3"=0,"2016"=0,"2016_Q1"=0,"2016_Q3"=0, "2017"=0,"2017_Q1"=0,"2017_Q3"=0,"2018"=0,"2018_Q1"=0,"2018_Q3"=0,Total=0,"Total_Q1"=0,"Total_Q3"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2017%>%filter(payment_type %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- median(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(5:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2015_Q1"=0,"2015_Q3"=0,"2016"=0,"2016_Q1"=0,"2016_Q3"=0, "2017"=0,"2017_Q1"=0,"2017_Q3"=0,"2018"=0,"2018_Q1"=0,"2018_Q3"=0,Total=0,"Total_Q1"=0,"Total_Q3"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2017%>%filter(payment_type %in% b[i])
  table_ptype$"X2017_Q1"[table_ptype$Payment_value==b[i]]<- quantile(seged_2$value, 0.25)
  rm(seged_2)
  
}

table_ptype$X2017_Q1 = as.numeric(table_ptype$X2017_Q1)

for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2015_Q1"=0,"2015_Q3"=0,"2016"=0,"2016_Q1"=0,"2016_Q3"=0, "2017"=0,"2017_Q1"=0,"2017_Q3"=0,"2018"=0,"2018_Q1"=0,"2018_Q3"=0,Total=0,"Total_Q1"=0,"Total_Q3"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2017%>%filter(payment_type %in% b[i])
  table_ptype$"X2017_Q3"[table_ptype$Payment_value==b[i]]<- quantile(seged_2$value, 0.75)
  rm(seged_2)
  
}

table_ptype$X2017_Q3 = as.numeric(table_ptype$X2017_Q3)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2015_Q1"=0,"2015_Q3"=0,"2016"=0,"2016_Q1"=0,"2016_Q3"=0, "2017"=0,"2017_Q1"=0,"2017_Q3"=0,"2018"=0,"2018_Q1"=0,"2018_Q3"=0,Total=0,"Total_Q1"=0,"Total_Q3"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2018%>%filter(payment_type %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- median(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(5:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2015_Q1"=0,"2015_Q3"=0,"2016"=0,"2016_Q1"=0,"2016_Q3"=0, "2017"=0,"2017_Q1"=0,"2017_Q3"=0,"2018"=0,"2018_Q1"=0,"2018_Q3"=0,Total=0,"Total_Q1"=0,"Total_Q3"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2018%>%filter(payment_type %in% b[i])
  table_ptype$"X2018_Q1"[table_ptype$Payment_value==b[i]]<- quantile(seged_2$value, 0.25)
  rm(seged_2)
  
}

table_ptype$X2018_Q1 = as.numeric(table_ptype$X2018_Q1)

for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2015_Q1"=0,"2015_Q3"=0,"2016"=0,"2016_Q1"=0,"2016_Q3"=0, "2017"=0,"2017_Q1"=0,"2017_Q3"=0,"2018"=0,"2018_Q1"=0,"2018_Q3"=0,Total=0,"Total_Q1"=0,"Total_Q3"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2018%>%filter(payment_type %in% b[i])
  table_ptype$"X2018_Q3"[table_ptype$Payment_value==b[i]]<- quantile(seged_2$value, 0.75)
  rm(seged_2)
  
}

table_ptype$X2018_Q3 = as.numeric(table_ptype$X2018_Q3)

for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2015_Q1"=0,"2015_Q3"=0,"2016"=0,"2016_Q1"=0,"2016_Q3"=0, "2017"=0,"2017_Q1"=0,"2017_Q3"=0,"2018"=0,"2018_Q1"=0,"2018_Q3"=0,Total=0,"Total_Q1"=0,"Total_Q3"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long%>%filter(payment_type %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- median(seged_2$value)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2015_Q1"=0,"2015_Q3"=0,"2016"=0,"2016_Q1"=0,"2016_Q3"=0, "2017"=0,"2017_Q1"=0,"2017_Q3"=0,"2018"=0,"2018_Q1"=0,"2018_Q3"=0,Total=0,"Total_Q1"=0,"Total_Q3"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2018%>%filter(payment_type %in% b[i])
  table_ptype$"Total_Q1"[table_ptype$Payment_value==b[i]]<- quantile(seged_2$value, 0.25)
  rm(seged_2)
  
}

table_ptype$Total_Q1 = as.numeric(table_ptype$Total_Q1)

for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2015_Q1"=0,"2015_Q3"=0,"2016"=0,"2016_Q1"=0,"2016_Q3"=0, "2017"=0,"2017_Q1"=0,"2017_Q3"=0,"2018"=0,"2018_Q1"=0,"2018_Q3"=0,Total=0,"Total_Q1"=0,"Total_Q3"=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long%>%filter(payment_type %in% b[i])
  table_ptype$"Total_Q3"[table_ptype$Payment_value==b[i]]<- quantile(seged_2$value, 0.75)
  rm(seged_2)
  
}

table_ptype$Total_Q3 = as.numeric(table_ptype$Total_Q3)


table_ptype = table_ptype[-(5:20),]



table_ptype$X2015 = round(table_ptype$X2015, digits = 2)
table_ptype$X2015_Q1 = round(table_ptype$X2015_Q1, digits = 2)
table_ptype$X2015_Q3 = round(table_ptype$X2015_Q3, digits = 2)
table_ptype$X2016 =  round(table_ptype$X2016, digits = 2)
table_ptype$X2016_Q1 = round(table_ptype$X2016_Q1, digits = 2)
table_ptype$X2016_Q3 = round(table_ptype$X2016_Q3, digits = 2)
table_ptype$X2017 =  round(table_ptype$X2017, digits = 2)
table_ptype$X2017_Q1 = round(table_ptype$X2017_Q1, digits = 2)
table_ptype$X2017_Q3 = round(table_ptype$X2017_Q3, digits = 2)
table_ptype$X2018 = round(table_ptype$X2018, digits = 2)
table_ptype$X2018_Q1 = round(table_ptype$X2018_Q1, digits = 2)
table_ptype$X2018_Q3 = round(table_ptype$X2018_Q3, digits = 2)
table_ptype$Total = round(table_ptype$Total, digits = 2)
table_ptype$Total_Q1 = round(table_ptype$Total_Q1, digits = 2)
table_ptype$Total_Q3 = round(table_ptype$Total_Q3, digits = 2)


table_ptype$X2015 = paste('£',formatC(table_ptype$X2015, big.mark=',', format = 'f', digits = 2))
table_ptype$X2015_Q1 = paste('£',formatC(table_ptype$X2015_Q1, big.mark=',', format = 'f', digits = 2))
table_ptype$X2015_Q3 = paste('£',formatC(table_ptype$X2015_Q3, big.mark=',', format = 'f', digits = 2))
table_ptype$X2016 = paste('£',formatC(table_ptype$X2016, big.mark=',', format = 'f', digits = 2))
table_ptype$X2016_Q1 = paste('£',formatC(table_ptype$X2016_Q1, big.mark=',', format = 'f', digits = 2))
table_ptype$X2016_Q3 = paste('£',formatC(table_ptype$X2016_Q3, big.mark=',', format = 'f', digits = 2))
table_ptype$X2017 = paste('£',formatC(table_ptype$X2017, big.mark=',', format = 'f', digits = 2))
table_ptype$X2017_Q1 = paste('£',formatC(table_ptype$X2017_Q1, big.mark=',', format = 'f', digits = 2))
table_ptype$X2017_Q3 = paste('£',formatC(table_ptype$X2017_Q3, big.mark=',', format = 'f', digits = 2))
table_ptype$X2018 = paste('£',formatC(table_ptype$X2018, big.mark=',', format = 'f', digits = 2))
table_ptype$X2018_Q1 = paste('£',formatC(table_ptype$X2018_Q1, big.mark=',', format = 'f', digits = 2))
table_ptype$X2018_Q3 = paste('£',formatC(table_ptype$X2018_Q3, big.mark=',', format = 'f', digits = 2))
table_ptype$Total = paste('£',formatC(table_ptype$Total, big.mark=',', format = 'f', digits = 2))
table_ptype$Total_Q1 = paste('£',formatC(table_ptype$Total_Q1, big.mark=',', format = 'f', digits = 2))
table_ptype$Total_Q3 = paste('£',formatC(table_ptype$Total_Q3, big.mark=',', format = 'f', digits = 2))



table_ptype$median_iqr_2015 <- paste(table_ptype$X2015,"(",table_ptype$X2015_Q1, "-",table_ptype$X2015_Q3,")")
table_ptype$median_iqr_2016 <- paste(table_ptype$X2016,"(",table_ptype$X2016_Q1, "-",table_ptype$X2016_Q3,")")
table_ptype$median_iqr_2017 <- paste(table_ptype$X2017,"(",table_ptype$X2017_Q1, "-",table_ptype$X2017_Q3,")")
table_ptype$median_iqr_2018 <- paste(table_ptype$X2018,"(",table_ptype$X2018_Q1, "-",table_ptype$X2018_Q3,")")
table_ptype$median_iqr_Total <- paste(table_ptype$Total,"(",table_ptype$Total_Q1, "-",table_ptype$Total_Q3,")")


table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2015_Q1", "X2015_Q3","X2016_Q1", "X2016_Q3","X2017_Q1", "X2017_Q3","X2018_Q1", "X2018_Q3","Total_Q1","Total_Q3"))

table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

median(trust_long_2015$value)
median(trust_long_2016$value)
median(trust_long_2017$value)
median(trust_long_2018$value)
median(trust_long$value)

quantile(trust_long_2015$value, 0.25)
quantile(trust_long_2016$value, 0.25)
quantile(trust_long_2017$value, 0.25)
quantile(trust_long_2018$value, 0.25)
quantile(trust_long$value, 0.25)

quantile(trust_long_2015$value, 0.75)
quantile(trust_long_2016$value, 0.75)
quantile(trust_long_2017$value, 0.75)
quantile(trust_long_2018$value, 0.75)
quantile(trust_long$value, 0.75)






###############

########## Number of payments per payment type





table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("£0","<£100","£100 to £1,000","£1000 to £10,000","£10,000 to £100,000","£100,000 to £500,000","≥£500,000")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov_cat)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(9:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov_cat)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(8:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(final_tov_cat %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov_cat)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(8:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(final_tov_cat %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov_cat)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(8:20),]

table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015 = round(table_ptype$X2015, digits = 2)
table_ptype$X2016 =  round(table_ptype$X2016, digits = 2)
table_ptype$X2017 =  round(table_ptype$X2017, digits = 2)
table_ptype$X2018 = round(table_ptype$X2018, digits = 2)
table_ptype$Total = round(table_ptype$Total, digits = 2)

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)




table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))




table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))




###### Number of payment per payment type per year



table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("infl_vat_adj_donations","infl_vat_adj_costsofevents","infl_vat_adj_service","infl_vat_adj_jointworking")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2015%>%filter(payment_type %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- length(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2016%>%filter(payment_type %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- length(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(5:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2017%>%filter(payment_type %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- length(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(5:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2018%>%filter(payment_type %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- length(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(5:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long%>%filter(payment_type %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- length(seged_2$value)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(5:20),]

table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)



table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

length(trust_2015$final_tov)




###### Value of payment per payment type per year



table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("infl_vat_adj_donations","infl_vat_adj_costsofevents","infl_vat_adj_service","infl_vat_adj_jointworking")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2015%>%filter(payment_type %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- sum(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2016%>%filter(payment_type %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- sum(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(5:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2017%>%filter(payment_type %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- sum(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(5:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long_2018%>%filter(payment_type %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- sum(seged_2$value)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(5:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_long%>%filter(payment_type %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- sum(seged_2$value)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(5:20),]

table_ptype$X2015 = round(table_ptype$X2015, digits = 2)
table_ptype$X2016 =  round(table_ptype$X2016, digits = 2)
table_ptype$X2017 =  round(table_ptype$X2017, digits = 2)
table_ptype$X2018 = round(table_ptype$X2018, digits = 2)
table_ptype$Total = round(table_ptype$Total, digits = 2)




table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)

table_ptype$X2015 = paste('£',formatC(table_ptype$X2015, big.mark=',', format = 'f', digits = 2))
table_ptype$X2016 = paste('£',formatC(table_ptype$X2016, big.mark=',', format = 'f', digits = 2))
table_ptype$X2017 = paste('£',formatC(table_ptype$X2017, big.mark=',', format = 'f', digits = 2))
table_ptype$X2018 = paste('£',formatC(table_ptype$X2018, big.mark=',', format = 'f', digits = 2))
table_ptype$Total = paste('£',formatC(table_ptype$Total, big.mark=',', format = 'f', digits = 2))

table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

length(trust_2015$final_tov)







###### Number of payment per trust type (foundation vs. non-foundation) per year



table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("1","2","3","4","5")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(trust_type %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(trust_type %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(6:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(trust_type %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(6:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(trust_type %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(6:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(trust_type %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(6:20),]

table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)



table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

length(trust_2015$final_tov)




###### Value of payment per trust type (foundation vs. non-foundation) per year



table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("1","2","3","4","5")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(trust_type %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(trust_type %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(6:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(trust_type %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(6:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(trust_type %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(6:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(trust_type %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(6:20),]

table_ptype$X2015 = round(table_ptype$X2015, digits = 2)
table_ptype$X2016 =  round(table_ptype$X2016, digits = 2)
table_ptype$X2017 =  round(table_ptype$X2017, digits = 2)
table_ptype$X2018 = round(table_ptype$X2018, digits = 2)
table_ptype$Total = round(table_ptype$Total, digits = 2)




table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)

table_ptype$X2015 = paste('£',formatC(table_ptype$X2015, big.mark=',', format = 'f', digits = 2))
table_ptype$X2016 = paste('£',formatC(table_ptype$X2016, big.mark=',', format = 'f', digits = 2))
table_ptype$X2017 = paste('£',formatC(table_ptype$X2017, big.mark=',', format = 'f', digits = 2))
table_ptype$X2018 = paste('£',formatC(table_ptype$X2018, big.mark=',', format = 'f', digits = 2))
table_ptype$Total = paste('£',formatC(table_ptype$Total, big.mark=',', format = 'f', digits = 2))

table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

length(trust_2015$final_tov)














###### Number of payment per region per year



table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("East Midlands","East of England","Greater London","North East","North West","South East","South West","West Midlands")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(Region %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(Region %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(9:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(Region %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(9:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(Region %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(9:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(Region %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- length(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(9:20),]

table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)

table_ptype$X2015 = prettyNum(table_ptype$X2015, big.mark = ",", scientific = FALSE)
table_ptype$X2016 = prettyNum(table_ptype$X2016, big.mark = ",", scientific = FALSE)
table_ptype$X2017 = prettyNum(table_ptype$X2017, big.mark = ",", scientific = FALSE)
table_ptype$X2018 = prettyNum(table_ptype$X2018, big.mark = ",", scientific = FALSE)
table_ptype$Total = prettyNum(table_ptype$Total, big.mark = ",", scientific = FALSE)

table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

length(trust_2015$final_tov)




###### Value of payment per trust type (foundation vs. non-foundation) per year



table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2015perc"=character(0),"2016"=character(0), "2016perc"=character(0),"2017"=character(0),"2017perc"=character(0), "2018"=character(0),"2018perc"=character(0), Total=character(0), Total_perc=character(0))


b<-c("East Midlands","East of England","Greater London","North East","North West","South East","South West","West Midlands")




for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2015%>%filter(Region %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2016%>%filter(Region %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(9:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2017%>%filter(Region %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(9:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_2018%>%filter(Region %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(9:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0, "2015perc"=0,"2016"=0, "2016perc"=0,"2017"=0,"2017perc"=0, "2018"=0,"2018perc"=0, Total=0, Total_perc=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-trust_data%>%filter(Region %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- sum(seged_2$final_tov)
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(9:20),]

table_ptype$X2015 = round(table_ptype$X2015, digits = 2)
table_ptype$X2016 =  round(table_ptype$X2016, digits = 2)
table_ptype$X2017 =  round(table_ptype$X2017, digits = 2)
table_ptype$X2018 = round(table_ptype$X2018, digits = 2)
table_ptype$Total = round(table_ptype$Total, digits = 2)




table_ptype$X2015perc = table_ptype$X2015 / sum(table_ptype$X2015) *100
table_ptype$X2016perc = table_ptype$X2016 / sum(table_ptype$X2016) *100
table_ptype$X2017perc = table_ptype$X2017 / sum(table_ptype$X2017) *100
table_ptype$X2018perc = table_ptype$X2018 / sum(table_ptype$X2018) *100
table_ptype$Total_perc = table_ptype$Total / sum(table_ptype$Total) *100

table_ptype$X2015perc = round(table_ptype$X2015perc, digits = 2)
table_ptype$X2016perc =  round(table_ptype$X2016perc, digits = 2)
table_ptype$X2017perc =  round(table_ptype$X2017perc, digits = 2)
table_ptype$X2018perc = round(table_ptype$X2018perc, digits = 2)
table_ptype$Total_perc = round(table_ptype$Total_perc, digits = 2)

table_ptype$X2015 = paste('£',formatC(table_ptype$X2015, big.mark=',', format = 'f', digits = 2))
table_ptype$X2016 = paste('£',formatC(table_ptype$X2016, big.mark=',', format = 'f', digits = 2))
table_ptype$X2017 = paste('£',formatC(table_ptype$X2017, big.mark=',', format = 'f', digits = 2))
table_ptype$X2018 = paste('£',formatC(table_ptype$X2018, big.mark=',', format = 'f', digits = 2))
table_ptype$Total = paste('£',formatC(table_ptype$Total, big.mark=',', format = 'f', digits = 2))

table_ptype$X2015perc_final <- paste(table_ptype$X2015,"(",table_ptype$X2015perc,")")
table_ptype$X2016perc_final <- paste(table_ptype$X2016,"(",table_ptype$X2016perc,")")
table_ptype$X2017perc_final <- paste(table_ptype$X2017,"(",table_ptype$X2017perc,")")
table_ptype$X2018perc_final <- paste(table_ptype$X2018,"(",table_ptype$X2018perc,")")
table_ptype$Total_perc_final <- paste(table_ptype$Total,"(",table_ptype$Total_perc,")")

table_ptype = select(table_ptype, -c("X2015", "X2016", "X2017", "X2018", "Total", "X2018", "X2015perc","X2016perc", "X2017perc", "X2018perc", "Total_perc"))



table_ptype = table_ptype %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

length(trust_2015$final_tov)





###### Number of regions per top company per year

table(top_company$Region)
top_company$Region[top_company$Region == "East England"] = "East of England"

top_company_2015 = top_company[which(top_company$year == 2015), ]
top_company_2016 = top_company[which(top_company$year == 2016), ]
top_company_2017 = top_company[which(top_company$year == 2017), ]
top_company_2018 = top_company[which(top_company$year == 2018), ]


table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2016"=character(0), "2017"=character(0),"2018"=character(0),Total=character(0))


b<-c("AbbVie Limited","AstraZeneca","Bayer Plc","Biogen Idec Ltd","Gilead","Janssen-Cilag Ltd","Novartis Pharmaceuticals UK Ltd","Pfizer Ltd","Roche Products Limited","Sanofi Aventis")

seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-top_company_2015%>%filter(company %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- length(unique(seged_2$Region))
  rm(seged_2)
  
}

table_ptype$X2015 = as.numeric(table_ptype$X2015)



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-top_company_2016%>%filter(company %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- length(unique(seged_2$Region))
  rm(seged_2)
  
}

table_ptype$X2016 = as.numeric(table_ptype$X2016)

table_ptype = table_ptype[-(11:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-top_company_2017%>%filter(company %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- length(unique(seged_2$Region))
  rm(seged_2)
  
}

table_ptype$X2017 = as.numeric(table_ptype$X2017)

table_ptype = table_ptype[-(11:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-top_company_2018%>%filter(company %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- length(unique(seged_2$Region))
  rm(seged_2)
  
}

table_ptype$X2018 = as.numeric(table_ptype$X2018)

table_ptype = table_ptype[-(11:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-top_company%>%filter(company %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- length(unique(seged_2$Region))
  rm(seged_2)
  
}

table_ptype$Total = as.numeric(table_ptype$Total)

table_ptype = table_ptype[-(11:20),]




##### Table for the most frequent region per top company



table_ptype<-data.frame("Payment_value" = character(0),"2015"=character(0), "2016"=character(0), "2017"=character(0),"2018"=character(0),Total=character(0))


b<-c("AbbVie Limited","AstraZeneca","Bayer Plc","Biogen Idec Ltd","Gilead","Janssen-Cilag Ltd","Novartis Pharmaceuticals UK Ltd","Pfizer Ltd","Roche Products Limited","Sanofi Aventis")

seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-top_company_2015%>%filter(company %in% b[i])
  table_ptype$"X2015"[table_ptype$Payment_value==b[i]]<- names(which.max(table(seged_2$Region)))
  rm(seged_2)
  
}





for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-top_company_2016%>%filter(company %in% b[i])
  table_ptype$"X2016"[table_ptype$Payment_value==b[i]]<- names(which.max(table(seged_2$Region)))
  rm(seged_2)
  
}



table_ptype = table_ptype[-(11:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-top_company_2017%>%filter(company %in% b[i])
  table_ptype$"X2017"[table_ptype$Payment_value==b[i]]<- names(which.max(table(seged_2$Region)))
  rm(seged_2)
  
}



table_ptype = table_ptype[-(11:20),]



for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-top_company_2018%>%filter(company %in% b[i])
  table_ptype$"X2018"[table_ptype$Payment_value==b[i]]<- names(which.max(table(seged_2$Region)))
  rm(seged_2)
  
}



table_ptype = table_ptype[-(11:20),]


for(i in 1:length(b)){
  
  seged_1<-data.frame("Payment_value"=b[i],"2015"=0,"2016"=0, "2017"=0,"2018"=0,Total=0)
  table_ptype<-rbind(table_ptype,seged_1)
  rm(seged_1)
  seged_2<-top_company%>%filter(company %in% b[i])
  table_ptype$Total[table_ptype$Payment_value==b[i]]<- names(which.max(table(seged_2$Region)))
  rm(seged_2)
  
}



table_ptype = table_ptype[-(11:20),]



##### Company sum per year

company_sumtov = aggregate(final_tov ~ company + year, data=trust_data, FUN=sum)
company_sumtov = transform(company_sumtov, freq.loc = ave(seq(nrow(company_sumtov)), company, FUN=length))

company_sumtov = company_sumtov[which(company_sumtov$freq.loc == 4),]
company_sumtov = unique(company_sumtov$company)
company_sumtov = as.data.frame(company_sumtov)


#### Trust sum per year

trust_sumtov2 = aggregate(final_tov ~ trust + year, data=trust_data, FUN=sum)
trust_sumtov2 = transform(trust_sumtov2, freq.loc = ave(seq(nrow(trust_sumtov2)), trust, FUN=length))

trust_sumtov2 = trust_sumtov2[which(trust_sumtov2$freq.loc == 4),]
trust_sumtov2 = unique(trust_sumtov2$trust)
trust_sumtov2 = as.data.frame(trust_sumtov2)

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
############# PANEL DATA ANALYSIS ###########################################################################################
#####################################################################################################################

### SOURCE: https://www.princeton.edu/~otorres/Panel101R.pdf

library(readxl)
trust_data = read_excel(file.choose())

# sum tov per trust per year

trust_sumtov = aggregate(final_tov ~ trust, data=trust_data, FUN=sum)

# add trust characteristics to the sum data
trust_vars = select(trust_data, trust, Region, trust_type, foundation_trust)

# choose most frequent region for each trust (because in the case of some trust, they have more than one regions)
trust_vars = trust_vars %>% 
  group_by(trust) %>% 
  count(Region) %>%
  slice(which.max(n))

trust_vars = select(trust_vars, -c(n))

trust_type = read_excel(file.choose())
trust_type = rename(trust_type, trust = trust_name)

trust_vars = merge(trust_vars, trust_type, by = "trust")

# merge trust characteristics with sum data

trust_sumtov = merge(trust_sumtov, trust_vars, by = "trust")

###### Exploring panel data #########



aggregate(final_tov ~ trust, data=trust_data, FUN=sum)

length(trust_2015$trust)

length(unique(trust_data$trust))




###### Add rows with zero for trusts 

trust = select(trust_data, trust, year)
trust = unique(trust)
trust<-trust%>%group_by(trust)%>%mutate(count=n())

trust = trust[which(trust$count<4), ]

trust2 = select(trust, trust)
trust2 = unique(trust2)

trust2_2015 = trust2
trust2_2015$year = "2015"
trust2_2016 = trust2
trust2_2016$year = "2016"
trust2_2017 = trust2
trust2_2017$year = "2017"
trust2_2018 = trust2
trust2_2018$year = "2018"

trust2 = rbind(trust2_2015,trust2_2016,trust2_2017,trust2_2018)
trust2$year = as.numeric(trust2$year)

trust = left_join(trust2, trust, by = c("trust", "year"))

trust = trust[which(is.na(trust$count)), ]

trust = left_join(trust, trust_variables, by = "trust")

trust_data_panel = select(trust_data, 2,4,7,15:17)
trust$count = NULL
trust$final_tov = 0

trust_data_panel = trust_data_panel[, c(1,2,5,6,3,4)]
trust_data_panel = rbind(trust_data_panel, trust)



trust_data_panel_sum = aggregate(final_tov ~ trust + year, data=trust_data_panel, FUN=sum)
trust_data_panel_sum = left_join(trust_data_panel_sum, trust_variables, by = "trust")


#### Not all standardised postcodes are the same throughout the dataframe, select the most frequent one for each trust
trust_data = rename(trust_data, standardised_postcode = 'Standardised trust postcode')

library(readxl)
standardised_postcodes = trust_data %>% 
  count(trust, standardised_postcode) %>%
  slice(which.max(standardised_postcode))

standardised_postcodes = trust_data %>% group_by(trust) %>% count(standardised_postcode) %>% top_n(1) # includes ties

library(tidyverse)
standardised_postcodes = trust_data %>%
  # add a column n with count by categories
  add_count(trust, standardised_postcode) %>%
  # select max or first occurrence by patient
  group_by(trust) %>%
  # keep only first TRUE
  mutate(postcode = standardised_postcode[n == max(n)][1]) %>%
  # do not keep temp var
  select(-n)

standardised_postcodes$Prefix = substr(standardised_postcodes$standardised_postcode, start = 1, stop = 2)
standardised_postcodes$Prefix = sub("^([[:alpha:]]*).*", "\\1", standardised_postcodes$Prefix)


standardised_postcodes = left_join(standardised_postcodes, prefix, by = "Prefix")
standardised_postcodes$Region.x = NULL
standardised_postcodes$Postcode = NULL
standardised_postcodes = rename(standardised_postcodes, Region = Region.y)

standardised_postcodes$Region[standardised_postcodes$Region == "Wales"] = "West Midlands"
standardised_postcodes = select(standardised_postcodes, trust, Region)

trust_variables = select(trust_data, trust, trust_type, foundation_trust)
trust_variables = unique(trust_variables)

trust_variables = merge(trust_variables, standardised_postcodes, by = "trust")

trust_sumtov = merge(trust_sumtov, trust_variables, by = "trust")



###### Random-effects model

library(tidyverse) # Modern data science library 
library(plm)       # Panel data analysis library
library(car)       # Companion to applied regression 
library(gplots)    # Various programing tools for plotting data
library(tseries)   # For timeseries analysis
library(lmtest)    # For hetoroskedasticity analysis


trust_data_panel_sum <- transform(trust_data_panel_sum, id=match(trust, unique(trust)))

trust_data_panel_sum <- pdata.frame(trust_data_panel_sum, index=c("id","year"))

trust_data_panel_sum$year = as.factor(trust_data_panel_sum$year)
trust_data_panel_sum$trust_type = as.factor(trust_data_panel_sum$trust_type)
trust_data_panel_sum$foundation_trust = as.factor(trust_data_panel_sum$foundation_trust)
trust_data_panel_sum$Region = as.factor(trust_data_panel_sum$Region)

trust_data_panel_sum <- within(trust_data_panel_sum, trust_type <- relevel(trust_type, ref = 1))
trust_data_panel_sum <- within(trust_data_panel_sum, year <- relevel(year, ref = 2015))
trust_data_panel_sum <- within(trust_data_panel_sum, foundation_trust <- relevel(foundation_trust, ref = 0))
trust_data_panel_sum <- within(trust_data_panel_sum, Region <- relevel(Region, ref = "Greater London"))

scatterplot(final_tov~year|trust, data=trust_data_panel_sum)

plotmeans(final_tov ~ trust, data = trust_data_panel_sum)
plotmeans(final_tov ~ year, data = trust_data_panel_sum)

# OLS
ols1 <-plm(final_tov_log ~ trust_type, index = c("id", "year"), data = trust_data_panel_sum, model = "pooling")
summary(ols1)

ols2 <-plm(final_tov_log ~ trust_type + foundation_trust, index = c("id", "year"), data = trust_data_panel_sum, model = "pooling")
summary(ols2)

ols3 <-plm(final_tov_log ~ trust_type + foundation_trust + Region, index = c("id", "year"), data = trust_data_panel_sum, model = "pooling")
summary(ols3)

ols4 <-plm(final_tov_log ~ trust_type + foundation_trust + Region + year, index = c("id", "year"), data = trust_data_panel_sum, model = "pooling")
summary(ols4)






### Random-effects
random1 <- plm(final_tov_log ~ trust_type, index = c("id", "year"), data=trust_data_panel_sum, model="random", random.method="walhus")
summary(random1)

random2 <- plm(final_tov_log ~ trust_type + foundation_trust, index = c("id", "year"), data=trust_data_panel_sum, model="random", random.method="walhus")
summary(random2)

random3 <- plm(final_tov_log ~ trust_type + foundation_trust + Region, index = c("id", "year"), data=trust_data_panel_sum, model="random", random.method="walhus")
summary(random3)

random4 <- plm(final_tov_log ~ trust_type + foundation_trust + Region + year, index = c("id", "year"), data=trust_data_panel_sum, model="random", random.method="walhus")
summary(random4)


(exp(coef(random4)["trust_type2"]) - 1) * 100

(exp(coef(random4)["trust_type3"]) - 1) * 100

(exp(coef(random4)["trust_type4"]) - 1) * 100

(exp(coef(random4)["trust_type5"]) - 1) * 100

(exp(coef(random4)["RegionEast England"]) - 1) * 100

(exp(coef(random4)["year2018"]) - 1) * 100

trust_data_panel_sum$final_tov_log = log(trust_data_panel_sum$final_tov)
trust_data_panel_sum$final_tov_log[trust_data_panel_sum$final_tov_log == "-Inf"] = 0

# Breusch and Pagan test, reference: https://www.jstor.org/stable/1911963#metadata_info_tab_contents
plmtest(ols4, type=c("bp"))

# Hausman test
phtest(fixed, random)


library(stargazer)

stargazer(ols1, ols2, ols3, ols4, random1, random2, random3, random4, title="Regression Results", align=TRUE, dep.var.labels=c("sum payment per trust"),  omit.stat=c("LL","ser"), no.space=TRUE, out = "regoutput.txt", type = "text")






###### Trust data and explanatory model on the number of payments

trust_data_number = aggregate(final_tov ~ trust + year, data=trust_data, FUN=length)

trust_data_number = merge(trust_data_number, trust_variables, by = "trust")

trust_data_number = rename(trust_data_number, payment_number = final_tov)


###### Add rows with zeros for trusts 

trust = select(trust_data, trust, year)
trust = unique(trust)
trust<-trust%>%group_by(trust)%>%mutate(count=n())

trust = trust[which(trust$count<4), ]

trust2 = select(trust, trust)
trust2 = unique(trust2)

trust2_2015 = trust2
trust2_2015$year = "2015"
trust2_2016 = trust2
trust2_2016$year = "2016"
trust2_2017 = trust2
trust2_2017$year = "2017"
trust2_2018 = trust2
trust2_2018$year = "2018"

trust2 = rbind(trust2_2015,trust2_2016,trust2_2017,trust2_2018)
trust2$year = as.numeric(trust2$year)

trust = left_join(trust2, trust, by = c("trust", "year"))

trust = trust[which(is.na(trust$count)), ]

trust = left_join(trust, trust_variables, by = "trust")

trust$count = NULL
trust$final_tov = 0

trust_data_number = trust_data_number[, c(1,2,4,5,6,3)]
trust = rename(trust, payment_number = final_tov)

trust_data_number = rbind(trust_data_number, trust)




trust_data_number <- transform(trust_data_number, id=match(trust, unique(trust)))

trust_data_number <- pdata.frame(trust_data_number, index=c("id","year"))

trust_data_number$year = as.factor(trust_data_number$year)
trust_data_number$trust_type = as.factor(trust_data_number$trust_type)
trust_data_number$foundation_trust = as.factor(trust_data_number$foundation_trust)
trust_data_number$Region = as.factor(trust_data_number$Region)

trust_data_number <- within(trust_data_number, trust_type <- relevel(trust_type, ref = 1))
trust_data_number <- within(trust_data_number, year <- relevel(year, ref = 2015))
trust_data_number <- within(trust_data_number, foundation_trust <- relevel(foundation_trust, ref = 0))
trust_data_number <- within(trust_data_number, Region <- relevel(Region, ref = "Greater London"))

scatterplot(payment_number~year|trust, data=trust_data_number)

plotmeans(payment_number ~ trust, data = trust_data_number)
plotmeans(payment_number ~ year, data = trust_data_number)

trust_data_number$payment_number_log = log(trust_data_number$payment_number)

trust_data_number$payment_number_log[trust_data_number$payment_number_log == "-Inf"] = 0

# OLS
ols1 <-plm(payment_number_log ~ trust_type, index = c("id", "year"), data = trust_data_number, model = "pooling")
summary(ols1)

ols2 <-plm(payment_number_log ~ trust_type + foundation_trust, index = c("id", "year"), data = trust_data_number, model = "pooling")
summary(ols2)

ols3 <-plm(payment_number_log ~ trust_type + foundation_trust + Region, index = c("id", "year"), data = trust_data_number, model = "pooling")
summary(ols3)

ols4 <-plm(payment_number_log ~ trust_type + foundation_trust + Region + year, index = c("id", "year"), data = trust_data_number, model = "pooling")
summary(ols4)






### Random-effects
random1 <- plm(payment_number_log ~ trust_type, index = c("id", "year"), data=trust_data_number, model="random", random.method="walhus")
summary(random1)

random2 <- plm(payment_number_log ~ trust_type + foundation_trust, index = c("id", "year"), data=trust_data_number, model="random", random.method="walhus")
summary(random2)

random3 <- plm(payment_number_log ~ trust_type + foundation_trust + Region, index = c("id", "year"), data=trust_data_number, model="random", random.method="walhus")
summary(random3)

random4 <- plm(payment_number_log ~ trust_type + foundation_trust + Region + year, index = c("id", "year"), data=trust_data_number, model="random", random.method="walhus")
summary(random4)


(exp(coef(random4)["trust_type2"]) - 1) * 100

(exp(coef(random4)["trust_type3"]) - 1) * 100

(exp(coef(random4)["trust_type4"]) - 1) * 100

(exp(coef(random4)["trust_type5"]) - 1) * 100

(exp(coef(random4)["trust_type5"]) - 1) * 100

(exp(coef(random4)["RegionSouth West"]) - 1) * 100

(exp(coef(random4)["year2016"]) - 1) * 100

trust_data_panel_sum$final_tov_log = log(trust_data_panel_sum$final_tov)
trust_data_panel_sum$final_tov_log[trust_data_panel_sum$final_tov_log == "-Inf"] = 0

# Breusch and Pagan test, reference: https://www.jstor.org/stable/1911963#metadata_info_tab_contents
plmtest(ols4, type=c("bp"))

# Hausman test
phtest(fixed, random)


library(stargazer)

stargazer(ols1, ols2, ols3, ols4, random1, random2, random3, random4, title="Regression Results", align=TRUE, dep.var.labels=c("sum payment number per trust"),  omit.stat=c("LL","ser"), no.space=TRUE, out = "regoutput.txt", type = "text")


hist(trust_sumtov$final_tov,
     xlim = c(0,1000000),
     breaks = 100,
     xlab = "final tov",
     main = "Histogram of sum payment per trust per year")

summary(trust_sumtov$final_tov)

