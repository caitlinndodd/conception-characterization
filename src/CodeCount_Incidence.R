incidence<-read.csv(file="../data/raw/Incidence.csv", header = T, sep = ',') #read in csv
code_count<-read.csv(file="../data/raw/CodeCount.csv", header = T, sep = ',')

#Write aggregate function here with parameters for aggregation variables 
#Concatenate into one dataframe
year_agg<-aggregate(cbind(N_EVENTS, PERSON_YEARS)~EVENT+YEAR, incidence, sum)
sex_agg<-aggregate(cbind(N_EVENTS, PERSON_YEARS)~EVENT+SEX, incidence, sum)
age_agg<-aggregate(cbind(N_EVENTS, PERSON_YEARS)~EVENT+AGE, incidence, sum)
year_sex_agg<-aggregate(cbind(N_EVENTS, PERSON_YEARS)~EVENT+YEAR+SEX, incidence, sum)
year_age_agg<-aggregate(cbind(N_EVENTS, PERSON_YEARS)~EVENT+YEAR+AGE, incidence, sum)
sex_age_agg<-aggregate(cbind(N_EVENTS, PERSON_YEARS)~EVENT+SEX+AGE, incidence, sum)

library (data.table)

graph<-rbindlist(list(incidence, year_agg, sex_agg, age_agg, year_sex_agg, year_age_agg, sex_age_agg), fill = TRUE)
pneumonia$Age[is.na(pneumonia$Age)] <- pneumonia$AgeGroup.text[is.na(pneumonia$Age)] #set NAs in Age variable equal to AgeGroup.text

pneumo_agg <- aggregate(cbind(PersonYears, Events) ~ Age, pneumonia, sum) #Aggregate over Age
pneumo_agg$incidence<-100000*(pneumo_agg$Events/pneumo_agg$PersonYears) #Calculate incidence per 1000PY

library(ggplot2) #Load library ggplot2

ggplot(pneumo_agg, aes(Age, incidence, group = 1)) + #create plot
  geom_line() +
  labs(x = "Age Group", y = "Incidence per 100,000 PY", 
       title = "Incidence of Pneumonia, ARS")


zero_four<- pneumonia$AgeGroup.text %in% c("0" , "1" , "2" , "3", "4") #create age groups in children
pneumonia$Age[zero_four] <- "00-04"
five_nine<- pneumonia$AgeGroup.text %in% c("5" , "6" , "7" , "8", "9")
pneumonia$Age[five_nine] <- "05-09"
ten_seventeen <- pneumonia$AgeGroup.text %in% c("10" , "11" , "12" , "13", "14", "15", "16", "17")
pneumonia$Age[ten_seventeen] <- "10-17"