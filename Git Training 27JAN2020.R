incidence<-read.csv(file="./data/raw/Incidence.csv", header = T, sep = ',') #read in csv
code_count<-read.csv(file="./data/raw/CodeCount.csv", header = T, sep = ',')

EVENT_A<-incidence[which(incidence$EVENT=='EVENT_A'), ] #select for event a
EVENT_B<-incidence[which(incidence$EVENT=='EVENT_B'), ] #select for event b
EVENT_C<-incidence[which(incidence$EVENT=='EVENT_C'), ] #select for event c


zero_four<- pneumonia$AgeGroup.text %in% c("0" , "1" , "2" , "3", "4") #create age groups in children
pneumonia$Age[zero_four] <- "00-04"
five_nine<- pneumonia$AgeGroup.text %in% c("5" , "6" , "7" , "8", "9")
pneumonia$Age[five_nine] <- "05-09"
ten_seventeen <- pneumonia$AgeGroup.text %in% c("10" , "11" , "12" , "13", "14", "15", "16", "17")
pneumonia$Age[ten_seventeen] <- "10-17"

pneumonia$Age[is.na(pneumonia$Age)] <- pneumonia$AgeGroup.text[is.na(pneumonia$Age)] #set NAs in Age variable equal to AgeGroup.text

pneumo_agg <- aggregate(cbind(PersonYears, Events) ~ Age, pneumonia, sum) #Aggregate over Age
pneumo_agg$incidence<-100000*(pneumo_agg$Events/pneumo_agg$PersonYears) #Calculate incidence per 1000PY

library(ggplot2) #Load library ggplot2

ggplot(pneumo_agg, aes(Age, incidence, group = 1)) + #create plot
  geom_line() +
  labs(x = "Age Group", y = "Incidence per 100,000 PY", 
       title = "Incidence of Pneumonia, ARS")
