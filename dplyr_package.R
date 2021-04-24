library(dplyr)
#'dplyr package###
data<- data.frame(Titanic)
data
#total passengers

num_passenger<-summarise(data,number_passengers=sum(Freq))
num_passenger
num_passenger1<-data%>%  #pipe operator to continue with the data
  summarise(number_passengers=sum(Freq))
num_passenger1

#group by 
num_passenger1<-data%>% 
  group_by(Class)%>%
  summarise(num_passenger1=sum(Freq))
num_passenger1

#select
data_sex_age_feq<-data%>% select(Sex,Age,Freq)
data_sex_age_feq

#mutate
data2<-data%>% mutate(Freq_mod=Freq*10)
data2
# filter

data_female<-data%>% filter(Sex=="Female")
data_female

#arrange
data_range<-data%>%arrange(desc(Freq))
data_range



library(ggplot2)
data_plt<-data%>% ggplot(aes(x=Sex,y=Freq, color=Class))+geom_line()
data_plt
