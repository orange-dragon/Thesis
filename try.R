


#########          
#########   1  response index
#########

library(dslabs)
library(dplyr)
library(lubridate)
library(ggplot2)#高级绘图包

# data download from https://ourworldindata.org/grapher/covid-stringency-index

data<-read.csv("covid-stringency-index.csv")    
data$Entity
colnames(data)[4] <- 'Stringency_Index'  

Latin <- c("Brazil","Argentina","Colombia","Mexico","Peru","Chile") 
data1 =data[which(data $ Entity %in% Latin),] 
rownames(data1) <- NULL  
write.csv(data1, file="Latin.csv") 

Latin1 <-read.csv("Latin.csv",header=T) 

#######  1.1 boxplot and Violin Plot

## Violin Plot 
p1 = ggplot(Latin1,aes(x =Entity, y = Stringency_Index,fill = Entity)) +geom_violin()
print(p1)

## Boxplot 
p2 = ggplot(Latin1,aes(x = Entity, y = Stringency_Index,fill = Entity)) +geom_boxplot()+
  theme(axis.title.x  = element_text(face = 'italic'),
        axis.text.x =  element_text(angle = 45 , vjust = 0.5))
print(p2)

## Density Plot                            
p3 = ggplot(Latin1,aes(Stringency_Index,col=Entity)) +geom_density() + 
  facet_wrap(~Entity,nrow = 2)  #nrow是显示行数
print(p3)

####### 1.2  Nonpharmaceutical Intervention Actions

### "Brazil","Argentina","Colombia","Mexico","Peru","Chile"  

Brazil =data1[which(data1 $ Entity=="Brazil"),] 
rownames(Brazil) <- NULL  
colnames(Brazil)[4] <- 'Brazil_Index' 
Brazil = Brazil %>% select(Day,Brazil_Index)
write.csv(data1, file="Brazil.csv") 

Argentina =data1[which(data1 $ Entity=="Argentina"),] 
rownames(Argentina) <- NULL  
colnames(Argentina)[4] <- 'Argentina_Index' 
Argentina = Argentina %>% select(Day,Argentina_Index) 

Colombia =data1[which(data1 $ Entity=="Colombia"),] 
rownames(Colombia) <- NULL  
colnames(Colombia)[4] <- 'Colombia_Index' 
Colombia = Colombia %>% select(Day,Colombia_Index) 

Mexico =data1[which(data1 $ Entity=="Mexico"),] 
rownames(Mexico) <- NULL  
colnames(Mexico)[4] <- 'Mexico_Index' 
Mexico = Mexico %>% select(Day,Mexico_Index) 

Peru =data1[which(data1 $ Entity=="Peru"),] 
rownames(Peru) <- NULL  
colnames(Peru)[4] <- 'Peru_Index' 
Peru = Peru %>% select(Day,Peru_Index) 

Chile =data1[which(data1 $ Entity=="Chile"),] 
rownames(Chile) <- NULL  
colnames(Chile)[4] <- 'Chile_Index' 
Chile = Chile %>% select(Day,Chile_Index) 


# Define a function-multimerge
multimerge<-function(dat=list(),...){
  if(length(dat)<2)return(as.data.frame(dat))
  mergedat<-dat[[1]]
  dat[[1]]<-NULL
  for(i in dat){
    mergedat<-merge(mergedat,i,...,all=T)   
  }
  return(mergedat)
}

Latin6<- multimerge(list(Brazil,Argentina,Colombia,Mexico,Peru,Chile))             

library(lubridate)
Latin6$Day <- lubridate::ymd(Latin6$Day)

write.csv(Latin6, file="Latin_Index.csv")  

library(ggplot2)

vd = rbind(data.frame(v=Latin6$Day, y=Latin6$Brazil_Index, type=as.factor(1)), 
           data.frame(v=Latin6$Day, y=Latin6$Argentina_Index, type=as.factor(2)),
           data.frame(v=Latin6$Day, y=Latin6$Colombia_Index, type=as.factor(3)),
           data.frame(v=Latin6$Day, y=Latin6$Mexico_Index, type=as.factor(4)),
           data.frame(v=Latin6$Day, y=Latin6$Peru_Index, type=as.factor(5)),
           data.frame(v=Latin6$Day, y=Latin6$Chile_Index, type=as.factor(6)))

ggplot(vd,aes(x=v,y=y,shape=type,color=type,group=type))+geom_point()+labs(title="Covid-19 Response Stringency Index")+xlab("Date")+ylab("Response Stringency Index")+scale_shape_manual(values=c(1,4,5,7,9,11))+scale_color_manual(values=c(1:6))+theme(plot.title=element_text(hjust=0.5))+geom_line()
dev.off()
###The two expressions are similar
ggplot(vd,aes(x=v,y=y,shape=type,color=type,group=type))+labs(title="Covid-19 Response Stringency Index")+xlab("Date")+ylab("Response Stringency Index")+scale_shape_manual(values=c(1,4,5,7,9,11))+scale_color_manual(values=c(1:6))+theme(plot.title=element_text(hjust=0.5))+geom_line(size=1.0,shape=4)
dev.off()



#########          
#########   2  Confirmed cases per million people
#########


# The data download from https://github.com/owid/covid-19-data/tree/master/public/data/owid-covid-data.csv
data<-read.csv("owid-covid-data.csv")   
data$location 
colnames(data)[10] <- 'Cases_per_million'  

Latin <- c("Brazil","Argentina","Colombia","Mexico","Peru","Chile") 

data1 =data[which(data$location %in% Latin),]  
rownames(data1) <- NULL  
write.csv(data1, file="Latin_permillon.csv") 
Latin_permillon1= data1

#######  2.1 Violin Plot and Boxplot

## Violin Plot 
p1 = ggplot(Latin_permillon1,aes(x =location, y = Cases_per_million,fill = location)) +geom_violin()
print(p1)

## Boxplot 
p2 = ggplot(Latin_permillon1,aes(x = location, y = Cases_per_million,fill =location)) +geom_boxplot()+
  theme(axis.title.x  = element_text(face = 'italic'),
        axis.text.x =  element_text(angle = 45 , vjust = 0.5))
print(p2)

## Density Plot 
p3 = ggplot(Latin_permillon1,aes(Cases_per_million,col=location)) +geom_density() + 
  facet_wrap(~location,nrow = 2)  
print(p3)

#######  2.2  The line chart

library(dplyr)

### "Brazil","Argentina","Colombia","Mexico","Peru","Chile" 
data<-read.csv("owid-covid-data.csv")   
colnames(data)[2] <- 'Entity'   
colnames(data)[3] <- 'Date' 

Brazil = filter(data,data$Entity=="Brazil")  
colnames(Brazil)[11] <- 'Brazil_Index'       
Brazil = Brazil %>% select(Date,Brazil_Index)   

Argentina = filter(data,data$Entity=="Argentina")
colnames(Argentina)[11] <- 'Argentina_index' 
Argentina = Argentina %>% select(Date,Argentina_index) 

Colombia = filter(data,data$Entity=="Colombia")
colnames(Colombia)[11] <- 'Colombia_index' 
Colombia = Colombia %>% select(Date,Colombia_index) 

Mexico = filter(data,data$Entity=="Mexico")
colnames(Mexico)[11] <- 'Mexico_Index' 
Mexico = Mexico %>% select(Date,Mexico_Index) 

Peru = filter(data,data$Entity=="Peru")
colnames(Peru)[11] <- 'Peru_Index' 
Peru = Peru %>% select(Date,Peru_Index) 

Chile = filter(data,data$Entity=="Chile")
colnames(Chile)[11] <- 'Chile_Index' 
Chile = Chile %>% select(Date,Chile_Index) 


# multimerge
multimerge<-function(dat=list(),...){
  if(length(dat)<2)return(as.data.frame(dat))
  mergedat<-dat[[1]]
  dat[[1]]<-NULL
  for(i in dat){
    mergedat<-merge(mergedat,i,...,all=T)   
  }
  return(mergedat)
}

Latin_permillon6<- multimerge(list(Brazil,Argentina,Colombia,Mexico,Peru,Chile))             
write.csv(Latin_permillon6, file="Latin_permillon6.csv") 

library(lubridate)
Latin_permillon6$Date <- lubridate::ymd(Latin_permillon6$Date)


library(ggplot2)
vd = rbind(data.frame(v=Latin_permillon6$Date, y=Latin_permillon6$Brazil_Index, type=as.factor(1)), 
           data.frame(v=Latin_permillon6$Date, y=Latin_permillon6$Argentina_index, type=as.factor(2)),
           data.frame(v=Latin_permillon6$Date, y=Latin_permillon6$Colombia_index, type=as.factor(3)), 
           data.frame(v=Latin_permillon6$Date, y=Latin_permillon6$Mexico_Index, type=as.factor(4)), 
           data.frame(v=Latin_permillon6$Date, y=Latin_permillon6$Peru_Index, type=as.factor(5)),
           data.frame(v=Latin_permillon6$Date, y=Latin_permillon6$Chile_Index, type=as.factor(6)))

ggplot(vd,aes(x=v,y=y,shape=type,color=type,group=type))+geom_point()+labs(title="Total confirmed cases of COVID-19 per million people ")+xlab("Date")+ylab("Total confirmed cases per million")+scale_shape_manual(values=c(1,4,5,7,9,11))+scale_color_manual(values=c(1:6))+theme(plot.title=element_text(hjust=0.5))+geom_line()

ggplot(vd,aes(x=v,y=y,shape=type,color=type,group=type))+labs(title="Total confirmed cases of COVID-19 per million people")+xlab("Date")+ylab("Total confirmed cases per million")+scale_shape_manual(values=c(1,4,5,7,9,11))+scale_color_manual(values=c(1:6))+theme(plot.title=element_text(hjust=0.5))+geom_line(size=0.5,shape=4)
dev.off()



#########          
#########   3  response index and the number of confirmed   #########

library(dslabs)
library(dplyr)
library(lubridate)
library(ggplot2)

data<-read.csv("owid-covid-data.csv")    

Latin <- c("Brazil","Argentina","Colombia","Mexico","Peru","Chile") 

data1 =data[which(data$location %in% Latin),] 
rownames(data1) <- NULL 
data1 = data1 %>% select(location,date,total_cases) 
colnames(data1)[1] <- 'Entity' 
colnames(data1)[2] <- 'Date' 
write.csv(data1, file="Latin_totalcase.csv") 
Latin_totalcase1 <- data1

#######  3.1 Violin Plot and Boxplot for Total number of confirmed cases

## Violin Plot 
p1 = ggplot(Latin_totalcase1,aes(x =Entity, y = total_cases,fill = Entity)) +geom_violin()
print(p1)

## Boxplot 
p2 = ggplot(Latin_totalcase1,aes(x = Entity, y = total_cases,fill = Entity)) +geom_boxplot()+
  theme(axis.title.x  = element_text(face = 'italic'),
        axis.text.x =  element_text(angle = 45 , vjust = 0.5))
print(p2)

## Density Plot                            
p3 = ggplot(Latin_totalcase1,aes(total_cases,col=Entity)) +geom_density() + 
  facet_wrap(~Entity,nrow = 2) 
print(p3)

####### 3.2  Total number of confirmed cases

library(dplyr)
data <- Latin_totalcase1
### "Brazil","Argentina","Colombia","Mexico","Peru","Chile"

Brazil = filter(data,data$Entity=="Brazil")
colnames(Brazil)[3] <- 'Brazil_total' 
Brazil = Brazil %>% select(Date,Brazil_total) 

Argentina = filter(data,data$Entity=="Argentina")
colnames(Argentina)[3] <- 'Argentina_total' 
Argentina = Argentina %>% select(Date,Argentina_total) 

Colombia = filter(data,data$Entity=="Colombia")
colnames(Colombia)[3] <- 'Colombia_total' 
Colombia = Colombia %>% select(Date,Colombia_total) 

Mexico = filter(data,data$Entity=="Mexico")
colnames(Mexico)[3] <- 'Mexico_total' 
Mexico = Mexico %>% select(Date,Mexico_total) 

Peru = filter(data,data$Entity=="Peru")
colnames(Peru)[3] <- 'Peru_total' 
Peru = Peru %>% select(Date,Peru_total) 

Chile = filter(data,data$Entity=="Chile")
colnames(Chile)[3] <- 'Chile_total' 
Chile = Chile %>% select(Date,Chile_total) 

# multimerge
multimerge<-function(dat=list(),...){
  if(length(dat)<2)return(as.data.frame(dat))
  mergedat<-dat[[1]]
  dat[[1]]<-NULL
  for(i in dat){
    mergedat<-merge(mergedat,i,...,all=T)   
  }
  return(mergedat)
}

Latin_totalcase6<- multimerge(list(Brazil,Argentina,Colombia,Mexico,Peru,Chile))             
write.csv(Latin_totalcase6, file="Latin_totalcase6.csv") 


####### 3.3   relationship between government response index and the number of confirmed cases


Latin_Index <- read.csv("Latin_Index.csv")
##Remove the first column
Latin_Index<- Latin_Index[,-1]
Latin_Index_mean <- apply(Latin_Index[1:799,2:7], 2, mean,na.rm=T)  
Latin_Index_mean   
#Brazil_Index Argentina_Index  Colombia_Index    Mexico_Index  Peru_Index     Chile_Index 
#60.73118        68.07453        63.32868        53.40246       70.37180        62.71433 


Latin_Index_Median <- apply(Latin_Index[1:799,2:7], 2, median,na.rm=T)  
Latin_Index_Median

#Brazil_Index Argentina_Index  Colombia_Index    Mexico_Index  Peru_Index     Chile_Index 
#61.570          77.310          61.575          60.650         74.070          78.240

class(Latin_Index_Median)

Index_mean = matrix(Latin_Index_mean)                       
Index_Median = matrix(Latin_Index_Median)                    

Entity = c("Brazil","Argentina","Colombia","Mexico","Peru","Chile")

Latin_Total = Latin_totalcase6
Total_cases = Latin_Total[799,]
Total_cases = Total_cases[,-1] 
class(Total_cases)
Total_cases
#Brazil_total Argentina_total Colombia_total Mexico_total
#799     29532810         8990413        6079231      5624954
#Peru_total Chile_total
#799    3539400     3353259
colnames(Total_cases) <- NULL
rownames(Total_cases) <- NULL
#unlist(Total_cases)
Total_cases
Total_cases <- t(Total_cases)

Casepermillion = Latin_permillon6 
Casepermillion = Casepermillion[799,]
Casepermillion = Casepermillion[,-1] 
class(Casepermillion)
Casepermillion
colnames(Casepermillion) <- NULL
rownames(Casepermillion) <- NULL
#unlist(Total_cases)
Casepermillion
Casepermillion<- t(Casepermillion)
Casepermillion

lastday_index = Latin_Index[799,]
lastday_index = lastday_index[,-1] 
class(lastday_index)
lastday_index
colnames(lastday_index) <- NULL
rownames(lastday_index) <- NULL
#unlist(Total_cases)
lastday_index
lastday_index<- t(lastday_index)
lastday_index

Entity <- matrix(Entity)

mix= data.frame(Entity,Total_cases,Casepermillion,lastday_index,Index_mean,Index_Median)

class(mix)

write.csv(mix, file=" Latin_Index and Case.csv") 

#index and total number of confirmed cases
ggplot(mix,aes(x=mix$Total_cases,y=mix$lastday_index))+geom_point(stat="identity",fill="steelblue",colour="steelblue",width = 1.8,size=8.5)+labs(title="Relationship between Total confirmed cases of COVID-19 and Response Index")+xlab("Total confirmed cases of COVID-19")+ylab("Response Index")+theme(plot.title=element_text(hjust=0.5))+geom_text(aes(label = mix$Entity, vjust = -1.2, hjust = 0.0), show.legend = TRUE)

# Index_mean and total number of confirmed cases
ggplot(mix,aes(x=mix$Total_cases,y=mix$Index_mean))+geom_point(stat="identity",fill="green",colour="green",width = 1.8,size=8.5)+labs(title="Relationship between Total confirmed cases of COVID-19 and Response Index")+xlab("Total confirmed cases of COVID-19")+ylab("Response Index")+theme(plot.title=element_text(hjust=0.5))+geom_text(aes(label = mix$Entity, vjust = -0.9, hjust =  0.0), show.legend = TRUE)

# Index_median and total number of confirmed cases
ggplot(mix,aes(x=mix$Total_cases,y=mix$Index_Median))+geom_point(stat="identity",fill="purple",colour="purple",width = 1.8,size=8.5)+labs(title="Relationship between Total confirmed cases of COVID-19 and Response Index")+xlab("Total confirmed cases of COVID-19")+ylab("Response Index")+theme(plot.title=element_text(hjust=0.5))+geom_text(aes(label = mix$Entity, vjust = 1.8, hjust = 0.0), show.legend = TRUE)

# Index and total number of confirmed cases per million
ggplot(mix,aes(x=mix$Casepermillion,y=mix$lastday_index))+geom_point(stat="identity",fill="steelblue",colour="steelblue",width = 1.8,size=8.5)+labs(title="Relationship between the cases per million and Response Index")+xlab("Cases per million")+ylab("Response Index")+theme(plot.title=element_text(hjust=0.5))+geom_text(aes(label = mix$Entity, vjust = -1.2, hjust = 0.0), show.legend = TRUE)

# Index_mean and total number of confirmed cases per million
ggplot(mix,aes(x=mix$Casepermillion,y=mix$Index_mean))+geom_point(stat="identity",fill="green",colour="green",width = 1.8,size=8.5)+labs(title="Relationship between the cases per million and Response Index")+xlab("Cases per million")+ylab("Response Index")+theme(plot.title=element_text(hjust=0.5))+geom_text(aes(label = mix$Entity, vjust = -1.2, hjust =  0.0), show.legend = TRUE)

# Index_median and total number of confirmed cases per million
ggplot(mix,aes(x=mix$Casepermillion,y=mix$Index_Median))+geom_point(stat="identity",fill="purple",colour="purple",width = 1.8,size=8.5)+labs(title="Relationship between the cases per million and Response Index")+xlab("Cases per million")+ylab("Response Index")+theme(plot.title=element_text(hjust=0.5))+geom_text(aes(label = mix$Entity, vjust = -1.2, hjust =  0.0), show.legend = TRUE)

