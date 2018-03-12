setwd("~/rproject/")
rf<-read.csv("Data_R_project.csv")
View(rf)
library(ggplot2)
library(ggmap)
library(Rcpp)
#http://www.unhcr.org/en-us/figures-at-a-glance.html
#http://www.unhcr.org/en-us/figures-at-a-glance.html
#http://www.unhcr.org/en-us/figures-at-a-glance.html
#http://www.unhcr.org/en-us/figures-at-a-glance.html
#http://www.unhcr.org/en-us/resettlement-in-the-united-states.html
#http://www.omaha.com/news/metro/a-welcoming-state-nebraska-led-the-nation-in-resettling-most/article_b84f8b71-d374-5cda-ad33-afa9a923fb54.html

#creating the data frame from above references 
df<-data.frame(Description=c("Displaced","Refugee","Stateless","Total_Resettled","US_Resettled","NE_Resettled"),Total=c(65000000,22500000,10000000,189300,84995,1441))

#Status of Refugees in 2016
ggplot(df,aes(Description,(Total), fill=Description))+geom_col()+
  geom_text(aes(label=Total),position=position_dodge(width=0.9), vjust=-0.25)+scale_x_discrete(name="Type of Refugees")+
  scale_y_continuous(name="Total in Millions")+ggtitle("The Refugee Data, 2016")

#copying the data frame 
df1<-df[,1:2]
#sorting the data new Data Farame 
df1$Description<-reorder(df1$Description,-df1$Total)
df1

#sorting the columns 

ggplot(df1,aes(Description,(Total), fill=Description))+geom_col()+
  geom_text(aes(label=" "),position=position_dodge(width=0.9), vjust=-0.25)+scale_x_discrete(name="Type of Refugees")+
  scale_y_continuous(name="Total in Million")+ggtitle("The Refugee Data, 2016")+
  theme(text = element_text(size=10),axis.text.x = element_text(angle=45, hjust=1))

#Taking the log of Y-Axis

ggplot(df,aes(Description,(Total), fill=Description))+geom_col()+
  geom_text(aes(label=Total),position=position_dodge(width=0.9), vjust=-0.25)+scale_x_discrete(name="Type of Refugees")+
  scale_y_log10()+ggtitle("The Refugee Data, 2016")+theme(plot.title = element_text(size = 14,face = "bold.italic"))+ylab("In Million")
# Sorting the columns  
ggplot(df1,aes(Description,(Total), fill=Description))+geom_col()+
  geom_text(aes(label=Total),position=position_dodge(width=0.9), vjust=-0.25)+scale_x_discrete(name="Type of Refugees")+
  scale_y_log10()+ggtitle("The Refugee Data, 2016")+theme(plot.title = element_text(size = 14,face = "bold.italic"))+ylab("In Million")

#http://www.unhcr.org/en-us/figures-at-a-glance.html
#creating a data frame from above reference site 

df3<-data.frame(Country=c("Pakistan","Turkey","Iran","Lebanon","Uganda","Ethiopia"),Refugee_No=c(1400000,2900000,979400,1000000,940800,791600))
# levels(df3$Country)
ggplot(df3,aes(df3$Country,df3$Refugee_No, fill=Country))+geom_col()+
  geom_text(aes(label=df3$Refugee_No),position=position_dodge(width=0.9), vjust=-0.25)+scale_x_discrete(name="Refugees Host Country")+
  scale_y_continuous(name="Total in Million")+ggtitle("Top 6 Refugees Host Countries, 2016")

#shorting the values in the data fram 
df3$Country<-reorder(df3$Country,-df3$Refugee_No)
# levels(df3$Country)
#Sorted graph 
ggplot(df3,aes(df3$Country,df3$Refugee_No, fill=Country))+geom_col()+
  scale_x_discrete(name="Refugees Host Country")+
  geom_text(aes(label=df3$Refugee_No),position=position_dodge(width=0.9), vjust=-0.25)+
  scale_y_continuous(name="Total in Million")+
  ggtitle("Top 6 Refugees Host Countries, 2016")+
  theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1))


ggplot(rf,aes(x=rf$Country.of.Origin, fill=rf$Country.of.Origin))+geom_bar()+
  ggtitle("New Immigrants Country of Origen")+
  ylab("Total Number of Arrival")+xlab("Country Name")+guides(fill=FALSE)+
  theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1))

ggplot(rf,aes(x=rf$Country.of.Origin, fill=Religion))+geom_bar()+
  ggtitle("New Immigrants Country of Origen & Religion")+
  ylab("Total Number of Arrival")+xlab("Country Name")+theme(text = element_text(size=7))+
  theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1))

# Gender Division 
tab<-table(rf$Sex)
d<-as.data.frame(tab)
ggplot(d,aes(x="",y=Freq,fill=Var1))+geom_bar(stat = "identity")+coord_polar("y",start = 0)+
  ggtitle("New Immigrants Gender Division ")+ylab(" ")+xlab("")+labs(fill="Key")+theme_bw()

#Plot of muslim vs other religion 

rf$rl<-as.numeric(rf$Religion=="Islam")

ggplot(rf,aes(rl))+geom_bar(aes(fill=Religion))+ggtitle("Muslim Immigrants vs Other Religion ")+
  theme(axis.text.x=element_blank())+xlab("")+ylab("")


#Syrian immigrant vs all other immigrants 
rf$sr<-as.numeric(rf$Country.of.Origin=="Syria")
tab1<-table(rf$sr)
s<-as.data.frame(tab1)
colnames(s)<- c("Country","Total")


ggplot(s,aes(x="",y=Total,fill=factor(Country,labels=c("Other","Syrian"))))+
  geom_bar(stat = "identity")+coord_polar("y",start = 0)+
  xlab("")+ylab("")+labs(fill="Key")+theme_bw()+ ggtitle("Syrian Immigrants vs all other ")

#employment Anaylysis

rf$emp<-!(as.numeric(rf$Employer==""))
View(rf)
re<-table(rf$Country.of.Origin,rf$Employer)
View(re)
#Employment graph based on country of origen

ggplot(rf,aes(rf$emp, fill=factor(rf$FT.PT,labels=c("Unemployed","Emp.Full Time","Emp.Part Time"))))+geom_bar()+
  labs(fill="key")+facet_grid(~rf$Country.of.Origin)+ xlab("")+ylab("")+
  scale_x_discrete(breaks = c(FALSE, TRUE), labels = c("Unemp", "Emp"))+   #theme(axis.text.x=element_blank())
  ggtitle("Employment/unemployment based on Countery of Origen ")+theme(text = element_text(size=7),
                                                                        axis.text.x = element_text(angle=90, hjust=1))

#employment graph based on gender

ggplot(rf,aes(rf$emp,fill=Sex))+geom_bar()+facet_wrap(~rf$Sex)+scale_x_discrete(breaks = c(FALSE, TRUE), labels = c("Unemployed", "Employed"))+
  ggtitle("Employment/unemployment based on Gender")+ylab("")+xlab("")+theme(text = element_text(size=7),
                                                                             axis.text.x = element_text(angle=90, hjust=1))

ggplot(rf,aes(rf$emp,fill=Religion))+geom_bar()+facet_wrap(~rf$Religion)+scale_x_discrete(breaks = c(FALSE, TRUE), labels = c("Uem", "Em"))+
  ggtitle("Employment/unemployment based on Religen")+ylab("")+xlab("")#+facet_wrap(~rf$Sex)

rf$fullAddress<-paste(rf$Address,rf$City,sep=",")
rf$zipState<-paste(rf$Zip.Code,rf$State, sep = " ")

rf$fullAdd<-paste(rf$fullAddress,rf$zipState,sep = " ")


for(i in 1:5){
  temp<-geocode(rf$fullAdd[i])
  rf$lon[i]<-temp$lon
  rf$lat<-temp$lat
}

US<-qmap("Omaha",zoom=10,color="bw")
US	+ geom_point(aes(x = rf$lon,	y = rf$lat),color="red",data = rf)+
  annotate("text",	x=rf$lon,	y=rf$lat+1,		label = rf$First.Name,
           colour = I("red"),	size = 4)+
  ggtitle("Refugee Locaion")
