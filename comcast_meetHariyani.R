## importing Libraries
library(dplyr)
library(ggplot2)
library(lubridate)


#importing data
comcastData<- read.csv(file.choose())
View(comcastData)


# Data manipulation
names(comcastData)<- gsub(pattern = '\\.',replacement = "", x=names(comcastData))
names(comcastData)


# date processing
comcastData$Date<- dmy(comcastData$Date)
View(comcastData)


#Daily complaint trend
dailyCompl<- comcastData %>% group_by(Date) %>% summarise(NumberofComplaints=n())
ggplot(data= dailyCompl,aes(as.POSIXct(Date),NumberofComplaints))+
  geom_line()+
  geom_point(size= 1)+
  scale_x_datetime(breaks =  "1 week", date_labels = "%d/%m")+
  labs(title = "Daily Ticket Count", x="Days",y="No. Of Tickes")+
  theme(axis.text.x = element_text(angle = 75), plot.title = element_text(hjust = 0.5))


#monthly complaint trend
comcastData$Months<- months(comcastData$Date)
monthlyCompl<- comcastData %>% group_by(Months=as.integer(month(Date))) %>% 
  summarise(NumofComplaints=n()) %>% arrange(desc(NumofComplaints))
View(monthlyCompl)
ggplot(data = monthlyCompl,aes(Months,NumofComplaints))+
  geom_line()+
  geom_point(size=0.8)+
  scale_x_continuous(breaks = monthlyCompl$Months)+
  labs(x="Months",y="No. of Tickets",title = "Monthly ticket count")+
  theme(plot.title = element_text(hjust = 0.5))


#complaint type processing
network_tickets<- contains(comcastData$CustomerComplaint,match = 'network',ignore.case = T)
internet_tickets<- contains(comcastData$CustomerComplaint,match = 'internet',ignore.case = T)
billing_tickets<- contains(comcastData$CustomerComplaint,match = 'bill',ignore.case = T)
email_tickets<- contains(comcastData$CustomerComplaint,match = 'email',ignore.case = T)
charges_tickets<- contains(comcastData$CustomerComplaint,match = 'charge',ignore.case = T)
comcastData$complaintType[internet_tickets]<-"Internet"
comcastData$complaintType[billing_tickets]<-"Billing"
comcastData$complaintType[email_tickets]<-"Email"
comcastData$complaintType[network_tickets]<-"Network"
comcastData$complaintType[charges_tickets]<-"Charges"
comcastData$complaintType[-c(internet_tickets,billing_tickets,email_tickets,network_tickets,charges_tickets)]<- "Others"
table(comcastData$complaintType)


#new categorical veriable complaint status.
open_complaints<- (comcastData$Status=='Open'| comcastData$Status=='Pending')
close_complaints<- (comcastData$Status=='Closed' | comcastData$Status=='Solved')
comcastData$complaintsStatus[open_complaints]<- 'Open'
comcastData$complaintsStatus[close_complaints]<- 'Closed'
stakes<- table(comcastData$complaintsStatus,comcastData$State)
stake
comcastData<- group_by(comcastData,State,complaintsStatus)
chartData<-summarise(comcastData,Count=n())
View(chartData)
ggplot(as.data.frame(chartData),mapping = aes(State,Count))+
  geom_col(aes(fill=complaintsStatus),width = 0.95)+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 16,colour = "#0073C2FF"),
        plot.title = element_text(hjust =  0.5))+
  labs(title = "Ticket Status Staked Bar Chart",x="States",y="No. of tickets",fill= "Status")

#Unsolved complaints by state
comcastData %>% filter(complaintsStatus=='Open') %>% group_by(State) %>% 
  summarise(Numofcomplaints=n()) %>% arrange(desc(Numofcomplaints)) 


#Resolved complaints
total<- comcastData %>% group_by(complaintsStatus) %>% summarise(Numofcomplaints=n())
total
slc<- total$Numofcomplaints
perc<- round((slc/sum(slc)*100),2)
lb<-paste(total$complaintsStatus," ",perc,"%",sep="")
pie(slc,labels = lb,radius = 0.5)


#solved complaints which are came through internet and Customer care call.
internet<- comcastData %>% filter(ReceivedVia=='Internet',complaintsStatus=='Closed') %>% 
  group_by(ReceivedVia,complaintsStatus) %>% summarise(Numofcomplaints=n())
customercc<- comcastData %>% filter(ReceivedVia=='Customer Care Call',complaintsStatus=='Closed') %>% 
  group_by(ReceivedVia,complaintsStatus)%>% summarise(Numofcomplaints=n())


#percentage of resolved internet complaints
internet_pr<-round(internet$Numofcomplaints/sum(total$Numofcomplaints)*100,2)
internet_pr


#percentage of resolved customer care call complaints
custo_pr<-round(customercc$Numofcomplaints/sum(total$Numofcomplaints)*100,2)
custo_pr
