library("data.table")
library("plotly")
library("plyr")

#Read the Data
Facebook <- read.csv("Report.csv",header = T)
#Set As Data Table
setDT(Facebook)

#Calculate the sum of types by rows
Facebook[, .N, by = Type]
Total<-Facebook[, c(.N, lapply(.SD, sum)), by= .(Month,Type,Fiscal.Year), .SDcols = Lifetime.Post.Total.Reach:Lifetime.Video.length]
Total<- Total[, .SD, .SDcols=sapply(Total, function(x) any(x!=0))]


#YEAR
Total_Year<- setDT(Facebook)[order(Year)]
Total_Year<-Total_Year[, c(.N, lapply(.SD, sum)), by= .(Year,Type), .SDcols = Lifetime.Post.Total.Reach:Lifetime.Video.length]
Total_Year<- Total_Year[, .SD, .SDcols=sapply(Total_Year, function(x) any(x!=0))]
Total_Year<-ddply(Total_Year, .(Year,Type,N), summarise,n=length(Type))

#MONTH
Total_Month<- setDT(Facebook)[order(Posted)]
Total_Month<-Total_Month[, c(.N, lapply(.SD, sum)), by= .(Posted,Type), .SDcols = Lifetime.Post.Total.Reach:Lifetime.Video.length]
Total_Month<- Total_Month[, .SD, .SDcols=sapply(Total_Month, function(x) any(x!=0))]
Total_Month<-ddply(Total_Month, .(Posted,Type,N), summarise,n=length(Posted))

#DAY
Total_Day<-Facebook[, c(.N, lapply(.SD, sum)), by= .(Type,Day), .SDcols = Lifetime.Post.Total.Reach:Lifetime.Video.length]
Total_Day<- Total_Day[, .SD, .SDcols=sapply(Total_Day, function(x) any(x!=0))]

#HOUR
Total_Hour<-Facebook[, c(.N, lapply(.SD, sum)), by= .(Type,Hour), .SDcols = Lifetime.Post.Total.Reach:Lifetime.Video.length]
Total_Hour<- Total_Hour[, .SD, .SDcols=sapply(Total_Hour, function(x) any(x!=0))]

#Day/Night
Total_DN<-Facebook[, c(.N, lapply(.SD, sum)), by= .(DN,Type), .SDcols = Lifetime.Post.Total.Reach:Lifetime.Video.length]
Total_DN<- Total_DN[, .SD, .SDcols=sapply(Total_DN, function(x) any(x!=0))]
Total_DN<-ddply(Total_DN, .(DN,Type,N), summarise,n=length(DN))


#Individual Sums of Link/Status/Photo/SharedVideo/Video by DAY
Link_Day<-Total_Day[which(Total_Day$Type == "Link"),]
Status_Day<-Total_Day[which(Total_Day$Type == "Status"),]
Photo_Day<-Total_Day[which(Total_Day$Type == "Photo"),]
SharedVideo_Day<-Total_Day[which(Total_Day$Type == "SharedVideo"),]
Video_Day<-Total_Day[which(Total_Day$Type == "Video"),]


#Individual Sums of Link/Status/Photo/SharedVideo/Video by HOUR
Link_Hour<-Total_Hour[which(Total_Hour$Type == "Link"),]
Photo_Hour<-Total_Hour[which(Total_Hour$Type == "Photo"),]
Status_Hour<-Total_Hour[which(Total_Hour$Type == "Status"),]
SharedVideo_Hour<-Total_Hour[which(Total_Hour$Type == "SharedVideo"),]
Video_Hour<-Total_Hour[which(Total_Hour$Type == "Video"),]

#Link/Status/Photo/SharedVideo/Video by Year
Link_Year<-Facebook[Type=='Link', c(.N, lapply(.SD, sum)), by= .(Type,Year), .SDcols = Lifetime.Post.Total.Reach:Lifetime.Video.length]
Link_Year<- Link_Year[, .SD, .SDcols=sapply(Link_Year, function(x) any(x!=0))]
Status_Year<-Facebook[Type=='Status', c(.N, lapply(.SD, sum)), by= .(Type,Year), .SDcols = Lifetime.Post.Total.Reach:Lifetime.Video.length]
Status_Year<- Status_Year[, .SD, .SDcols=sapply(Status_Year, function(x) any(x!=0))]
Photo_Year<-Facebook[Type=='Photo', c(.N, lapply(.SD, sum)), by= .(Type,Year), .SDcols = Lifetime.Post.Total.Reach:Lifetime.Video.length]
Photo_Year<- Photo_Year[, .SD, .SDcols=sapply(Photo_Year, function(x) any(x!=0))]
SharedVideo_Year<-Facebook[Type=='SharedVideo', c(.N, lapply(.SD, sum)), by= .(Type,Year), .SDcols = Lifetime.Post.Total.Reach:Lifetime.Video.length]
SharedVideo_Year<- SharedVideo_Year[, .SD, .SDcols=sapply(SharedVideo_Year, function(x) any(x!=0))]
Video_Year<-Facebook[Type=='Video', c(.N, lapply(.SD, sum)), by= .(Type,Year), .SDcols = Lifetime.Post.Total.Reach:Lifetime.Video.length]
Video_Year<- Video_Year[, .SD, .SDcols=sapply(Video_Year, function(x) any(x!=0))]

#Plot for links per Year
plot_ly(Link_Year, x = ~interaction(N,Year), y = ~Lifetime.Post.Total.Reach,name='Lifetime.Post.Total.Reach',type = 'scatter', mode = 'lines+markers',line = list(shape = "spline"))%>% 
  add_trace(y=~Lifetime.Post.Organic.Reach,name='Lifetime.Post.Organic.Reach')%>% 
  add_trace(y=~Lifetime.Post.Total.Impressions,name='Lifetime.Post.Total.Impressions')%>% 
  add_trace(y=~Lifetime.Post.Organic.Impressions,name='Lifetime.Post.Organic.Impressions')%>% 
  add_trace(y=~Lifetime.Engaged.Users,name='Lifetime.Engaged.Users')%>% 
  add_trace(y=~Lifetime.Post.Consumers,name='Lifetime.Post.Consumers')%>% 
  add_trace(y=~Lifetime.Post.Consumptions,name='Lifetime.Post.Consumptions')%>% 
  add_trace(y=~Lifetime.Negative.Feedback,name='Lifetime.Negative.Feedback')%>% 
  add_trace(y=~Lifetime.Negative.feedback.from.users,name='Lifetime.Negative.feedback.from.users')%>% 
  add_trace(y=~Lifetime.Post.impressions.by.people.who.have.liked.your.Page,name='Lifetime.Post.impressions.by.people.who.have.liked.your.Page')%>% 
  add_trace(y=~Lifetime.Post.reach.by.people.who.like.your.Page,name='Lifetime.Post.reach.by.people.who.like.your.Page')%>% 
  add_trace(y=~Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post,name='Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post')%>% 
  add_trace(y=~Lifetime.Organic.Video.Views,name='Lifetime.Organic.Video.Views')%>% 
  add_trace(y=~Lifetime.Organic.Video.Views4,name='Lifetime.Organic.Video.Views4')%>% 
  layout(title = " ",xaxis = list(title = "Links per Year"),yaxis = list (title = "Counts"))

#Plot for Status per Year
plot_ly(Status_Year, x = ~interaction(N,Year), y = ~Lifetime.Post.Total.Reach,name='Lifetime.Post.Total.Reach',type = 'scatter', mode = 'lines+markers',line = list(shape = "spline"))%>% 
  add_trace(y=~Lifetime.Post.Organic.Reach,name='Lifetime.Post.Organic.Reach')%>% 
  add_trace(y=~Lifetime.Post.Total.Impressions,name='Lifetime.Post.Total.Impressions')%>% 
  add_trace(y=~Lifetime.Post.Organic.Impressions,name='Lifetime.Post.Organic.Impressions')%>% 
  add_trace(y=~Lifetime.Engaged.Users,name='Lifetime.Engaged.Users')%>% 
  add_trace(y=~Lifetime.Post.Consumers,name='Lifetime.Post.Consumers')%>% 
  add_trace(y=~Lifetime.Post.Consumptions,name='Lifetime.Post.Consumptions')%>% 
  add_trace(y=~Lifetime.Negative.Feedback,name='Lifetime.Negative.Feedback')%>% 
  add_trace(y=~Lifetime.Negative.feedback.from.users,name='Lifetime.Negative.feedback.from.users')%>% 
  add_trace(y=~Lifetime.Post.impressions.by.people.who.have.liked.your.Page,name='Lifetime.Post.impressions.by.people.who.have.liked.your.Page')%>% 
  add_trace(y=~Lifetime.Post.reach.by.people.who.like.your.Page,name='Lifetime.Post.reach.by.people.who.like.your.Page')%>% 
  add_trace(y=~Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post,name='Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post')%>% 
  layout(title = " ",xaxis = list(title = "Status per Year"),yaxis = list (title = "Counts"))

#Plot for Photo per Year
plot_ly(Photo_Year, x = ~interaction(N,Year), y = ~Lifetime.Post.Total.Reach,name='Lifetime.Post.Total.Reach',type = 'scatter', mode = 'lines+markers',line = list(shape = "spline"))%>% 
  add_trace(y=~Lifetime.Post.Organic.Reach,name='Lifetime.Post.Organic.Reach')%>% 
  add_trace(y=~Lifetime.Post.Total.Impressions,name='Lifetime.Post.Total.Impressions')%>% 
  add_trace(y=~Lifetime.Post.Organic.Impressions,name='Lifetime.Post.Organic.Impressions')%>% 
  add_trace(y=~Lifetime.Engaged.Users,name='Lifetime.Engaged.Users')%>% 
  add_trace(y=~Lifetime.Post.Consumers,name='Lifetime.Post.Consumers')%>% 
  add_trace(y=~Lifetime.Post.Consumptions,name='Lifetime.Post.Consumptions')%>% 
  add_trace(y=~Lifetime.Negative.Feedback,name='Lifetime.Negative.Feedback')%>% 
  add_trace(y=~Lifetime.Negative.feedback.from.users,name='Lifetime.Negative.feedback.from.users')%>% 
  add_trace(y=~Lifetime.Post.impressions.by.people.who.have.liked.your.Page,name='Lifetime.Post.impressions.by.people.who.have.liked.your.Page')%>% 
  add_trace(y=~Lifetime.Post.reach.by.people.who.like.your.Page,name='Lifetime.Post.reach.by.people.who.like.your.Page')%>% 
  add_trace(y=~Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post,name='Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post')%>% 
  add_trace(y=~Lifetime.Organic.Video.Views,name='Lifetime.Organic.Video.Views')%>% 
  add_trace(y=~Lifetime.Organic.Video.Views4,name='Lifetime.Organic.Video.Views4')%>% 
  layout(title = " ",xaxis = list(title = "Photo per Year"),yaxis = list (title = "Counts"))

#Plot for SharedVideo per Year
plot_ly(SharedVideo_Year, x = ~interaction(N,Year), y = ~Lifetime.Post.Total.Reach,name='Lifetime.Post.Total.Reach',type = 'scatter', mode = 'lines+markers',line = list(shape = "spline"))%>% 
  add_trace(y=~Lifetime.Post.Organic.Reach,name='Lifetime.Post.Organic.Reach')%>% 
  add_trace(y=~Lifetime.Post.Total.Impressions,name='Lifetime.Post.Total.Impressions')%>% 
  add_trace(y=~Lifetime.Post.Organic.Impressions,name='Lifetime.Post.Organic.Impressions')%>% 
  add_trace(y=~Lifetime.Engaged.Users,name='Lifetime.Engaged.Users')%>% 
  add_trace(y=~Lifetime.Post.Consumers,name='Lifetime.Post.Consumers')%>% 
  add_trace(y=~Lifetime.Post.Consumptions,name='Lifetime.Post.Consumptions')%>% 
  add_trace(y=~Lifetime.Negative.Feedback,name='Lifetime.Negative.Feedback')%>% 
  add_trace(y=~Lifetime.Negative.feedback.from.users,name='Lifetime.Negative.feedback.from.users')%>% 
  add_trace(y=~Lifetime.Post.impressions.by.people.who.have.liked.your.Page,name='Lifetime.Post.impressions.by.people.who.have.liked.your.Page')%>% 
  add_trace(y=~Lifetime.Post.reach.by.people.who.like.your.Page,name='Lifetime.Post.reach.by.people.who.like.your.Page')%>% 
  add_trace(y=~Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post,name='Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post')%>% 
  add_trace(y=~Lifetime.Organic.Video.Views,name='Lifetime.Organic.Video.Views')%>% 
  add_trace(y=~Lifetime.Organic.Video.Views4,name='Lifetime.Organic.Video.Views4')%>% 
  add_trace(y=~Lifetime.Video.length,name='Lifetime.Video.length')%>%
  layout(title = " ",xaxis = list(title = "SharedVideo per Year"),yaxis = list (title = "Counts"))

#Plot for Video per Year
plot_ly(Video_Year, x = ~interaction(N,Year), y = ~Lifetime.Post.Total.Reach,name='Lifetime.Post.Total.Reach',type = 'scatter', mode = 'lines+markers',line = list(shape = "spline"))%>% 
  add_trace(y=~Lifetime.Post.Organic.Reach,name='Lifetime.Post.Organic.Reach')%>% 
  add_trace(y=~Lifetime.Post.Total.Impressions,name='Lifetime.Post.Total.Impressions')%>% 
  add_trace(y=~Lifetime.Post.Organic.Impressions,name='Lifetime.Post.Organic.Impressions')%>% 
  add_trace(y=~Lifetime.Engaged.Users,name='Lifetime.Engaged.Users')%>% 
  add_trace(y=~Lifetime.Post.Consumers,name='Lifetime.Post.Consumers')%>% 
  add_trace(y=~Lifetime.Post.Consumptions,name='Lifetime.Post.Consumptions')%>% 
  add_trace(y=~Lifetime.Negative.Feedback,name='Lifetime.Negative.Feedback')%>% 
  add_trace(y=~Lifetime.Negative.feedback.from.users,name='Lifetime.Negative.feedback.from.users')%>% 
  add_trace(y=~Lifetime.Post.impressions.by.people.who.have.liked.your.Page,name='Lifetime.Post.impressions.by.people.who.have.liked.your.Page')%>% 
  add_trace(y=~Lifetime.Post.reach.by.people.who.like.your.Page,name='Lifetime.Post.reach.by.people.who.like.your.Page')%>% 
  add_trace(y=~Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post,name='Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post')%>% 
  add_trace(y=~Lifetime.Organic.views.to.95.,name='Lifetime.Organic.views.to.95.')%>%
  add_trace(y=~Lifetime.Organic.views.to.95.2,name='Lifetime.Organic.views.to.95.2')%>%
  add_trace(y=~Lifetime.Organic.Video.Views,name='Lifetime.Organic.Video.Views')%>% 
  add_trace(y=~Lifetime.Organic.Video.Views4,name='Lifetime.Organic.Video.Views4')%>% 
  add_trace(y=~Lifetime.Average.time.video.viewed,name='Lifetime.Average.time.video.viewed')%>%
  add_trace(y=~Lifetime.Video.length,name='Lifetime.Video.length')%>%
  layout(title = " ",xaxis = list(title = "Video per Year"),yaxis = list (title = "Counts"))



#SCATTER PLOT

#Scatter Plot for Hour versus Total Links
s1<-plot_ly(x = ~Link_Hour$Hour, y = ~Link_Hour$N,size=Link_Hour$N, type = 'scatter', mode = 'markers',marker = list(color = "Darkred"),name='Link')%>%
  layout(title = "Link",xaxis = list(title = "Hour of the day"),yaxis = list (title = "Total_Links"))
#Scatter Plot for Hour versus Total Status
s2<-plot_ly(x = ~Status_Hour$Hour, y = ~Status_Hour$N,size=Status_Hour$N, type = 'scatter', mode = 'markers',marker = list(color = "green"),name='Status')%>%
  layout(title = "Status",xaxis = list(title = "Hour of the day"),yaxis = list (title = "Total_Status"))
#Scatter Plot for Hour versus Total Photo
s3<-plot_ly(x = ~Photo_Hour$Hour, y = ~Photo_Hour$N,size=Photo_Hour$N, type = 'scatter', mode = 'markers',marker = list(color = "darkgrey"),name='Photo')%>%
  layout(title = "Photo",xaxis = list(title = "Hour of the day"),yaxis = list (title = "Total_Photo"))
#Scatter Plot for Hour versus Total SHaredVideo
s4<-plot_ly(x = ~SharedVideo_Hour$Hour, y = ~SharedVideo_Hour$N,size=SharedVideo_Hour$N, type = 'scatter', mode = 'markers',marker = list(color ="black"),name='SharedVideo')%>%
  layout(title = "SharedVideo",xaxis = list(title = "Hour of the day"),yaxis = list (title = "Total_SharedVideo"))
#Scatter Plot for Hour versus Total Video
s5<-plot_ly(x = ~Video_Hour$Hour, y = ~Video_Hour$N,size=Video_Hour$N, type = 'scatter', mode = 'markers',marker = list(color ="purple"),name='Video')%>%
  layout(title = "Video",xaxis = list(title = "Hour of the day"),yaxis = list (title = "Total_Video"))

#COMPARISON BETWEEN ALL THE TYPES AND HOUR
subplot(s1,s2,s3,s4,s5,nrows = 5,shareX = TRUE)%>%layout(title = " ",xaxis = list(title = "Hour of the day"),yaxis = list (title = ""))

#BAR PLOT for Day versus all the types
plot_ly(title = 'Types per Day',x = ~Link_Day$Day, y = ~Link_Day$N, type = 'bar', name = 'Link') %>%
  add_trace(x = ~Photo_Day$Day,y = ~Photo_Day$N, name = 'Photo') %>%
  add_trace(x = ~SharedVideo_Day$Day,y = ~SharedVideo_Day$N, name = 'SharedVideo') %>%
  add_trace(x = ~Status_Day$Day,y = ~Status_Day$N, name = 'Status') %>%
  add_trace(x = ~Video_Day$Day,y = ~Video_Day$N, name = 'Video') %>%
  layout(title = "Types Per Day",xaxis = list(title = ' ', tickfont = list(size = 12,color = 'rgb(107, 107, 107)')),yaxis = list(title = 'Counts'),barmode = 'group', bargap = 0.15, bargroupgap = 0.1)


#LINE PLOT for Years 2015-2017
#l1<-plot_ly(Total_Year[which(Total_Year$Type=='Link'),], x = ~Year, y = ~N, name = 'Link', type = 'scatter', mode = 'lines',line = list(color = 'rgb(205, 12, 24)', width = 4))
#l2<-plot_ly(Total_Year[which(Total_Year$Type=='Photo'),], x = ~Year, y = ~N, name = 'Photo', type = 'scatter', mode = 'lines',line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'))
#l3<-plot_ly(Total_Year[which(Total_Year$Type=='Status'),], x = ~Year, y = ~N, name = 'Status', type = 'scatter', mode = 'lines',line = list(color = 'rgb(205, 12, 24)', width = 4,dash = 'dot'))
#l4<-plot_ly(Total_Year[which(Total_Year$Type=='SharedVideo'),], x = ~Year, y = ~N, name = 'SharedVideo', type = 'scatter', mode = 'lines',line = list(color = 'rgb(22, 96, 167)', width = 4))
#l5<-plot_ly(Total_Year[which(Total_Year$Type=='Video'),], x = ~Year, y = ~N, name = 'Video', type = 'scatter', mode = 'lines',line = list(color = 'rgb(205, 12, 24)', width = 4,dash = 'dash'))

#subplot(l1,l2,l3,l4,l5,nrows = 5,shareX = TRUE)%>%layout(title = " ",xaxis = list(title = "Year"),yaxis = list (title = ""))

#LINE PLOT for Month 2015-2017
w1<-plot_ly(Total_Month[which(Total_Month$Type=='Link'),], x = ~Posted, y = ~N, name = 'Link', type = 'scatter', mode = 'lines',line = list(color = 'rgb(205, 12, 24)', shape = "spline"))
w2<-plot_ly(Total_Month[which(Total_Month$Type=='Photo'),], x = ~Posted, y = ~N, name = 'Photo', type = 'scatter', mode = 'lines',line = list(color = 'rgb(22, 96, 167)',shape = "spline"))
w3<-plot_ly(Total_Month[which(Total_Month$Type=='Status'),], x = ~Posted, y = ~N, name = 'Status', type = 'scatter', mode = 'lines',line = list(color = 'rgb(205, 12, 24)', dash = 'dot'))
w4<-plot_ly(Total_Month[which(Total_Month$Type=='SharedVideo'),], x = ~Posted, y = ~N, name = 'SharedVideo', type = 'scatter', mode = 'lines',line = list(color = 'rgb(22, 96, 167)', shape = "linear"))
w5<-plot_ly(Total_Month[which(Total_Month$Type=='Video'),], x = ~Posted, y = ~N, name = 'Video', type = 'scatter', mode = 'lines',line = list(color = 'rgb(205, 12, 24)',dash = 'dash'))

subplot(w1,w2,w3,w4,w5,nrows = 5,shareX = TRUE)%>%layout(title = "Types per Year-Month ",xaxis = list(title = "Year-Month"),yaxis = list (title = ""))

#LINE PLOT for Minutes
d1<-plot_ly(Total_DN[which(Total_DN$Type=='Link'),], x = ~N, y = ~DN,name='Link', type = 'bar', orientation = 'h')
d2<-plot_ly(Total_DN[which(Total_DN$Type=='Photo'),], x = ~N, y = ~DN,name='Photo', type = 'bar', orientation = 'h')
d3<-plot_ly(Total_DN[which(Total_DN$Type=='Status'),], x = ~N, y = ~DN,name='Status', type = 'bar', orientation = 'h')
d4<-plot_ly(Total_DN[which(Total_DN$Type=='SharedVideo'),], x = ~N, y = ~DN,name='SharedVideo', type = 'bar', orientation = 'h')
d5<-plot_ly(Total_DN[which(Total_DN$Type=='Video'),], x = ~N, y = ~DN,name='Video', type = 'bar', orientation = 'h')

subplot(d1,d2,d3,d4,d5,nrows = 5,shareX = TRUE)%>%layout(title = "Types used in Day/Night",xaxis = list(title = "Count"),yaxis = list (title = " "))
