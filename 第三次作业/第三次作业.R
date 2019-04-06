library(sp)
library(spacetime)
library(trajectories)
library(SimilarityMeasures)

gps_00 <- read.csv('D:/大三下/空间数据库/gps/gps_u52.csv',header = TRUE,sep=',')

#取全部时间
for (i in c(gps_00['time'])) {
  t = as.POSIXct(i, origin="1970-01-01 00:00:00")
}

#画图
x = as.vector(unlist(c(gps_00['latitude'])))
y = as.vector(unlist(c(gps_00['longitude'])))
n = length(x)
require(rgdal)
crs = CRS("+proj=longlat +ellps=WGS84") # longlat
stidf = STIDF(SpatialPoints(cbind(x,y),crs), t,data.frame(co2 = rnorm(n))) #rnorm产生n个服从正态分布的随机数
A0 = Track(stidf)
# Tracks for person A:
A = Tracks(list(A0))
B = Tracks(list(A0))
Tr = TracksCollection(list(B=B))
stplot(Tr, scales = list(draw=TRUE))
stplot(Tr, attr = "direction", arrows=TRUE, lwd = 3, by = "IDs")

#####################################################################
学生ID<-c('u01','u02','u04','u05','u07','u08','u09','u10','u12','u14','u15','u16','u17','u18','u19',
          'u22','u24','u25','u27','u30','u32','u33','u41','u43','u46','u49','u52','u54','u57','u59')
GPA<-c(2.863,3.505,3.029,3.679,3.474,3.705,3.806,3.667,3.245,3.293,2.815,3.373,3.476,3.474,3.947,3.889,
       2.987,2.765,3.719,3.93,3.826,2.815,3.652,3.79,3.646,3.625,2.4,3.343,3.389,3.519)
student<-data.frame(学生ID,GPA)
dtwframe<-data.frame()

xlist<-list.files("D:/大三下/空间数据库/gps") #读取文件夹下的所有文件
for(total in 1:length(xlist)) { 
  name<-paste("D:/大三下/空间数据库/gps/",xlist[total],sep="") #默认为空格为分隔符
  x <- read.csv(name,header = TRUE,sep=',')

  
#取全部时间
for (i in c(x['time'])) {
  t = as.POSIXct(i, origin="1970-01-01 00:00:00")
}
  

#取时间、经纬度
daytimes<-data.frame(time=t,latitude=x$latitude,longitude=x$longitude)

#统计每天的时间、经纬度
daytime<-c() #记录每一天时间、经纬度的向量
day<-format(daytimes[1,1],"%Y-%m-%d") #日期
count<-0 #记录天数
dayvector<-c() #记录全部天数时间、经纬度的向量
for (i in 1:nrow(daytimes))
{
  if(day==format(daytimes[i,1],"%Y-%m-%d")) 
  {
    dt<-list(daytimes[i,1],daytimes[i,2],daytimes[i,3])
    daytime<-c(daytime,dt)
  }
  else
  {
    dayvector<-c(dayvector,list(daytime))
    #统计下一天
    day<-format(daytimes[i,1],"%Y-%m-%d") #日期
    daytime<-data.frame() #记录每一天记录时间、经纬度的数据框
    dt<-list(daytimes[i,1],daytimes[i,2],daytimes[i,3])
    daytime<-c(daytime,dt)
  }
  #统计最后一天的
  if(i==nrow(daytimes))
  {
    dayvector<-c(dayvector,list(daytime))
  }
}

#统计相邻两天的dtw
dtw1<-0
dtw2<-0
dtw<-0

#三个数据表示一个时间的记录
a1<-1 #日期
b1<-2 #latitude
c1<-3 #longitude

count<-0 
for (i in 3:(length(dayvector)-1)) #舍去第一天和最后一天的数据
{
  ftime<-data.frame()
  stime<-data.frame()
  #前一天
  for (j in 1:(length(dayvector[[i-1]])/3))
  {
    day1<-format(dayvector[[i-1]][[1]],"%Y-%m-%d")
    ftime<-rbind(ftime,data.frame(time=dayvector[[i-1]][[a1]],latitude=dayvector[[i-1]][[b1]],longitude=dayvector[[i-1]][[c1]]))
    a1<-a1+3
    b1<-b1+3
    c1<-c1+3
  }
  a1<-1 
  b1<-2 
  c1<-3 
  
  #后一天
  for (j in 1:(length(dayvector[[i]])/3))
  {
    day2<-format(dayvector[[i]][[1]],"%Y-%m-%d")
    stime<-rbind(stime,data.frame(time=dayvector[[i]][[a1]],latitude=dayvector[[i]][[b1]],longitude=dayvector[[i]][[c1]]))
    a1<-a1+3
    b1<-b1+3
    c1<-c1+3
  }
  a1<-1 
  b1<-2 
  c1<-3 
  
  #前一天上午时间段
  study1<-as.POSIXct(paste(day1,"00:00:01"))
  study2<-as.POSIXct(paste(day1,"12:00:00"))
  #前一天下午时间段
  study3<-as.POSIXct(paste(day1,"12:00:00"))
  study4<-as.POSIXct(paste(day1,"23:59:59"))
  #分别计算两天三个时间段的dtw
  require('tidyverse')
  require('lubridate')
  #上午
  interval1 <- interval(study1,study2) #取时间间隔
  #ftime[(ftime$time)%within%interval1,] #获取时间间隔内的每行数据
  #ftime$time[(ftime$time)%within%interval1] #获取时间间隔内的时间
  #ftime$latitude[(ftime$time)%within%interval1] #获取时间间隔内的latitude
  #ftime$longitude[(ftime$time)%within%interval1] #获取时间间隔内的longitude
  if((ftime$time)%within%interval1==TRUE&&length(ftime$time[(ftime$time)%within%interval1])>1) #舍弃只有一个记录的，因为一个记录不能构成线
  {
    x1 = as.vector(unlist(c(ftime$latitude[(ftime$time)%within%interval1]))) #取latitude
    y1 = as.vector(unlist(c(ftime$longitude[(ftime$time)%within%interval1]))) #取longitude
    n = length(x)
    require(rgdal)
    crs = CRS("+proj=longlat +ellps=WGS84") # longlat
    stidf = STIDF(SpatialPoints(cbind(x1,y1),crs), ftime$time[(ftime$time)%within%interval1],data.frame(co2 = rnorm(n))) #rnorm产生n个服从正态分布的随机数
    A1 = Track(stidf)
  }
  #下午
  interval2 <- interval(study3,study4) #取时间间隔
  if((ftime$time)%within%interval2==TRUE&&length(ftime$time[(ftime$time)%within%interval1])>1)
  {
    x1 = as.vector(unlist(c(ftime$latitude[(ftime$time)%within%interval2]))) #取latitude
    y1 = as.vector(unlist(c(ftime$longitude[(ftime$time)%within%interval2]))) #取longitude
    n = length(x)
    require(rgdal)
    crs = CRS("+proj=longlat +ellps=WGS84") # longlat
    stidf = STIDF(SpatialPoints(cbind(x1,y1),crs), ftime$time[(ftime$time)%within%interval2],data.frame(co2 = rnorm(n))) #rnorm产生n个服从正态分布的随机数
    A2 = Track(stidf)
  }
  
  #后一天上午时间段
  study1<-as.POSIXct(paste(day2,"00:00:01"))
  study2<-as.POSIXct(paste(day2,"12:00:00"))
  #后一天下午时间段
  study3<-as.POSIXct(paste(day2,"12:00:00"))
  study4<-as.POSIXct(paste(day2,"23:59:59"))
  #上午
  interval1 <- interval(study1,study2) #取时间间隔
  if((stime$time)%within%interval1==TRUE&&length(stime$time[(stime$time)%within%interval1])>1)
  {
    x1 = as.vector(unlist(c(stime$latitude[(stime$time)%within%interval1]))) #取latitude
    y1 = as.vector(unlist(c(stime$longitude[(stime$time)%within%interval1]))) #取longitude
    n = length(x)
    require(rgdal)
    crs = CRS("+proj=longlat +ellps=WGS84") # longlat
    stidf = STIDF(SpatialPoints(cbind(x1,y1),crs), stime$time[(stime$time)%within%interval1],data.frame(co2 = rnorm(n))) #rnorm产生n个服从正态分布的随机数
    B1 = Track(stidf) 
  }
  #下午
  interval2 <- interval(study3,study4) #取时间间隔
  if((stime$time)%within%interval2==TRUE&&length(stime$time[(stime$time)%within%interval1])>1)
  {
    x1 = as.vector(unlist(c(stime$latitude[(stime$time)%within%interval2]))) #取latitude
    y1 = as.vector(unlist(c(stime$longitude[(stime$time)%within%interval2]))) #取longitude
    n = length(x)
    require(rgdal)
    crs = CRS("+proj=longlat +ellps=WGS84") # longlat
    stidf = STIDF(SpatialPoints(cbind(x1,y1),crs), stime$time[(stime$time)%within%interval2],data.frame(co2 = rnorm(n))) #rnorm产生n个服从正态分布的随机数
    B2 = Track(stidf)
  }
  
  dtw1<-dtw1+DTW(A1@sp@coords,B1@sp@coords,-1)
  dtw2<-dtw2+DTW(A2@sp@coords,B2@sp@coords,-1)
  dtw<-(dtw1+dtw2)/(length(dayvector)-2)
}
paste("这个同学的DTW为：",dtw)
dtwframe<-rbind(dtwframe,data.frame(特征值=dtw))
#student[total,3]=dtw
}
studenttotal<-data.frame(student,dtwframe)
cor_mat<-cor(studenttotal[,2],studenttotal[,3])
paste("学生的GPA和DTW的相关系数等于:",cor_mat)