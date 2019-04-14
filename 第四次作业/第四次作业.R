library(sp)
library(spacetime)
library(trajectories)
library(SimilarityMeasures)
library(geosphere)
t1=c(43.69990,-72.29154)
t2=c(43.69991,-72.29153)
aa=rbind(t1=t1,t2=t2)
distance=data.frame(distm(aa))#注意distm(aa)，里面有四个数值，第一个数值为第一个点和第一个点的距离，第二个数值为第一个点和第二个点的距离，
#第三个数值为第二个点和第一个点的距离，第四个数值为第二个点和第二个点的距离（单位都是米）



#处理一个同学的数据并画图
#################################################
gps_00 <- read.csv('D:/大三下/空间数据库/gps/gps_u19.csv',header = TRUE,sep=',')

#取全部时间
for (i in c(gps_00['time'])) {
  t = as.POSIXct(i, origin="1970-01-01 00:00:00")
}


#取时间、经纬度
daytimes<-data.frame(time=t,latitude=gps_00$latitude,longitude=gps_00$longitude)

#取时间段内的数据
eat1<-as.POSIXct(paste("2013-04-13","12:00:00"))
eat2<-as.POSIXct(paste("2013-04-13","24:00:00"))
require('tidyverse')
require('lubridate')
interval123<-interval(eat1,eat2)

timesinterval<-data.frame(time=daytimes$time[(daytimes$time%within%interval123)],
                          latitude=daytimes$latitude[(daytimes$time%within%interval123)],
                          longitude=daytimes$longitude[(daytimes$time%within%interval123)])

firsttimes<-data.frame() #保存在进行聚类分析时用过的时间、经纬度
latertimes<-data.frame() #保存聚类之后的点的时间、经纬度

for (axis1 in 1:nrow(timesinterval))
{
  for (axis2 in (axis1+1):nrow(timesinterval))
  {
    #判断之后的点是否被测量距离，如果被测量则退出本次循环，进行下一个点的循环
    if((timesinterval[axis1,1]%in%firsttimes$time)==TRUE)
      break
    
    t1=c(timesinterval[axis1,2],timesinterval[axis1,3])
    t2=c(timesinterval[axis2,2],timesinterval[axis2,3])
    aa=rbind(t1,t2)
    distance=data.frame(distm(aa))
    if(distance[1,2]<15)
    {
      #将后面被测量过的点装入firsttimes中
      firsttimes<-rbind(firsttimes,data.frame(time=timesinterval[axis2,1],latitude=timesinterval[axis2,2],longitude=timesinterval[axis2,3]))
    }
  }
  
  #将firsttimes中没有的点（即为聚合点）装入latertimes中
  if((timesinterval[axis1,1]%in%firsttimes$time)==FALSE)
  {
    latertimes<-rbind(latertimes,data.frame(time=timesinterval[axis1,1],latitude=timesinterval[axis1,2],longitude=timesinterval[axis1,3]))
  }
}

#画图
x = as.vector(unlist(c(latertimes$latitude)))
y = as.vector(unlist(c(latertimes$longitude)))
n = length(x)
require(rgdal)
crs = CRS("+proj=longlat +ellps=WGS84") # longlat
stidf = STIDF(SpatialPoints(cbind(x,y),crs), latertimes$time,data.frame(co2 = rnorm(n))) #rnorm产生n个服从正态分布的随机数
A0 = Track(stidf)
# Tracks for person A:
A = Tracks(list(A0))
B = Tracks(list(A0))
Tr = TracksCollection(list(B=B))
stplot(Tr, scales = list(draw=TRUE))





#处理所有同学的数据
#####################################################################
学生ID<-c('u02','u04','u05','u07','u08','u09','u10','u14','u15','u16','u17','u18','u19',
        'u24','u25','u27','u32','u33','u41','u43','u46','u49')
GPA<-c(3.505,3.029,3.679,3.474,3.705,3.806,3.667,3.293,2.815,3.373,3.476,3.474,3.947,
       2.987,2.765,3.719,3.826,2.815,3.652,3.79,3.646,3.625)
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
    timesinterval<-data.frame(time=ftime$time[(ftime$time%within%interval1)],
                              latitude=ftime$latitude[(ftime$time%within%interval1)],
                              longitude=ftime$longitude[(ftime$time%within%interval1)])
    firsttimes<-data.frame() #保存在进行聚类分析时用过的时间、经纬度
    latertimes<-data.frame() #保存聚类之后的点的时间、经纬度
    for (axis1 in 1:nrow(timesinterval))
    {
      if(nrow(timesinterval)==0) #时间段内没有数据，则直接结束
        break
      for (axis2 in (axis1+1):nrow(timesinterval))
      {
        if(axis1==(nrow(timesinterval))) #判断到最后一个直接结束，不然axis2=axis1+1超出范围
          break
        #判断之后的点是否被测量距离，如果被测量则退出本次循环，进行下一个点的循环
        if((timesinterval[axis1,1]%in%firsttimes$time)==TRUE)
          break
        t1=c(timesinterval[axis1,2],timesinterval[axis1,3])
        t2=c(timesinterval[axis2,2],timesinterval[axis2,3])
        aa=rbind(t1,t2)
        distance=data.frame(distm(aa))
        if(distance[1,2]<5)
        {
          #将后面被测量过的点装入firsttimes中
          firsttimes<-rbind(firsttimes,data.frame(time=timesinterval[axis2,1],latitude=timesinterval[axis2,2],longitude=timesinterval[axis2,3]))
        }
      }
      #将firsttimes中没有的点（即为聚合点）装入latertimes中
      if((timesinterval[axis1,1]%in%firsttimes$time)==FALSE)
      {
        latertimes<-rbind(latertimes,data.frame(time=timesinterval[axis1,1],latitude=timesinterval[axis1,2],longitude=timesinterval[axis1,3]))
      }
    }
    if(length(latertimes$time)>1) #舍弃只有一个记录的，因为一个记录不能构成线
    {
      x1 = as.vector(unlist(c(latertimes$latitude))) #取latitude
      y1 = as.vector(unlist(c(latertimes$longitude))) #取longitude
      n = length(x)
      require(rgdal)
      crs = CRS("+proj=longlat +ellps=WGS84") # longlat
      stidf = STIDF(SpatialPoints(cbind(x1,y1),crs), latertimes$time,data.frame(co2 = rnorm(n))) #rnorm产生n个服从正态分布的随机数
      A1 = Track(stidf)
    }
    
    #下午
    interval2 <- interval(study3,study4) #取时间间隔
    timesinterval<-data.frame(time=ftime$time[(ftime$time%within%interval2)],
                              latitude=ftime$latitude[(ftime$time%within%interval2)],
                              longitude=ftime$longitude[(ftime$time%within%interval2)])
    firsttimes<-data.frame() #保存在进行聚类分析时用过的时间、经纬度
    latertimes<-data.frame() #保存聚类之后的点的时间、经纬度
    for (axis1 in 1:nrow(timesinterval))
    {
      if(nrow(timesinterval)==0) #时间段内没有数据，则直接结束
        break
      for (axis2 in (axis1+1):nrow(timesinterval))
      {
        if(axis1==(nrow(timesinterval))) #判断到最后一个直接结束，不然axis2=axis1+1超出范围
          break
        #判断之后的点是否被测量距离，如果被测量则退出本次循环，进行下一个点的循环
        if((timesinterval[axis1,1]%in%firsttimes$time)==TRUE)
          break
        t1=c(timesinterval[axis1,2],timesinterval[axis1,3])
        t2=c(timesinterval[axis2,2],timesinterval[axis2,3])
        aa=rbind(t1,t2)
        distance=data.frame(distm(aa))
        if(distance[1,2]<5)
        {
          #将后面被测量过的点装入firsttimes中
          firsttimes<-rbind(firsttimes,data.frame(time=timesinterval[axis2,1],latitude=timesinterval[axis2,2],longitude=timesinterval[axis2,3]))
        }
      }
      #将firsttimes中没有的点（即为聚合点）装入latertimes中
      if((timesinterval[axis1,1]%in%firsttimes$time)==FALSE)
      {
        latertimes<-rbind(latertimes,data.frame(time=timesinterval[axis1,1],latitude=timesinterval[axis1,2],longitude=timesinterval[axis1,3]))
      }
    }
    if(length(latertimes$time)>1)
    {
      x1 = as.vector(unlist(c(latertimes$latitude))) #取latitude
      y1 = as.vector(unlist(c(latertimes$longitude))) #取longitude
      n = length(x)
      require(rgdal)
      crs = CRS("+proj=longlat +ellps=WGS84") # longlat
      stidf = STIDF(SpatialPoints(cbind(x1,y1),crs), latertimes$time,data.frame(co2 = rnorm(n))) #rnorm产生n个服从正态分布的随机数
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
    timesinterval<-data.frame(time=stime$time[(stime$time%within%interval1)],
                              latitude=stime$latitude[(stime$time%within%interval1)],
                              longitude=stime$longitude[(stime$time%within%interval1)])
    firsttimes<-data.frame() #保存在进行聚类分析时用过的时间、经纬度
    latertimes<-data.frame() #保存聚类之后的点的时间、经纬度
    for (axis1 in 1:nrow(timesinterval))
    {
      if(nrow(timesinterval)==0) #时间段内没有数据，则直接结束
        break
      for (axis2 in (axis1+1):nrow(timesinterval))
      {
        if(axis1==(nrow(timesinterval))) #判断到最后一个直接结束，不然axis2=axis1+1超出范围
          break
        #判断之后的点是否被测量距离，如果被测量则退出本次循环，进行下一个点的循环
        if((timesinterval[axis1,1]%in%firsttimes$time)==TRUE)
          break
        t1=c(timesinterval[axis1,2],timesinterval[axis1,3])
        t2=c(timesinterval[axis2,2],timesinterval[axis2,3])
        aa=rbind(t1,t2)
        distance=data.frame(distm(aa))
        if(distance[1,2]<5)
        {
          #将后面被测量过的点装入firsttimes中
          firsttimes<-rbind(firsttimes,data.frame(time=timesinterval[axis2,1],latitude=timesinterval[axis2,2],longitude=timesinterval[axis2,3]))
        }
      }
      #将firsttimes中没有的点（即为聚合点）装入latertimes中
      if((timesinterval[axis1,1]%in%firsttimes$time)==FALSE)
      {
        latertimes<-rbind(latertimes,data.frame(time=timesinterval[axis1,1],latitude=timesinterval[axis1,2],longitude=timesinterval[axis1,3]))
      }
    }
    if(length(latertimes$time)>1)
    {
      x1 = as.vector(unlist(c(latertimes$latitude))) #取latitude
      y1 = as.vector(unlist(c(latertimes$longitude))) #取longitude
      n = length(x)
      require(rgdal)
      crs = CRS("+proj=longlat +ellps=WGS84") # longlat
      stidf = STIDF(SpatialPoints(cbind(x1,y1),crs), latertimes$time,data.frame(co2 = rnorm(n))) #rnorm产生n个服从正态分布的随机数
      B1 = Track(stidf) 
    }
    
    #下午
    interval2 <- interval(study3,study4) #取时间间隔
    timesinterval<-data.frame(time=stime$time[(stime$time%within%interval2)],
                              latitude=stime$latitude[(stime$time%within%interval2)],
                              longitude=stime$longitude[(stime$time%within%interval2)])
    firsttimes<-data.frame() #保存在进行聚类分析时用过的时间、经纬度
    latertimes<-data.frame() #保存聚类之后的点的时间、经纬度
    for (axis1 in 1:nrow(timesinterval))
    {
      if(nrow(timesinterval)==0) #时间段内没有数据，则直接结束
        break
      for (axis2 in (axis1+1):nrow(timesinterval))
      {
        if(axis1==(nrow(timesinterval))) #判断到最后一个直接结束，不然axis2=axis1+1超出范围
          break
        #判断之后的点是否被测量距离，如果被测量则退出本次循环，进行下一个点的循环
        if((timesinterval[axis1,1]%in%firsttimes$time)==TRUE)
          break
        t1=c(timesinterval[axis1,2],timesinterval[axis1,3])
        t2=c(timesinterval[axis2,2],timesinterval[axis2,3])
        aa=rbind(t1,t2)
        distance=data.frame(distm(aa))
        if(distance[1,2]<5)
        {
          #将后面被测量过的点装入firsttimes中
          firsttimes<-rbind(firsttimes,data.frame(time=timesinterval[axis2,1],latitude=timesinterval[axis2,2],longitude=timesinterval[axis2,3]))
        }
      }
      #将firsttimes中没有的点（即为聚合点）装入latertimes中
      if((timesinterval[axis1,1]%in%firsttimes$time)==FALSE)
      {
        latertimes<-rbind(latertimes,data.frame(time=timesinterval[axis1,1],latitude=timesinterval[axis1,2],longitude=timesinterval[axis1,3]))
      }
    }
    if(length(latertimes$time)>1)
    {
      x1 = as.vector(unlist(c(latertimes$latitude))) #取latitude
      y1 = as.vector(unlist(c(latertimes$longitude))) #取longitude
      n = length(x)
      require(rgdal)
      crs = CRS("+proj=longlat +ellps=WGS84") # longlat
      stidf = STIDF(SpatialPoints(cbind(x1,y1),crs), latertimes$time,data.frame(co2 = rnorm(n))) #rnorm产生n个服从正态分布的随机数
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
#计算DTW和GPA的相关系数
studenttotal<-data.frame(student,dtwframe)
cor_mat<-cor(studenttotal[,2],studenttotal[,3])




#预测成绩
############################################
#建立线性回归模型
library("car")
GPA_train<-lm(GPA~特征值,data=studenttotal)
summary(GPA_train)
#预测成绩
predict(GPA_train,studenttotal["特征值"])
studentpredict<-data.frame(studenttotal,预测的GPA=predict(GPA_train,studenttotal["特征值"]))