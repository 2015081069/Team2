# 第三次实验报告
## 一、摘要
 1、收集了30名达特茅斯学生10周内的自动传感数据，并提出了一种新的衡量标准，即衡量每个学生的校园日常生活（如：行走轨迹）的规律性。<br>
 2、我们还发现，即使在学生的勤奋工作中，“自律性”是预测学业表现的一个重要特征，它也能显著提高预测的准确性。
 
## 二、数据描述
### 数据来源：30名达特茅斯学生10周内的自动传感数据
### 规模：30名学生行为记录
数据字段：

![数据如下所示:](https://github.com/cuit201608/Team2/blob/master/第三次作业/数据.png)

## 三、分析框架
### 1、选取每个学生的经纬度数据，以0点-12点和12点-24点为时间间隔，建立空间轨迹。
### 2、选取每个学生相邻两天的相同时间段的轨迹，提取特征，计算DTW值。
### 3、将每个同学的DTW值和GPA做相关性分析，从而分析GPA与学生的活动轨迹有无关联。

## 四、数据分析
### 1、循环读取文件夹下每个学生的数据文件
``` R
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
```

### 2、统计每个同学每一天所记录的时间、经纬度的数据
``` R
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
```

### 3、计算相邻两天的相同时间段中的DTW
### 基于DTW距离的特征提取理论如下：
![DTW理论如下：](https://github.com/cuit201608/Team2/blob/master/第三次作业/基于DTW距离的特征提取.png)
``` R
for (i in 3:(length(dayvector)-1)) #舍去第一天和最后一天的数据
{
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
```
### 结果如下：
![结算所生成结果如下所示：](https://github.com/cuit201608/Team2/blob/master/第三次作业/结果1.png)

### 4、计算GPA和DTW之间的相关性
### 相关系数定义如下：
![相关系数定义如下：](https://github.com/cuit201608/Team2/blob/master/第三次作业/相关系数定义.png)
``` R
studenttotal<-data.frame(student,dtwframe)
cor_mat<-cor(studenttotal[,2],studenttotal[,3])
paste("学生的GPA和DTW的相关系数等于:",cor_mat)
```

## 五、实验结果
### GPA和DTW的相关性计算结果如下：
![GPA和DTW的相关性结果如下：](https://github.com/cuit201608/Team2/blob/master/第三次作业/结果2.png)

### 结果分析：如上图所示，GPA和DTW的相关系数靠近0，所以根据相关系数的定义可知：学生的GPA与DTW不相关，即是说学生的GPA和学生的行为轨迹无关。

### 论证上述结论：下图为成绩最差的同学尽十周来的轨迹，可见行为轨迹越规律，GPA不一定越好。
![论证结论：](https://github.com/cuit201608/Team2/blob/master/第三次作业/例子.png)
