# 第四次实验报告
## 一、摘要
1、物体之间的距离随着时间的推移会发现一些有趣的信息。<br>
2、随时间的成对距离可以某种方式揭示对象之间的交互类型。<br>
3、拥有一个轨道模式，我们可以看到移动对象如何随着时间的推移相互作用。<br>
4、这可以指示特定时期内的拥挤时间。<br>
5、移动对象是由多个时间片(Time Slices)上的空间位置组成。<br>
6、首先分别对每个时间片上的点进行聚类，然后计算连续时间片中聚类所包含点的重合程度，如果大于一定阈值，那么这个移动聚类成立。

## 二、数据描述
### 数据来源：30名达特茅斯学生10周内的自动传感数据
### 规模：30名学生行为记录
数据字段：

![数据如下所示:](https://github.com/cuit201608/Team2/blob/master/第四次作业/截图/数据.png)

## 三、分析框架
### 1、选取每个学生的经纬度数据，以0点-12点和12点-24点为时间间隔，建立空间轨迹。
### 2、坐标之间求距离，将距离小于15米的坐标归为一个坐标
### 3、选取每个学生相邻两天的相同时间段的轨迹，提取特征，计算DTW值。
### 4、将每个同学的DTW值和GPA做相关性分析，从而分析GPA与学生的活动轨迹有无关联。
### 5、通过线性回归的方式预测同学的成绩。

## 四、数据分析
### 1、分时间段描绘同学的路径
#### 画图主要代码如下：
``` R
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
```
#### 一天的轨迹如下：
![数据如下所示:](https://github.com/cuit201608/Team2/blob/master/第四次作业/截图/一天的轨迹.png)

### 2、分时间段分析轨迹
#### 14点至20点轨迹和21点至24点轨迹如下：
![数据如下所示:](https://github.com/cuit201608/Team2/blob/master/第四次作业/截图/14至24点两个时间段路径对比.png)
#### 14点至21点轨迹如下：
![数据如下所示:](https://github.com/cuit201608/Team2/blob/master/第四次作业/截图/14点至21点的轨迹.png)

### 3、对路径进行聚类分析
#### 步骤如下：
![数据如下所示:](https://github.com/cuit201608/Team2/blob/master/第四次作业/截图/聚类方法.png)
#### 主要代码如下：
``` R
firsttimes<-data.frame() #保存在进行聚类分析时用过的时间、经纬度
latertimes<-data.frame() #保存聚类之后的点的时间、经纬度
for (axis1 in 1:nrow(timesinterval))
{
  for (axis2 in (axis1+1):nrow(timesinterval))
  {
    if((timesinterval[axis1,1]%in%firsttimes$time)==TRUE)
      break
    
    t1=c(timesinterval[axis1,2],timesinterval[axis1,3])
    t2=c(timesinterval[axis2,2],timesinterval[axis2,3])
    aa=rbind(t1,t2)
    distance=data.frame(distm(aa))
    if(distance[1,2]<15)
    {
   firsttimes<rbind(firsttimes,data.frame(time=timesinterval[axis2,1],latitude=timesinterval[axis2,2],longitude=timesinterval[axis2,3]))
    }
  }
  if((timesinterval[axis1,1]%in%firsttimes$time)==FALSE)
  {
   latertimes<rbind(latertimes,data.frame(time=timesinterval[axis1,1],latitude=timesinterval[axis1,2],longitude=timesinterval[axis1,3]))
  }
}
```
#### 下图为没有进行聚类的路径
![数据如下所示:](https://github.com/cuit201608/Team2/blob/master/第四次作业/截图/12点至24点的轨迹.png)
#### 下图为以15米为阈值的聚类后的路径
![数据如下所示:](https://github.com/cuit201608/Team2/blob/master/第四次作业/截图/15米为阈值的聚类.png)

### 4、对同学的成绩进行预测
#### 线性回归定义如下：
![数据如下所示:](https://github.com/cuit201608/Team2/blob/master/第四次作业/截图/线性回归定义.png)
#### 主要代码如下：
``` R
#建立线性回归模型
library("car")
GPA_train<-lm(GPA~特征值,data=studenttotal)
summary(GPA_train)
#预测成绩
predict(GPA_train,studenttotal["特征值"])
studentpredict<-data.frame(studenttotal,预测的GPA=predict(GPA_train,studenttotal["特征值"]))
```
#### 预测结果如下：
![数据如下所示:](https://github.com/cuit201608/Team2/blob/master/第四次作业/截图/成绩预测.png)

## 五、实验结论
### 1、通过对聚类前和聚类后的数据对比：部分学生的GPA和DTW呈负相关关系，也就是说部分学生的GPA越大，其DTW越小。即是说部分GPA越大的同学，其行为轨迹越规律。

### 2、本次实验划分的时间段为0点至12点，12点至24点，一天当中只划分了两个时间段，相对来说比较粗糙，没有划分得很精细，如果再将时间段划分精细一点的话，预测：学生的GPA和DTW会呈负相关关系。

### 3、通过对学生的GPA和DTW建立线性回归模型，从而对成绩进行预测，通过预测的成绩可知：DTW小，则GPA小；DTW大，则GPA大。
