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
