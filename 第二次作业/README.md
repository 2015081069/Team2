# 第二次实验报告
## (数据说明：本次作业选取的数据均为4月12日所有学生的座次数据)
## 一、数据处理
### 1、画教室座位表并记录每个座位的坐标
``` R
library(lctools)
library(maptools)
library(spdep)


#使用Grid
library(grid)

#创建讲台
vp1 <- viewport(x = 0.52, y = 0.7, width = 0.1, height = 0.05)#x，y表示坐标
pushViewport(vp1)#将对象push到图形中
grid.rect(gp=gpar(col="gray29",fill="red"))#输出图形，其中col表示边框及字体颜色，fill表示填充颜色
grid.text("讲台")#添加文字
upViewport() #回到上一个viewport（即回到的父节点）,不然后一个的长宽会受前一个viewport影响

#创建座位
xvec <- 0.15
yvec <- 0.6
newdata<-data.frame() #创建newdata存取grid的x，y坐标和对应当天所坐学生的gpa
ddd<-data.frame()
for (i in 1 : 96) {
  if(i!=17&i!=33&i!=49&i!=65&i!=81){
    newdata[i,1]=xvec #存x坐标
    newdata[i,2]=yvec #存y坐标
    #给viewport取名字方便搜索，即以座位号为名字
    vp1 <- viewport(x = xvec, y = yvec, width = 0.05, height = 0.05,name = as.character(i))#整数转换为字符
    pushViewport(vp1)
    grid.rect()
    grid.text(i)
    upViewport() #回到上一个viewport（即回到的父节点）,不然现在的长宽会受前一个viewport影响
    xvec<-xvec+0.05
  }
  else{
    newdata[i,1]=xvec #存x坐标
    newdata[i,2]=yvec #存y坐标
    xvec <- 0.15
    yvec<-yvec-0.05
    vp1 <- viewport(x = xvec, y = yvec, width = 0.05, height = 0.05,name = as.character(i))
    pushViewport(vp1)
    grid.rect()
    grid.text(i)
    upViewport() #回到上一个viewport（即回到的父节点）,不然现在后一个的长宽会受前一个viewport影响
    xvec<-xvec+0.05
  }
}
```
### 效果图如下：
![教室座位表如下所示：](https://github.com/cuit201608/Team2/blob/master/第二次作业/结果图1.png)

### 2、填涂4月12日当天学生所坐座位并在座位标注学生的gpa
``` R

newdata1<-read.csv(file = "D:/大三下/空间数据库/课件(新)/seat.csv",header = T,sep=",") #创建newdata1存取表中的信息
gpa<-newdata123$gpa.all
number<-"X4月12日" #所查对象的列名
#填涂查找的同学所坐的座位
for(i in 1 : nrow(newdata1))
{
  if(is.na(newdata1[i,number])==FALSE) #判断位置上值是否为缺省值（R语言中，NA代表位置上的值为空，NULL代表连位置都没有，变量为空。）
  {
    newdata[newdata1[i,number],3]=gpa[i] #存座位所对应的gpa
    seekViewport(as.character(newdata1[i,number]))#查找对应的Viewport对象
    grid.rect(gp=gpar(col="gray29",fill="green"))#col表示边框及字体颜色，fill表示填充颜色
    grid.text(newdata1[i,number])
  }
}
```
### 效果图如下：
![学生所坐座位如下图所示：](https://github.com/cuit201608/Team2/blob/master/第二次作业/结果图2.png)

### 3、提取非空的座位
``` R
m<-0
#提取有位置的座位
for (i in 1:nrow(newdata)) 
{
  if(is.na(newdata[i,3])==FALSE)
  {
    m<-m+1
    ddd[m,1]=newdata[i,1]
    ddd[m,2]=newdata[i,2]
    ddd[m,3]=newdata[i,3]
  }
}
```

## 二、生成图像
``` R
newdata2<-data.frame(ddd$V1,ddd$V2) #创建newdata2存放座位的x，y坐标
spoint<-data.frame(cbind(newdata2$ddd.V1,newdata2$ddd.V2))
sptest <- SpatialPoints(spoint, proj4string = CRS("+proj=longlat +datum=WGS84"))
nbk1 <- knn2nb(knearneigh(sptest, k = 5, longlat = TRUE))
snbk1 <- make.sym.nb(nbk1)
plot(nb2listw(snbk1), cbind(spoint$X1 , spoint$X2))
```
### 效果图如下：
![生成的效果图如下所示：](https://github.com/cuit201608/Team2/blob/master/第二次作业/结果图4.png)

## 三、根据莫兰指数定义进行结果分析
``` R
moran.test(ddd$V3, nb2listw(snbk1))
```
![定义一如下：](https://github.com/cuit201608/Team2/blob/master/第二次作业/说明1.png)
![定义二如下：](https://github.com/cuit201608/Team2/blob/master/第二次作业/说明2.png)
### 实验结果如下：
![莫兰指数计算结果如下：](https://github.com/cuit201608/Team2/blob/master/第二次作业/结果图3.png)

## 根据分析结果可知：得出的莫兰指数小于0，但是也比较靠近0，所以同学们所坐的座位分布与他们的成绩不相关
