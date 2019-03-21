nrow()#判断有多少行
ncol()#判断有多少列

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
for (i in 1 : 96) {
  if(i!=17&i!=33&i!=49&i!=65&i!=81){
    #给viewport取名字方便搜索，即以座位号为名字
    vp1 <- viewport(x = xvec, y = yvec, width = 0.05, height = 0.05,name = as.character(i))#整数转换为字符
    pushViewport(vp1)
    grid.rect()
    grid.text(i)
    upViewport() #回到上一个viewport（即回到的父节点）,不然现在的长宽会受前一个viewport影响
    xvec<-xvec+0.05
  }
  else{
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

newdata<-read.csv(file = "D:/大三下/空间数据库/课件(新)/seat.csv",header = T,sep=",")
number<-"X4月11日" #所查对象的列名
#填涂查找的同学所坐的座位
for(i in 1 : nrow(newdata))
{
  if(is.na(newdata[i,number])==FALSE) #判断位置上值是否为缺省值（R语言中，NA代表位置上的值为空，NULL代表连位置都没有，变量为空。）
  {
    seekViewport(as.character(newdata[i,number]))#查找对应的Viewport对象
    grid.rect(gp=gpar(col="gray29",fill="green"))#col表示边框及字体颜色，fill表示填充颜色
    grid.text(newdata[i,number])
  }
}
#计算平均值、方差
times1<-0 #记录存在的座位数
l1<-c(1,2,3,4,17,18,19,20,33,34,35,36)
l2<-c(5,6,7,8,21,22,23,24,37,38,39,40)
l3<-c(9,10,11,12,25,26,27,28,41,42,43,44)
l4<-c(13,14,15,16,29,30,31,32,45,46,47,48)
l5<-c(49,50,51,52,65,66,67,68,81,82,83,84)
l6<-c(53,54,55,56,69,70,71,72,85,86,87,88)
l7<-c(57,58,59,60,73,74,75,76,89,90,91,92)
l8<-c(61,62,63,64,77,78,79,80,93,94,95,96)
yflist<-list(l1,l2,l3,l4,l5,l6,l7,l8)
for(i in 1 : nrow(newdata))
{
  if(is.na(newdata[i,number])==FALSE) #判断位置上值是否为缺省值（R语言中，NA代表位置上的值为缺省值，NULL代表连位置都没有，变量为空。）
  {
    times1<-times1+1
  }
}
Mean1<-times1/8 #平均值
paste("平均值为:",Mean1)
Vartotal<-0 #方差子数的和
times2<-0 #记录每个样方中的点数
for (i in 1:8) 
{
  for(j in 1 : nrow(newdata))
  {
    if(is.na(newdata[j,number])==FALSE&newdata[j,number]%in%yflist[[i]]) #判断该座位是否在样方中，注意list[[i]]才表示嵌套list里面的第几个元素
    {
      times2<-times2+1
    }
  }
  Vartotal<-Vartotal+(times2-Mean1)^2
  times2<-0
}
Var1<-Vartotal/(8-1)
paste("方差为:",Var1)
VMR<-Var1/Mean1
paste("方差均值比为:",VMR)