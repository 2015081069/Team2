# 第一次实验报告
## (数据说明：本次作业选取的数据均为4月11日所有学生的座次数据 )
## 一、数据处理
### 1、画教室座位表
``` R
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
```
