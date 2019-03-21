# rgrtg
tryjutyj

```
#使用Grid
library(grid)

#创建讲台
vp1 <- viewport(x = 0.52, y = 0.7, width = 0.1, height = 0.05)#x，y表示坐标
pushViewport(vp1)#将对象push到图形中
grid.rect(gp=gpar(col="gray29",fill="red"))#输出图形，其中col表示边框及字体颜色，fill表示填充颜色
grid.text("讲台")#添加文字
upViewport() #回到上一个viewport（即回到的父节点）,不然后一个的长宽会受前一个viewport影响
```
