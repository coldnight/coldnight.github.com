Title: Python 断点调试
Tags: 调试,断点,python,pdb,debug
Category: Python
Date: 2012-08-10 16:16
## pdb模块
pdb是一个Python 内置的调式模块这里用来介绍用它进行断点调试

### 插入断点
在需要插入断点的地方插入如下代码可以插入一个断点
import pdb; pdb.set_trace()
当Python执行到这条语句时在运行shell里就会中断执行出现一个类似下面的shell窗口
```
> # 这里会出现当前运行程序的信息,源文件和当前函数
-> # 这里是将要运行的语句
 (Pdb)
```
### pdb指令
进入(Pdb)后有很多命令可以使用,可以使用 `h` 查看帮助

* l 查看代码上下文
* p var 监视变量var
* n 单步执行
* b line  在line行插入断点
* c 继续到下一个断点,没有则执行程序
* r 执行到函数返回前
