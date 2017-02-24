Title: Linux shell脚本调试技巧
Tags: 调试,技巧,shell,Linux
Category: shell
Date: 2012-01-17 17:04
有时候shell脚本不会给予明显的调试信息,而且有时不报错,但是脚本没有达到预期的效果这时候脚本调试就可以帮你准确定位错误.

在脚本的最顶部加上
```bash 
set -x
```
开启调试
在脚本的最底部加上
```bash 
set +x
```
关闭调试
如果在终端界面下,调试信息过多,调试信息是无法重定向到文件的,所以该怎样查看调试呢,当然这对SecureCRT连接的当然不是问题,但是如果是终端界面的话,可以使用命令

&nbsp;
```bash 
script
```
然后执行要捕捉内容的命令,完成后通过
```bash 
exit
```
退出,当前目录下会生成typescript,通过
```bash 
more typescript
```
查看调试命令.
