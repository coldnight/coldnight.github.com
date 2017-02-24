Title: python 使用pyxmpp2编写gtalk群
Tags: 群,xmpp,pyxmpp2,python,gtalk,bot
Category: Python
Date: 2012-10-29 14:48
gtalk是一款google开发的基于xmpp协议的聊天软件,其优点就是协议开源,我们可以通过任何支持xmpp的客户端协议来链接gtalk,但是gtalk不支持群聊天,所以各路高手都会自己来开发一个机器人来支持群功能.

其实主要原理就是机器人接收到消息后再将消息广播出去,从而达到群的效果.

python有两个模块可以用来支持xmpp,分别是pyxmpp和pyxmpp2,之前也用pyxmpp写了一个,功能和兼容性不是很好,经常出现问题,所以又使用pyxmpp2重写了一遍,今天修复了一些bug,所以公布出来,大家可以下载测试,也可以加入我们使用gtalk进行群交流,

我们的gtalk机器人是:`clubot@vim-cn.com`

喜欢gtalk,同时喜欢Linux/Python/Vim等爱好者的同学可以加进来交流

如果对我们的代码比较感兴趣,可以访问github,我们将代码放在了github上:https://github.com/coldnight/clubot

下面介绍一下安装:

环境为:python 2.7, 因为有少量的shell所以系统需要Linux,也可稍作更改支持windows

下载源码:
```bash
git clone git://github.com/coldnight/clubot.git
```
安装依赖:
```bash
easy_install pyxmpp2
```
修改settings.py文件,填入bot的账户和密码,执行:
```bash
python clubot.py
```
即可开启群bot,群bot支持翻译,天气查询,贴代码等等待功能.

如果您想贡献代码可以加入我们的bot群:`clubot@vim-cn.com`

如果您有bug可以提交在评论里
