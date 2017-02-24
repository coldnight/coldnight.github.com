Title: AppArmor引起的无法启动Evince
Tags: Ubuntu,evince,apparmor
Category: Linux
Date: 2012-11-15 16:05
今天在Ubuntu上使用文档查看器(Evince),总是打开没有响应,在命令行下使用命令:
evince
却提示:
```
No protocol specified
Cannot parse arguments: 无法打开显示：
```
google说是$XAUTHORITY权限的问题,于是查看:
```bash
ls -l $XAUTHORITY
-rw------- 1 vim vim 51 2012-11-15 12:12 /data/home/vim/.Xauthority
```
更改为如下:
```bash
chmod +rw $XAUTHORITY
```
还是不行,这时候猛然想起查看日志,日志有如下一行:
```log
Nov 15 15:48:53 Vostro kernel: [13010.203241] type=1400 audit(1352965733.221:74): apparmor="DENIED" operation="open" parent=1 profile="/usr/bin/evince" name="/data/home/vim/.Xauthority" pid=7088 comm="evince" requested_mask="r" denied_mask="r" fsuid=1000 ouid=1000
```
好吧,问题是一个叫做AppArmor的内核模块引起的,它阻止了evince读取.Xauthority,找到配置文件,添加:
```bash
sudo vim /etc/apparmor.d/usr.bin.evince 
@{HOME}/.Xauthority rw,
sudo /etc/init.d/apparmor restart
```
做完这些还是刷同样的日志,无奈只有禁用它了:
```bash
sudo ln -sf /etc/apparmor.d/usr.bin.evince /etc/apparmor.d/disable/
```
重启apparmor就能打开Evince了,都不知道这玩意是怎么加上的,之前都没问题.

此次解决看来Linux问题解决之路是从日志开始的,长时间Linux桌面工作差点都给忘掉日志的重要性.
