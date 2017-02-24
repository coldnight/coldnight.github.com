Title: iptables:restorecon: command not found无法保存策略解决
Tags: 防火墙,策略,无法保存,restorecon,iptables
Category: Linux
Date: 2012-04-30 10:43
今天更新了几条iptables,但是运行命令`service iptables save`保存时确提示`iptables: Saving firewall rules to /etc/sysconfig/iptables: /etc/init.d/iptables: line 268: restorecon: command not found`

好吧,很明显的错误,找不到restorecon命令. 查找一下:
```
which restorecon
whereis restorecon
```
没结果,继续手动查看:
```
ls -l /sbin/ ¦ grep restore
lrwxrwxrwx 1 root root     14 Apr 19 09:58 iptables-restore -> iptables-multi
```
有一条但是不是我们想要的,猜测大概是包没装
先yum搜索一下吧:
```
yum search restorecon
```
好吧依然没有结果.google一下说是缺少policycoreutils这个包.安装这个包:
```
yum -y install policycoreutils
```
然后查看:
```
ls /sbin/ -l ¦grep restore
lrwxrwxrwx 1 root root     14 Apr 19 09:58 iptables-restore -> iptables-multi
lrwxrwxrwx 1 root root      8 Apr 30 10:31 restorecon -> setfiles
```
然后运行命令保存iptables策略:
```
service iptables save
iptables: Saving firewall rules to /etc/sysconfig/iptables: [  OK  ]
```
只是一篇笔记,记录问题方便以后遇到问题后查找.有点乱.没多少内容.
