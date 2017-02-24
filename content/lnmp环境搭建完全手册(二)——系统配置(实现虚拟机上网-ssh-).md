Title: lnmp环境搭建完全手册(二)——系统配置(实现虚拟机上网/ssh/)
Tags: 虚拟机,上网,ssh,NAT,lnmp
Category: Linux
Date: 2012-03-10 11:31
上一篇介绍了如何安装Linux,安装Linux后我们如果只在终端界面下配置会很麻烦,我们可以通过windows连接linux的ssh进行配置linux.SSH 是目前较可靠，专为远程登录会话和其他网络服务提供安全性的协议。

要想连接ssh,首先就需要保证windows能和虚拟机Linux进行通信,我们如果想要本机能和虚拟机通信需要设置虚拟机的网卡连接关系,有3中连接关系可以和本机连通:
一种是Bridged(桥接),就是使用你本机的连接,如果本地用的是局域网DHCP上网可以选择这种方式.
一种是NAT,就是用NAT模式，就是让虚拟系统借助NAT(网络地址转换)功能，通过宿主机器所在的网络来访问公网。如果本地用的是静态公网ip,本地有无DHCP这个是首选
一种host-only:这种模式不能访问外网,只能何宿主(也就是本机)主机通信.也就不能访问Internet

这里介绍如何设置NAT模式上网.按说网卡选择NAT模式直接重启网卡就可以dhcp获取,但是我们在以后配置Linux服务器的时候为了方便管理肯定不会使用DHCP,所以我们使用静态ip的方式来设置NAT上网.
首先右键你的虚拟机标签-&gt;Setting-&gt;选中Network Adapter.在右边选中NAT(也可直接双击右下角的网卡图标)
![www.linuxzen.com](/upload/QQ截图20120310101223.jpg)

![www.linuxzen.com](/upload/QQ截图20120310101241.jpg)

做完这步之后我们还要编辑一下虚拟机的虚拟卡设置,以达到我们使用静态ip上网的需求:

点击虚拟机的Edit(编辑)-&gt;Virtual Network Editor(虚拟网卡编辑器):
![www.linuxzen.com](/upload/QQ截图20120310101559.jpg)
在弹出的界面选中VMnet8 NAT,然后修改最下面的Subnet IP,改成你想要的网段,这里使用`192.168.3.0/24`.
![www.linuxzen.com](/upload/QQ截图20120310101834.jpg)

然后点击NAT Settting:设置Gateway IP为`192.168.3.0`网段的一个ip,这里使用`192.168.3.254`.
![www.linuxzen.com](/upload/QQ截图20120310102222.jpg)

设置好后然后点击OK保存退出.

一切已经就绪,下面我们就开始我们的Linux之旅.首先进入系统,在login:界面输入root用户,然后输入安装时输入的root密码.进入系统.首先我们来配置网卡.CentOS的网卡路径在"/etc/sysconifg/network-scripts/"下面,第一个网卡一般命名为eth0,网卡配置文件则是ifcfg-eth0,第二个是eth1,配置文件ifcfg-eth1,后面的以此类推.下面我们来配置网卡:
```bash 
vi /etc/sysconfig/network-scripts/ifcfg-eth0
```
(在这里说一下,Linux有一个很方便的功能当路径太长不方便记忆时,可打出一部分敲Tab键如果这个是唯一的就会自动补全,如果不是就敲两下Tab,就会列出所有可选内容,文件和命令一样)
按i进入输入模式,将配置文件改成如下内容:
```bash 
DEVICE=eth0                   # 设备名
HWADDR=00:00:00:00:01         # MAC地址
BOOTPROTO=static              # IP获取方式为static,如果是dhcp则是使用DHCP获取
ONBOOT=yes                    # 是否启用网卡(也就是是否在重启设备或重启网卡时启动这个网卡)
IPADDR=192.168.3.120          # IP地址
NETMASK=255.255.255.0         # 掩码
GATEWAY=192.168.3.254         # 网关(也就是刚才虚拟网卡编辑器里NAT Setting里设置的网关)
```
编辑完成后按Esc进入末行模式然后输入:wq保存退出
然后重启网卡,如果都是[OK]的就没有问题
```bash 
service network restart
```
查看网络配置的命令是
```bash
ifconfig
```
![www.linuxzen.com](/upload/QQ截图20120310104548.jpg)

ping命令用来检测网络连通性,配置好后我们ping网关测试:
```bash 
ping 192.168.3.254
```
如果显示如下则表示是通的,如果不是则表示不通
![www.linuxzen.com](/upload/QQ截图20120310104753.jpg)
如果能正常访问网络我们还需要配置一个DNS服务器,指定一个DNS服务器的配置文件为:/etc/resolv.conf.Linux可以配置3个DNS.
```bash 
vi /etc/resolv.conf
```
同样按i或a进入输入模式添加如下内容:
```bash 
nameserver    192.168.3.254    # 本地DNS
nameserver    202.106.0.20     # 北京地区网通DNS
nameserver    8.8.8.8          # google的根DNS
```
然后按Esc,输入:wq保存退出,然后ping [www.linuxzen.com](http://www.linuxzen.com)看能否解析:
![www.linuxzen.com](/upload/未命名.jpg)
上图表示解析成功.

好了到这一步Linux已经可以访问internet了.下面我们就配置ssh连接虚拟机.连接ssh的软件有几款都是比较不错的,个人觉得最好的还是Secure CRT,这个是收费的但较低版本还是可以在百度/谷歌上找到特别码(你懂得),还有一款是putty,这一个款开源软件而且只有几百KB,很轻量级.

我们首先要关闭selinux(一款美国国家安全局贡献出来的软件,如果会配置可以开启,如果不会就关闭)
编辑/etc/selinux/config:
```bash 
vi /etc/selinux/config
```
然后按i进入输入模式,修改SELINUX参数为disabled:
```bash
# This file controls the state of SELinux on the system.
# SELINUX= can take one of these three values:
#       enforcing - SELinux security policy is enforced.
#       permissive - SELinux prints warnings instead of enforcing.
#       disabled - SELinux is fully disabled.
SELINUX=disabled   # 修改此行
# SELINUXTYPE= type of policy in use. Possible values are:
#       targeted - Only targeted network daemons are protected.
#       strict - Full SELinux protection.
SELINUXTYPE=disabled

```
然后重启系统
```bash 
reboot
```
然后清空系统自带防火墙
```bash 
iptables -Z
iptables -F
iptables -X
service iptables save
```
然后我们就可以使用ssh连接Linux虚拟机
首先确保Linux启动了ssh,ssh占用TCP的22端口.执行命令查看22端口是否开放:
```bash 
netstat -antlp | grep 22
```
如果出现如下内容则表示ssh已经启动:
![www.linuxzen.com](/upload/QQ截图20120310111326.jpg)
如果没有启动尝试运行下面命令:
```bash 
service sshd start        # 启动ssh服务
chkconfig --add sshd      # 加入开机启动
```
如果没报错则sshd启动成功

ssh也配置成功了,下面就需要本地连接ssh了,首先我们要配置本地能访问虚拟机,不知道大家注意到们装完虚拟机我们多出两个链接分别是VMware Network Adapter VMnet1和VMware Network Adapter VMnet8,VMnet1是host only,VMnet8是NAT,我们使用的是NAT,所以配置VMware Network Adapter VMnet8就可以了,把VMware Network Adapter VMnet8配置成192.168.3.x的ip,这里使用192.168.3.111,配置过程这里不做解释了.在windows的cmd下ping 192.168.3.120,能ping通就打开你的ssh客户端软件.这里使用SecureCRT.打开SecureCRT点击快速连接:
![www.linuxzen.com](/upload/QQ截图20120310112142.jpg)
在弹出的界面输入主机名处输入ip地址:192.168.3.120,用户名处输入root:然后点击连接
![www.linuxzen.com](/upload/QQ截图20120310112351.jpg)

会弹出是否保存密钥:点击接收并保存:
![www.linuxzen.com](/upload/QQ截图20120310112605.jpg)

然后输入root密码既可以连接登录
![www.linuxzen.com](/upload/QQ截图20120310112713.jpg)

![www.linuxzen.com](/upload/QQ截图20120310112752.jpg)
好了到这里就可以用ssh连接Linux了,尽情探索把,下一节将会讲解yum库的配置和包安装.
