Title: 用pxe安装自己定制的发行版快速部署系统环境
Tags: 网络安装,网卡启动,无人值守,pxe,Linux,cfengine
Category: Linux
Date: 2012-02-02 11:37
自己的发行版出来之后如何快速应用到企业中,可以使用PXE网络安装实现无人值守安装. 先解释下PXE安装,这是由Intel开发的一项技术,支持通过网络从远程服务器下载启动文件系统本机,从而实现网络安装操作系统.

这里需要网络传输就需要ip,所以需要DHCP服务器,传输文件需要简单的传输协议就需要tftp服务器,安装的过程中需要传输安装文件就需要一个文件服务器(ftp/http/nfs等)
基本流程就是通过DHCP获取ip并获取下一步的服务器地址(也就是tftp),然后下载启动文件,通过启动文件获取指定的下一个服务器的地址(安装文件的远程路径)

使用环境:CentOS 5.5 32位

使用系统:根据CentOS定制的发行版ColdOS 0.1 32位

ip: 静态ip192.168.3.1

本文使用dhcpd分配ip,使用tftp-server提供下载启动文件,apache提供下载安装文件
1.安装配置tftp

安装tftp-server
```bash 
yum -y install tftp-server
```
配置tftp-server,编辑/etc/xinetd.d/tftp
```bash 
vi /etc/xinetd.d/tftp
```
修改配置文件

&nbsp;
```bash 
service tftp
{
        socket_type             = dgram
        protocol                = udp
        wait                    = yes
        user                    = root
        server                  = /usr/sbin/in.tftpd
        server_args             = -s /tftpboot      # 根目录
        disable                 = no                # 把yes改为no启动tftp
        per_source              = 11
        cps                     = 100 2
        flags                   = IPv4
}
```
启动tftp
```bash 
service xinetd restart
Stopping xinetd: [  OK  ]
Starting xinetd: [  OK  ]
```
2.在tftp根目录下创建启动文件供客户端下载:
```bash 
# 复制网卡启动文件
cp /usr/lib/syslinux/pxelinux.0 /tftpboot
```
将安装光盘上/image/pxeboot/initrd.img和vmlinux复制到/tftpboot/中
```bash 
# 挂载光盘
mount /dev/hdc /mnt
# 复制文件
cp /mnt/isolinux/vmlinuz /tftpboot/
cp /mnt/isolinux/initrd.img /tftpboot/
```
创建pxe启动菜单:
```bash 
# 创建存放启动菜单的目录
cd /tftpboot
mkdir pxelinux.cfg
# 从安装光盘内复制启动菜单命名为default
cp /mnt/isolinux/isolinux.cfg /tftpboot/pxelinux.cfg/default
```
编辑启动菜单:
```bash 
vi /tftpboot/pxelinux.cfg/default
default ColdOS
prompt 1
timeout 600
label ColdOS
  kernel vmlinuz
  append ks=http://192.168.3.1/cold/isolinux/ks.cfg initrd=initrd.img text # 指定ks文件位置
```
3.安装配置dhcp服务器
安装
```bash 
yum -y install dhcp
```
配置
```bash 
# 创建配置文件
cp /usr/share/doc/dhcp-3.0.5/dhcpd.conf.sample /etc/dhcpd.conf
# 编辑配置文件
vi /etc/dhcpd.conf
# 修改配置文件:
ddns-update-style interim;
ignore client-updates;
next-server 192.168.3.1;             # 指定tftp服务器
filename "/pxelinux.0";              # 指定启动文件

subnet 192.168.3.0 netmask 255.255.255.0 {

        option routers                  192.168.3.1;
        option subnet-mask              255.255.255.0;

        option nis-domain               "domain.org";
        option domain-name              "domain.org";
        option domain-name-servers      192.168.1.1;

        option time-offset              -18000; # Eastern Standard Time

        range dynamic-bootp 192.168.3.128 192.168.3.254;
        default-lease-time 21600;
        max-lease-time 43200;

}
# 启动dhcp服务器
service dhcpd start
Starting dhcpd: [ OK ]
```
4.安装配置http服务器
安装:
```bash 
yum -y install httpd
```
首先把安装光盘里的内容拷到本地服务器上:
```bash 
# 创建目录
mkdir -p /pxe/cold/
# 复制文件
( cd /mnt/ &amp;&amp; tar -cf - . ) ¦ (cd /pxe/cold &amp;&amp; tar -xvf - )
```
配置apache
```bash 
# 编辑配置文件:
vi /etc/httpd/conf/httpd.conf
# 更改apache根目录(将配置文件中的两处/var/www改成/pxe)
DocumentRoot "/pxe"
&lt;Directory /&gt;
    Options FollowSymLinks
    AllowOverride None
&lt;/Directory&gt;
&lt;Directory "/pxe"&gt;
    Options Indexes FollowSymLinks
    AllowOverride None
    Order allow,deny
    Allow from all
&lt;/Directory&gt;
```
配置完成后启动apache
```bash 
service httpd start
```
5.更改ks文件(ks文件可在CentOS图形界面下用kickstart程序直接生成,这里不做介绍)

因为网络安装,又需要部署cfengine的话,就需要更改cfengine的获取方式,首先要将文件夹打包:
```bash 
cd /pxe/cold
tar -cvf cfengine.tar cfengine/
```
然后编辑ks文件
```bash 
# 编辑ks文件
vi /pxe/cold/isolinux/ks.cfg
install
cdrom
lang en_US.UTF-8
keyboard us
rootpw --iscrypted $1$OKNHES6P$tPdz9HxIp6.QUvulqxNwa.
firewall --disable
authconfig --enableshadow --enablemd5
selinux --disabled
timezone --utc Asia/Chongqing
bootloader --location=mbr --driveorder=sda
url --url http://192.168.3.1/cold/              # 添加指定安装文件的语句
# The following is the partition information you requested
# Note that any partitions you deleted are not expressed
# here so unless you clear all partitions first, this is
# not guaranteed to work
clearpart --linux --drives=sda
part / --fstype ext3 --size=1 --grow --maxsize=4096
part /usr --fstype ext3 --size=1 --grow
part swap --size=1 --grow --maxsize=512

%packages
@core
@useful
@cfengine

%post --nochroot
mkdir -p /mnt/sysimage/var/cfengine/masterfiles
mkdir -p /mnt/sysimage/var/cfengine/inputs
mkdir -p /mnt/sysimage/var/cfengine/outputs
mkdir -p /mnt/sysimage/var/cfengine/bin

wget  http://192.168.3.1/cold/cfengine.tar    # 更改cfengine获取方式
tar -xvf cfengine.tar
cp /cfengine/sbin/cf-* /mnt/sysimage/var/cfengine/bin/
cp -r /cfengine /mnt/sysimage/usr/local/
```
6.测试
开启另一台设备测试,机器启动时一般按F12即可进入网卡启动.
