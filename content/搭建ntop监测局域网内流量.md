Title: 搭建ntop监测局域网内流量
Tags: 监控,流量,局域网,ntop,Linux
Category: 监控
Date: 2012-02-20 17:37
ntop是一种监控网络流量的工具，用ntop显示网络的使用情况比其他一些网管软件更加直观、详细。ntop甚至可以列出每个节点计算机的网络带宽利用率。ntop是一个灵活的、功能齐全的，用来监控和解决局域网问题的工具。它同时提供命令行输入和Web界面，可应用于嵌入式Web服务。

本文环境:CentOS5.5 32位

* ip:192.168.3.101
* rrdtool:rrdtool-1.4.7.tar.gz
* GeoIP:GeoIP-1.4.8.tar.gz
* ntop:ntop-4.1.0.tar.gz
## 一.安装依赖: ##
### 安装rrdtool
```bash 
yum -y install libxml2 glib-devel pango-devel
wget http://oss.oetiker.ch/rrdtool/pub/rrdtool-1.4.7.tar.gz                  # 下载
tar -zxvf rrdtool-1.4.7.tar.gz
cd rrdtool-1.4.7
./configure &amp;&amp; make &amp;&amp; make install
```
### 安装geoip:
```bash 
wget http://geolite.maxmind.com/download/geoip/api/c/GeoIP-1.4.8.tar.gz
tar -zxvf GeoIP-1.4.8.tar.gz
cd GeoIP-1.4.8
./configure &amp;&amp; make &amp;&amp; make install
```
### 安装其余依赖:
```bash 
yum -y install libtool libpcap-devel gd-devel gdbm-devel openssl-devel
 intltool
```
&nbsp;
## 二.下载安装: ##
```bash 
wget http://sourceforge.net/projects/ntop/files/ntop/Stable/ntop-4.1.0.tar.gz/download
tar -zxvf ntop-4.1.0.tar.gz
cd ntop-4.1.0
./autogen.sh --with-tcpwrap --with-rrd-home=/opt/rrdtool-1.4.7/
make &amp;&amp; make install
```
## 三.配置启动
更改检测数据保存目录的权限:
```bash 
chown -R nobody /usr/local/var/ntop/
```
创建admin管理密码:
```bash 
ntop -A
```
在eth0上启动ntop:
```bash 
ntop -d -i eth0 -M
```
客户端浏览监测结果:
在浏览器上输入:http://192.168.3.101:3000访问即可查看监测结果,Admin可以配置ntop.到此这个监控局域网流量的ntop就搭建完成,这样就可以很方便的查看局域网流量,可以发现是否有人在执行arp攻击等等常见攻击.剩余的功能大家就自己发现吧,这里不错介绍.

如果英语阅读困难的话可以,有牛人已经做出来了中文版,大家可以下载安装.
