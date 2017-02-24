Title: 使用cacti构建监控系统
Tags: 监控,lnmp,Linux,CentOS,cacti
Category: 监控
Date: 2012-03-14 14:02
Cacti是一套基于PHP,MySQL,SNMP及RRDTool开发的网络流量监测图形分析工具,它是通过 snmpget来获取数据，使用 RRDtool绘画图形,可以指定每一个用户能查看树状结构、host以及任何一张图，还可以与LDAP结合进行用户验证，同时也能自己增加模板，功能非常强大完善。界面友好。

cacti是用php语言实现的一个软件，它的主要功能是用snmp服务获取数据，然后用rrdtool储存和更新数据，当用户需要查看数据的时候用rrdtool生成图表呈现给用户。因此，snmp和rrdtool是cacti的关键。Snmp关系着数据的收集，rrdtool关系着数据存储和图表的生成。

cacti依赖于PHP+MYSQL环境,前面的几篇文章已经详细讲解了如何在Linux部署LNMP环境,这篇文章我们就是用前面几章所搭建的环境,所以这里不再讲解如何搭建环境,如果您不会可以先看看前几篇文章.

下面就介绍如何来部署cacti.

首先介绍本文所使用的环境:
server:

系统为CentOS 5.5 32bit

ip:192.168.3.120

cacti:cacti-0.8.7i.tar.gz

cacti使用SNMP采集数据,首先安装snmp数据采集工具:
```bash 
yum -y install lm_sensors net-snmp net-snmp-utils
```
同时cacti又依赖于rrdtool生成图表所以首先安装rrdtool:
```bash 
wget http://oss.oetiker.ch/rrdtool/pub/rrdtool-1.4.7.tar.gz
yum -y install cairo-devel glib2-devel pango-devel intltool   # 安装rrdtool依赖
./configure --prefix=/usr/local
make &amp;&amp; make install
```
下面就来下载cacti安装:
```bash 
wget http://www.cacti.net/downloads/cacti-0.8.7i.tar.gz
tar -zxvf cacti-0.8.7i.tar.gz
cp -r cacti-0.8.7i /usr/local/nginx/html/cacti  # 复制到html目录
useradd cactiuser -M -s /sbin/nologin          # 创建cacti用户
chown -R cactiuser.cactiuser /usr/local/nginx/html/cacti/rra/ # 改变属主和属组
chown -R cactiuser.cactiuser /usr/local/nginx/html/cacti/log/
```
然后进入到数据库创建cacti数据和创建一个用户:
```bash 
create database cactidb default character set utf8;   #创建数据库
grant all on cactidb.* to cactiuser@localhost identified by '123456'; # 创建一个mysql用户
use cactidb   # 使用刚才创建的数据库
source /usr/local/nginx/html/cacti/cacti.sql # 导入cacti数据
```
接下来我们编辑cacti配置文件/usr/local/nginx/html/cacti/include/config.php
```bash 
cd /usr/local/nginx/html/cacti/
vi include/config.php
```
编辑下面内容:
```bash 
$database_type = "mysql";            # 数据库类型
$database_default = "cactidb";       # 数据库名字
$database_hostname = "localhost";    # 数据库主机
$database_username = "cactiuser";    # 数据库用户
$database_password = "123456";       # 数据库密码
$database_port = "3306";             # 数据库端口
$database_ssl = false;
```
然后修改nginx配置文件像下面:
```bash 
        location / {
            root   html;
            index  index.php;
        }

        location ~ \.php$ {
            fastcgi_pass        unix:/var/run/php-fpm/php-fpm.sock;
            fastcgi_index       index.php;
            fastcgi_param SCRIPT_FILENAME $document_root/$fastcgi_script_name;
            include fastcgi_params;
            include fastcgi.conf;
        }
```
重启nginx
```bash 
pkill -9 nginx
/usr/local/nginx/sbin/nginx
```
然后设置php时区,
```bash 
cd /usr/local/nginx/html/cacti/
vi include/global_constants.php
```
在第二行添加
```bash 
date_default_timezone_set("Asia/Chongqing");
```
接下来配置snmp,编辑/etc/snmp/snmpd.conf
```bash 
vi /etc/snmp/snmpd.conf
```
然后找到41行将public改成一个较为复杂的名字:
```bash 
com2sec notConfigUser  default       public
```
然后找到62行
```bash 
access  notConfigGroup ""      any       noauth    exact  systemview none none
```
将systemview改成all:
```bash 
access  notConfigGroup ""      any       noauth    exact  all none none
```
然后去掉85行的注释:
```bash 
view all    included  .1                               80
```
保存配置文件后启动snmp
```bash 
service snmpd start
```
然后在浏览器里输入:http://192.168.3.120/cacti/
然后根据提示一步步安装,安装好后使用admin密码admin登录.如果点击graphs不能显示图像的话执行:
```bash 
php /usr/local/nginx/html/cacti/poller.php   # nginx下不会自动生成*.rrd文件必须手动执行这条命令才会生成,Debug没报错,测试权限也没问题,不知道怎么回事,望知道的能告知小弟
```
为了方便把这句加入到cron,执行:
```bash 
yum -y install vixie-cron  #安装
service crond start
crontab -e
```
添加如下内容:
```bash 
*/5 * * * * /usr/bin/php /usr/local/nginx/html/cacti/poller.php
```
好了这时候我们就可以打开查看生成的图像了.
注意如果报如下错误:
Call to undefined function session_unregister()
将session_unregister('username') 改成
$_SESSION['username']='';
