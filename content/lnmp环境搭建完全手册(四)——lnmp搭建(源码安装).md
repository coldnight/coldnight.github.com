Title: lnmp环境搭建完全手册(四)——lnmp搭建(源码安装)
Tags: 源码包,安装,php,nginx,MySQL,Linux,CentOS
Category: Linux
Date: 2012-03-13 13:13
上面3篇我们主要对系统进行了安装和配置,并且配置了yum包管理器,安装了几个常用的工具,这篇我们就来介绍如何来搭建lnmp环境.这里的LNMP环境是指Linux下搭建Nginx+MySQL+PHP.Nginx是一个高性能的 HTTP 和 反向代理 服务器，也是一个 IMAP/POP3/SMTP 代理服务器。Nginx不仅可以作为web服务器,也可以作为负载均衡器,之前也有文章介绍,大家可以看一下.

MySQL是一款开源免费的数据软件,MySQL是一个小型关系型数据库管理系统，其体积小、速度快、总体拥有成本低，尤其是开放源码这一特点，许多中小型网站为了降低网站总体拥有成本而选择了MySQL作为网站数据库.

PHP，是英文超级文本预处理语言Hypertext Preprocessor的缩写。PHP 是一种 HTML 内嵌式的语言，是一种在服务器端执行的嵌入HTML文档的脚本语言，语言的风格有类似于C语言，被广泛的运用。


nginx当前最新稳定版是nginx-1.0.13
首先我们下载nginx,在Linux下执行下面命令:
```bash
cd /usr/src                                           # 一般软件源码放在这个目录下
wget http://nginx.org/download/nginx-1.0.13.tar.gz    # 下载
nginx会有几个依赖包,我们首先安装依赖,不要安装过程中会报错:
yum  -y install zlib-devel pcre-devel openssl-devel
```
一般源代码安装分4个步骤(有人也会省去第一个步骤),`解压`(tar命令)`预编译`(执行源码包下的configure),`编译`(make),`编译安装`(make install)
首先我们解压源码包:
```
tar -zxvf nginx-1.0.13.tar.gz
```
这里解释下加压参数,z代表gzip(也就是后面的.gz文件)x代表加压,v表示显示详细信息,-f使用档案文件或设备(必选参数)

然后我们进行预编译,一般预编译会带上一些参数,已达到我们想要安装的效果,比如启用某个功能,禁用某个功能:

进入源码包目录进行预编译:
```bash
cd nginx-1.0.13
 ./configure --prefix=/usr/local/nginx\   # 指定安装目录为/usr/local/nginx
--with-openssl=/usr/include/openssl\  # 启用ssl
--with-pcre\                          # 启用正规表达式
--with-http_stub_status_module        # 安装可以查看nginx状态的程序
```
其中./configure指执行当前目录下的`configure`文件

预编译完成后我们就可以进行编译和安装:
```
make           #编译
```
执行后make后会有大量输出,等待输出完成后如果没有报错就可以进行安装执行:
```
make install   #安装
```
安装完成后我们可以到相应的目录查看安装的文件:
```bash
ls /usr/local/nginx/
conf  html  logs  sbin
```
好了,下面我们启动nginx:
```
/usr/local/nginx/sbin/nginx
```
通过查看端口看nginx是否启动成功,nginx占用TCP的80端口,执行下面命令:
```
 netstat -antlp ¦ grep 80
tcp        0      0 0.0.0.0:80                  0.0.0.0:*                   LISTEN      5946/nginx
```
我们查看80端口是开放的

然后打开浏览器访问http://192.168.3.120,我们会看到Welcome to nginx(之前的版本是 It's Work):



nginx安装完毕后我们来安装MySQL ,我们使用MySQl-5.0.95版首先下载:
```
wget http://dev.mysql.com/get/Downloads/MySQL-5.0/mysql-5.0.95.tar.gz/from/http://mysql.cdpa.nsysu.edu.tw/
```
安装之前我们先做一些准备工作,

安装依赖:
```
yum -y install ncurses-devel
```
创建MySQL用户:
```bash
useradd -M -s /sbin/nologin mysql  # -M不创建home目录,-s指定shell为不登录
```
然后进行安装:
```bash
tar -zxvf mysql-5.0.95.tar.gz
cd mysql-5.0.95
./configure --prefix=/usr/local/mysql \
--without-debug \                                 # 取消调试模式提高性能
--with-extra-charsets=utf8,gbk \                  # 仅仅指定需要的默认字符集提高性能
--enable-assembler \                              # 使用汇编模式提高性能
--with-mysqld-ldflags=-all-static \               # 以静态方式编译提高性能
--with-client-ldflags=-all-static \
--with-unix-socket-path=/tmp/mysql.sock \         # 使用unix socket提高性能
--with-ssl
make
make install
```
安装完成后复制配置文件和启动脚本:
```bash
cp support-files/my-medium.cnf /etc/my.cnf         # 复制配置文件
cp support-files/mysql.server /etc/init.d/mysqld   # 复制启动脚本
chmod +x /etc/init.d/mysqld         # 给启动脚本执行权限
```
为了以后方便我们为所有的二进制可执行文件和动态链接库文件做一个软连接:
```bash
ln -s /usr/local/mysql/bin/* /usr/local/bin/              # 为可执行的二进制文件做软连接
ln -s /usr/local/mysql/lib/mysql/lib* /usr/lib/  # 为动态链接库做一个软连接
```
然后我们初始化数据库:
```bash
mysql_install_db --user=mysql  # 用MySQL用户安装数据库
```
为了MySQL能正常使用我们需要更改一下MySQL安装目录和MySQL的数据库目录的属主和属组:
```bash
chown -R root.mysql /usr/local/mysql/           # 更改安装目录属主为root,属组为mysql
chown -R mysql.mysql /usr/local/mysql/var/      # 更改数据库目录属主和属组都为mysql
```
这里的`-R`参数用来应用到所有子目录和文件

配置完毕后我们启动mysql:
```
service mysqld start
```
现在我们查看MySQL是否启动成功,MySQL占用TCP的3306端口,我们查看端口是否被占用:
```
netstat -antlp ¦ grep 3306
tcp        0      0 0.0.0.0:3306                0.0.0.0:*                   LISTEN      32143/mysqld
```
然后我们通过mysql命令来连接mysql:
```
mysql
```
会显示如下内容表示已经成功启动MySQL并已经连接上
```
Welcome to the MySQL monitor.  Commands end with ; or \g.
Your MySQL connection id is 1
Server version: 5.0.95-log Source distribution

Copyright (c) 2000, 2011, Oracle and/or its affiliates. All rights reserved.

Oracle is a registered trademark of Oracle Corporation and/or its
affiliates. Other names may be trademarks of their respective
owners.

Type 'help;' or '\h' for help. Type '\c' to clear the current input statement.

mysql>
```
MySQL安装完毕下面我们就来安装PHP,安装PHP前首先要安装几个源码包依赖:`libmcrypt` `mhash` `mcrypt`

首先来安装几个源码包依赖:
```bash
wget http://sourceforge.net/projects/mcrypt/files/Libmcrypt/2.5.8/libmcrypt-2.5.8.tar.bz2/download
tar -jxvf libmcrypt-2.5.8.tar.bz2   # 这个包是bz2的  使用-j参数解压
cd libmcrypt-2.5.8
./configure
make
make install
####################################################
wget http://sourceforge.net/projects/mhash/files/mhash/0.9.9.9/mhash-0.9.9.9.tar.bz2/download
tar -jxvf mhash-0.9.9.9.tar.bz2
cd mhash-0.9.9.9
./configure
make
make install
# 这两个包安装完成后要把动态链接库做一个软连接到/usr/lib,以为接下来的mcrypt依赖于这两个包
ln -s /usr/local/lib/libmcrypt* /usr/lib
ln -s /usr/local/lib/libmhash.* /usr/lib/
ln -s /usr/local/bin/libmcrypt-config /usr/bin/libmcrypt-config
###########################################################
wget http://sourceforge.net/projects/mcrypt/files/MCrypt/2.6.8/mcrypt-2.6.8.tar.gz/download
tar -zxvf mcrypt-2.6.8.tar.gz
cd mcrypt-2.6.8
./configure
make
make install
```
然后下载php:
```
wget http://cn2.php.net/get/php-5.4.0.tar.bz2/from/this/mirror
```
安装依赖:
```
yum –y install libxml2-devel curl-devel libpng-devel openldap-devel
```
我们使用nginx调用php的时候使用fpm的方式,在php 5.4中加入了对php-fpm的支持,所以就不需要打补丁了.安装PHP:
```
tar -jxvf php-5.4.0.tar.bz2
cd php-5.4.0
./configure --prefix=/usr/local/php --with-mysql=/usr/local/mysql/ --with-zlib --enable-xml --disable-rpath --enable-safe-mode --enable-bcmath --enable-shmop --enable-sysvsem --with-curl --with-curlwrappers --enable-fpm --enable-fastcgi --with-mcrypt --with-gd --with-openssl --with-mhash --enable-sockets --with-ldap --with-ldap-sasl --with-xmlrpc -enable-zip --enable-soap
make
make install
```
到这里整个LNMP已经安装完成.下面我们就配置php和nginx能运行php网站:
首先为php创建配置文件:
```
cp php.ini-production /usr/local/php/php.ini # 如果是开发就复制php.ini-development
cp /usr/local/php/etc/php-fpm.conf.default /usr/local/php/etc/php-fpm.conf
ln -s /usr/local/php/bin/php /usr/bin/
```
配置php-fpm,编辑php-fpm.conf
```
vi /usr/local/php/etc/php-fpm.conf
```
找到listen那一行,修改成如下内容:
```
listen = /var/run/php-fpm/php-fpm.sock   # 使用unix socket
```
启动php-fpm
```
mkdir /var/run/php-fpm
/usr/local/php/sbin/php-fpm
```
然后配置nginx,编辑nginx配置文件
```
vi /usr/local/nginx/conf/nginx.conf
```
修改nginx配置文件支持php:
```
    server {
        listen       80;
        server_name  localhost;

        #charset koi8-r;

        #access_log  logs/host.access.log  main;

        location / {
            root   html;
            index  index.php index.html index.htm;         # 添加index.php的首页文件
        }

        # 添加下面内容
        location ~ \.php$ {
            fastcgi_pass        unix:/var/run/php-fpm/php-fpm.sock;
            fastcgi_index       index.php;
            fastcgi_param SCRIPT_FILENAME $document_root/$fastcgi_script_name;
            include fastcgi_params;
            include fastcgi.conf;
        }
```
修改完毕后保存退出重启nginx:
```
pkill -9 nignx
/usr/local/nginx/sbin/nginx
```
然后在/usr/local/nginx/html下创建index.php,
```
vi /usr/local/nginx/html/index.php
```
添加下面内容:
```
<?php
phpinfo();
?>
```
保存退出后访问[http://192.168.3.120/index.php](),看到下面页面表示已经安装配置成功:

![linuxzen.com](/upload/QQ截图20120313133251.jpg)
到这里我们的LNMP环境就完全搭建成功,运行你的网站或者学习你的PHP吧.
