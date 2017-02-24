Title: nginx平台搭建nagios监控系统
Tags: 配置,监控,rewrite,nginx认证,nginx支持cgi,nginx,nagios,Linux
Category: 监控
Date: 2012-02-26 10:48
Nagios是一款开源的免费网络监视工具，能有效监控Windows、Linux和Unix的主机状态，交换机路由器等网络设置，打印机等。在系统或服务状态异常时发出邮件或短信报警第一时间通知网站运维人员，在状态恢复后发出正常的邮件或短信通知。

Nagios 功能:

- 监视网络服务 (SMTP, POP3, HTTP, NNTP, PING等)
- 监视主机资源 (进程, 磁盘等)
- 简单的插件设计可以轻松扩展Nagios的监视功能
- 服务等监视的并发处理
- 错误通知功能 (通过email, pager, 或其他用户自定义方法)
- 可指定自定义的事件处理控制器
- 可选的基于浏览器的WEB界面以方便系统管理人员查看网络状态，各种系统问题，以及日志等等
- 可以通过手机查看系统监控信息

本文环境:

* 系统:CentOS 5.5 32bit
* ip:192.168.3.3
* nagios core:nagios-3.3.1.tar.gz
* web:nginx(nginx-1.0.5.tar.gz)

## 安装: ##
### 安装准备 ###
创建nagios系统用户:
```bash 
useradd  nagios
```

安装依赖:
```bash 
yum -y install gd-devel libpng-devel libtool-ltdl-devel
```
下载nagios内核:
```bash 
wget http://prdownloads.sourceforge.net/sourceforge/nagios/nagios-3.3.1.tar.gz
```
### 安装nagios内核: ###
```bash 
tar -zxvf nagios-3.3.1.tar.gz
cd nagios
./configure --prefix=/usr/local/nagios
make all
make install               # 安装主要程序,CGI和HTML
make install-commandmode   # 使外部命令有访问nagios配置文件的权限
make install-config        # 安装配置文件
make install-init          # 安装nagios启动脚本
```
### 安装插件: ###
nagios没有什么都做不了的,插件极大的扩展了nagios功能,除了安装官方常用插件外,也可以根据自己需求编写插件.
```bash 
wget http://prdownloads.sourceforge.net/sourceforge/nagiosplug/nagios-plugins-1.4.15.tar.gz
tar -zxvf nagios-plugins-1.4.15.tar.gz
cd nagios-plugins-1.4.15
./configure --prefix=/usr/local/nagios
make  &amp;&amp; make install
```
###  安装WEB服务器 ###
web服务器不是nagios所必须的,但是如果你无法忍受通过日志文件来监控各个主机状态,那么web服务器就至关重要,所以我们还是花上一点时间为之后的节省更多的时间.

这里web服务器使用当前主流的nginx:

安装nginx
```bash 
wget http://nginx.org/download/nginx-1.0.11.tar.gz
&amp;nbsp;yum&amp;nbsp;&amp;nbsp;-y&amp;nbsp;install&amp;nbsp;zlib-devel&amp;nbsp;pcre-devel&amp;nbsp;openssl-devel &amp;nbsp;# 安装依赖
tar -zxvf nginx-1.0.11.tar.gz
cd nginx-1.0.11
./configure --prefix=/usr/local/nginx --with-http_ssl_module --with-http_flv_module --with-http_gzip_static_module
make &amp;&amp; make install
```
配置nginx支持cgi:
```bash 
wget http://www.cpan.org/modules/by-module/FCGI/FCGI-0.67.tar.gz
tar -zxvf FCGI-0.67.tar.gz
cd FCGI-0.67
perl Makefile.PL
make &amp;&amp; make install
cd ..
wget http://cpan.wenzk.com/authors/id/G/GB/GBJK/FCGI-ProcManager-0.19.tar.gz
tar -zxvf FCGI-ProcManager-0.19.tar.gz
cd FCGI-ProcManager-0.19
perl Makefile.PL
make &amp;&amp; make install
cd ..
wget http://search.cpan.org/CPAN/authors/id/I/IN/INGY/IO-All-0.39.tar.gz
tar zxvf IO-All-0.39.tar.gz
cd IO-All-0.39
perl Makefile.PL
make &amp;&amp; make install
```
创建cgiwarp-fcgi.pl(内容参照[这里](http://wiki.nginx.org/NginxSimpleCGI)),将cgiwarp-fcgi.pl放到/usr/local/bin下给执行权限,并执行:
```bash 
chmod +x /usr/local/bin/cgiwarp-fcgi.pl
mkdir -p /var/run/nginx/
perl /usr/local/bin/cgiwarp-fcgi.pl
chown nobody.nobody /var/run/nginx/cgiwrap-dispatch.sock  # 改变属主为nobody,是nginx正常使用
```
配置nginx支持身份验证
```bash 
yum -y install httpd
chkconfig --del httpd
mkdir /usr/local/nginx/conf/htpasswd
htpasswd -c /usr/local/nginx/conf/htpasswd/nagios nagiosadmin   # 添加nagiosadmin用户
```
还需要配置nginx支持php,这里使用php-fpm的方式,这里就不介绍.
## 配置 ##
首先配置nginx:
```bash 
    server {
        listen  192.168.3.3:80;
        server_name 192.168.3.3;
        auth_basic "Nagios";
        auth_basic_user_file /usr/local/nginx/conf/htpasswd/nagios;

        # 为了正常使用css和显示图片做一个url重写
        if ( $request_filename ~ \.(gif¦png¦jpg¦jpeg¦ico) ) {
            rewrite ^/nagios/(images/.*) /$1 break;
        }   

        if ( $request_filename ~ \.(css) ) {
            rewrite ^/nagios/(stylesheets/.*) /$1 break;
        }   

        # 支持php的配置
        location / {
            root /usr/local/nagios/share/;
            index index.php;
            fastcgi_pass unix:/var/run/php/php-fpm.sock;
            fastcgi_index index.php;
            fastcgi_param SCRIPT_FILENAME $document_root/$fastcgi_script_name;
            include fastcgi_params;
            include fastcgi.conf;
        }

        # 支持cgi的配置
        location ~ \.cgi$ {
            root /usr/local/nagios/sbin/;
            rewrite ^/nagios/cgi-bin/(.*)\.cgi(.*) /$1.cgi$2 break;
            fastcgi_pass unix:/var/run/nginx/cgiwrap-dispatch.sock;
            fastcgi_index   index.cgi;
            fastcgi_param SCRIPT_FILENAME  $document_root/$fastcgi_script_name;
            fastcgi_param QUERY_STRING     $query_string;
            fastcgi_param REQUEST_METHOD   $request_method;
            fastcgi_param CONTENT_TYPE     $content_type;
            fastcgi_param CONTENT_LENGTH   $content_length;
            fastcgi_param GATEWAY_INTERFACE  CGI/1.1;
            fastcgi_param SERVER_SOFTWARE    nginx;
            fastcgi_param SCRIPT_NAME        $fastcgi_script_name;
            fastcgi_param REQUEST_URI        $request_uri;
            fastcgi_param DOCUMENT_URI       $document_uri;
            fastcgi_param DOCUMENT_ROOT      $document_root;
            fastcgi_param SERVER_PROTOCOL    $server_protocol;
            fastcgi_param REMOTE_ADDR        $remote_addr;
            fastcgi_param REMOTE_PORT        $remote_port;
            fastcgi_param SERVER_ADDR        $server_addr;
            fastcgi_param SERVER_PORT        $server_port;
            fastcgi_param SERVER_NAME        $server_name;
        }

    }
```

修改nagios主配文件,修改nagios运行账户:
```bash 
vi /usr/local/nagios/etc/nagios.cfg

# 编辑下面选项:
nagios_user=nobody
```
修改cgi.cfg配置文件
修改默认认证cgi用户的为刚才用htpasswd创建的nagiosadmin,或用htpasswd新建一个用户用来认证cgi文件(不然nagios报错:`It appears as though you do not have permission to view information for any of the hosts you requested...`)

```bash 
vi /usr/local/nagios/etc/cgi.cfg
default_user_name=nagiosadmin
```
修改localhost.cfg添加一个新的监控主机:
```bash 
vi /usr/local/nagios/etc/objects/localhost.cfg

#  通过定义host添加一个主机
#  添加一个ip为192.168.3.102的监控主机
define host{
        use                     linux-server
        host_name               slave.org
        alias                   102
        address                 192.168.3.102
        }
....
# 通过定义service定义监控的服务:
# 通过ping来监控主机是否存活
define service{
        use                             local-service
        host_name                       slave.org
        service_description             PING
        check_command                   check_ping!100.0,20%!500.0,%60
        }
```
修改nagios安装目录权限:
```bash 
chown -R nobody.nagios /usr/local/nagios
chmod -R 775 /usr/local/nagios/
```
检查nagios配置文件:
```bash 
/usr/local/nagios/bin/nagios -v /usr/local/nagios/etc/nagios.cfg
```
在最后看到如下则表示成功:

Total Warnings: 0
Total Errors:   0
Things look okay - No serious problems were detected during the pre-flight check

启动nagios:
```bash 
service nagios start
Starting nagios: done.
```
这时候在浏览器里输入`http://ip/`查看状态,现在能看到所有监控主机的的状态了.

![www.linuxzen.com](/upload/blog001.jpg)
