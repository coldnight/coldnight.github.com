Title: Gentoo下搭建python web环境(nginx+bottle+virtualenv+uwsgi)
Tags: web,virtualenv,uwsgi,python,Gentoo,bottle, nginx
Category: Linux
Date: 2012-05-16 11:05
最近根据Gentoo官方文档整了一台Gentoo的虚拟机,感觉还是不错的,决定放弃CentOS投奔Gentoo,这几天研究NoSQL mongodb和python的bottle框架,web.py效率不是很好,而且是类级,bottle使用装饰器(虽然对她还是懵懂阶段,但是貌似很强大).感觉bottle更加强大和接近python,没有封装太多东西,django只能依照她的思想来做自己的事,最后还是选择了bottle来进入python的web世界,web服务器同样选择nginx.virtualenv可以让一个应用有一个相对独立的环境,特别用于多解释器环境或者经常变更的环境.uwsgi是web和python的中间件(可以这么解释吧).

## 环境:
系统:Gentoo
ip:192.168.3.1

好吧废话不多说,Gentoo安装过程这里不再详述,官方文档很详细,下面记录安装配置过程.
首先Gentoo没有默认安装vim,先安装vim:
```python
emerge vim
```
### 安装配置Python
最新版的Gentoo安装完毕后默认使用python3.2,而我惯用Python2.7.先首先安装python2.7.
Gentoo使用emerge包管理,安装Python2.7:
```bash
cd /usr/portage/dev-lang/python/
emerge python-2.7.2-r3.ebuild
```
等待安装完成后使系统默认使用python2.7,先查看默认项
```
eselect python list
Available Python interpreters:
  [1]   python2.7 
  [2]   python3.2 *
```
默认使用3.2,选择python2.7
```bash
eselect python set 1
```
选择过后别忘记执行:
```bash
python-updater
```
好了我已经将python换成了常用的python2.7,下面来安装easy_install:
```bash
emerge setuptools
```
安装完后使用easy_install安装bottle框架和mongodb驱动:
```bash
easy_install bottle
easy_install pymogon
```
安装uwsgi/nginx/virtualenv:
```bash
emerge nginx
emerge uwsgi
emerge virtualenv
```
### 使用virtualenv创建应用
我们使用virtualenv创建一个应用:
```bash
mkdir /code/python
virtualenv /code/python
source bin/activate
```
然后查看应用目录下会多出几个目录.
### 配置uWSGI
首先创建一个uwsgi配置文件,并编辑它:
```bash
cp /etc/conf.d/uwsgi /etc/conf.d/uwsgi.py
vi /etc/conf.d/uwsgi.py
```
改成内如如下:
```conf
# Distributed under the terms of the GNU General Public License v2
# $Header: /var/cvsroot/gentoo-x86/www-servers/uwsgi/files/uwsgi.confd,v 1.1 2011/05/31 19:49:07 maksbotan Exp $

# DO NOT MODIFY THIS FILE DIRECTLY! CREATE A COPY AND MODIFY THAT INSTEAD!

# Path (or name) of UNIX/TCP socket to bind to
#
UWSGI_SOCKET=/var/tmp/bottle.sock            # 使用unix socket

# Enable threads?
#
UWSGI_THREADS=1

# The path to your uWSGI application.
#
#UWSGI_PROGRAM=

# The path to your uWSGI xml config file.
#
UWSGI_XML_CONFIG=/code/bottle.xml               # 使用xml配置文件

# The number of child processes to spawn. The default is 1.
#
UWSGI_CHILDREN=1

# The log file path. If empty logging is disabled
#
UWSGI_LOG_FILE=/var/log/uwsgi.log                    # 定义日志

# If you want to run your application inside a chroot then specify the
# directory here. Leave this blank otherwise.
#
#UWSGI_CHROOT=/code/python

# If you want to run your application from a specific directiory specify
# it here. Leave this blank otherwise.
#
# UWSGI_DIR=

# The user and group to run your application as. If you do not specify these,
# the application will be run as root:root.
#
UWSGI_USER=

# Additional options you might want to pass to uWSGI
#
#UWSGI_EXTRA_OPTIONS=
```
然后创建bottle.xml配置文件:
```bash
vi /code/bottle.xml
```
内容如下:
```xml
<uwsgi>
        <socket>/var/tmp/bottle.sock</socket>
        <home>/code/python</home>
        <chdir>/code/python</chdir>
        <python-path>/code/python</python-path>
        <module>index</module>
        <master/>
        <memory/>
        <logto>/var/log/bottle.log</logto>
        <daemonize>/var/log/uwsgi.log</daemonize>
</uwsgi>
```
其中<module></module>定义的就是你的web程序,也就是/code/python下要有一个index.py.

配置完毕后创建启动脚本:
```
ln -sf /etc/init.d/uwsgi /etc/init.d/uwsgi.py
```
并加入开机启动:
```
rc-update add uwsgi.py default
```
### 配置nginx
编辑配置文件:
```
vi /etc/nginx/nginx.conf
```
修改内容如下:
```
    server {
        listen 192.168.3.1:80;
        server_name linuxzen.com;

        access_log /var/log/nginx/localhost.access_log main;
        error_log /var/log/nginx/localhost.error_log info;

        root /code/python;

        location / { 
            include uwsgi_params;
            uwsgi_param UWSGI_PYTHOME /code/python;
            uwsgi_param UWSGI_CHDIR /code/python;                               
            uwsgi_param UWSGI_SCRIPT    index;
            uwsgi_pass  unix:/var/tmp/bottle.sock;
        }   
    }
```
同样将nginx作为开机启动:
```
rc-update add nginx default
```
## 测试
首先创建一个bottle程序供测试:
```
vi /code/python/index.py
参考内容如下:
#!/usr/bin/env python
#-*- coding: utf8 -*-

'''
# Author    : cold night
# Filename  : index.py                                                          
'''
from bottle import run, route, default_app

@route('/')
def index():
        return "Hello world!"

if __name__ == "__main__":
    run(host="localhost", port=8888)
else:
    application = default_app()
```
然后启动nginx和uwsgi:
```
/etc/init.d/nginx start
/etc/init.d/uwsgi.py start
```
然后通过浏览器访问即可看到`Hello world!`
