Title: CentOS下搭建python web生产环境(nginx+web.py+uwsgi)
Tags: web.py,web,uwsgi,spawn-fcgi,python,nginx
Category: Linux
Date: 2012-04-19 17:59
前面都一篇文章介绍介绍了Ubuntu下web.py的开发环境搭建,这篇文章主要来介绍如何让web.py结合nginx来实现生产环境

首先使用环境介绍:
系统: CentOS 5.5 32位

Python版本:2.7.2

nginx:1.0.13

ip:192.168.3.3/24

由于CentOS默认自带都python(2.4.3)版本较低,所以我们采用手动编译安装python的方式来使用python 2.7.2

首先安装python 2.7.2
```
cd /usr/src/
mkdir python
cd python
#下载python2.7.2
wget http://www.python.org/ftp/python/2.7.2/Python-2.7.2.tar.bz2
tar -jvxf Python-2.7.2.tar.bz2
cd Python-2.7.2
./configure --prefix=/usr/local/python27 --enable-unicode=ucs4
```
先别急着安装,为什么后面的步骤能顺利进行我们需要我们的python支持zlib模块,
```bash
vi Modules/Setup
#在454行左右找到:#zlib zlibmodule.c -I$(prefix)/include -L$(exec_prefix)/lib -lz
# 去掉注释
zlib zlibmodule.c -I$(prefix)/include -L$(exec_prefix)/lib -lz
然后接着编译
make && make install
```
安装完成后我们想更方面的使用我们新安装的Python我们做如下更改.
```bash
mv /usr/bin/python /usr/bin/python24
ln -s /usr/local/python27/bin/python /usr/bin/python
ln -s /usr/local/python27/bin/python2.7 /usr/bin/python2.7
```
这样改完我们的yum就无法工作了,我们要修改yum来使yum工作:
```bash
vi /usr/bin/yum
```
将/#!/usr/bin/python改为#!/usr/bin/python2.4即可正常工作(版本可能不一样需查看自己系统自带的版本是什么)

现在我们执行python -V查看应该就是我们刚刚安装的2.7.2版本:
```bash
python -V
Python 2.7.2
```
我们安装了python下面我们就来武装我们的新python,

我们先为新的Python安装python的setuptools,配备easy_install.easy_install用于安装Python第三方扩展包而且只要一个命令即可完成:

下载:
```bash
wget http://pypi.python.org/packages/2.7/s/setuptools/setuptools-0.6c11-py2.7.egg#md5=fe1f997bc722265116870bc7919059ea
```
然后直接执行安装:
```
sh setuptools-0.6c11-py2.7.egg
```
安装好之后我们做一个软链接方便我们使用:
```
ln -s /usr/local/python27/bin/easy_install* /usr/bin/
```
然后我们来使用easy_install来安装Python第三方扩展

安装本文所需要的web.py
```
easy_install web.py
```
然后我们打开Python shell输入
```python
import web
```
如果没有报错则说明我们安装成功
安装flup:
```
easy_install flup
```
安装Spawn-fcgi :
```
wegt http://www.lighttpd.net/download/spawn-fcgi-1.6.3.tar.gz
tar -zxvf spawn-fcgi-1.6.3.tar.gz
cd spawn-fcgi-1.6.3
./configure --prefix=/usr/local/spawn-fcgi
make && make install
ln -s /usr/local/spawn-fcgi/bin/spawn-fcgi /usr/bin/
```
到这里我们就完成了Python的所有包安装,下面我们来部署nginx(本博有大量文章来介绍安装nginx,这里还是再来一遍吧..)
```bash
yum -y install zlib-devel pcre-devel openssl-devel  # 安装依赖
wget http://nginx.org/download/nginx-1.0.13.tar.gz # 下载
tar -zxvf nginx-1.0.13.tar.gz
cd nginx-1.0.13
 ./configure --prefix=/usr/local/nginx\   # 指定安装目录为/usr/local/nginx
--with-openssl=/usr/include/openssl\  # 启用ssl
--with-pcre\                          # 启用正规表达式
--with-http_stub_status_module        # 安装可以查看nginx状态的程序
make  && make install
```
我们修改nginx的配置文件:
```
        location / {
#           root   html;
#          index  index.html index.htm;
            include fastcgi_params;
            fastcgi_param SCRIPT_FILENAME $fastcgi_script_name;
            fastcgi_param PATH_INFO $fastcgi_script_name;
            fastcgi_pass unix:/tmp/pyweb.sock;
            fastcgi_param SERVER_ADDR $server_addr;
            fastcgi_param SERVER_PORT $server_port;
            fastcgi_param SERVER_NAME $server_name;
        }
```
然后创建一个web.py程序:
```python
#!/usr/bin/env python
#-*-coding:utf8-*-
import web
urls = ("/.*", "hello")
app = web.application(urls, globals())

class hello:
        def GET(self):
                return 'Hello, world!'

if __name__ == "__main__":
        web.wsgi.runwsgi = lambda func, addr = None: web.wsgi.runfcgi(func, addr)
        app.run()
```
将内容保存到/usr/local/nginx/html/下命名为index.py(或任意你喜欢的名字)
然后赋予其执行权限:
```
chmod +x /usr/local/nginx/html/index.py
```

通过命令创建spawn-fcgi进程:
```
spawn-fcgi -d /usr/local/nginx/html/ -f /usr/local/nginx/html/index.py  -s /tmp/pyweb.sock -u nobody -g nobody
```
我们使用unix socket,并用nginx的用户来创建.现在我们访问http://192.168.3.3/就可以看到:`Hello, world!`
