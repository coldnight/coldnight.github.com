Title: nginx+keepalived配置高可用HTTP群集
Tags: 高可用,负载均衡,群集,反向代理,双机,nginx,keepalived
Category: 负载均衡
Date: 2012-02-09 17:35
Nginx不仅是一款优秀的WEB服务器,同时可以根据nginx的反代理可以配置成强大的负载均衡器.这里就介绍如何把nginx配置成负载均衡器,并结合keepalived配置高可用的集群.
一般集群主要架构为:

前端为负载均衡器两个:主/备,两种工作方式,一种是备机待机状态,主机故障时备机接管主机工作实现故障庄毅,在主机故障恢复完成时备机继续仅需待机状态,第二种是主备同时工作,一台宕机另外一台自动接管另一台的工作实现故障转移.
第一种方式可以通过将域名解析到一个虚拟ip(vip)上,主负载均衡器绑定虚拟ip,当主负载均衡器出现故障时,通过keepalived自动将vip绑定到备用负载均衡器上同时arping网关刷新MAC地址.,避免单点故障.
第二种方式主备同时绑定一个vip,把域名通过DNS轮询的方式解析到这两个服务器上,主机出现故障,备机就将主机绑定vip绑定到备机上,同时arping网关刷新MAC地址.实现故障转移.

中间为WEB服务器作为real server,处理请求.
后端为数据库和分布式文件系统.数据库一般为主从两台.分布式文件系统有效解决WEB服务器之间的数据同步.有的还会将图片服务器单独分离出来放在后端.

本文使用环境:

* CentOS 5.5 32位
* nginx:nginx-1.0.11
* keepalived：keepalived-1.1.19.tar.gz
* 主调度器:192.168.3.1
* 备调度器:192.168.3.2
* real server:192.168.3.4/5/6

本文采用第一种方式来进行vip为:192.168.3.253
## 一、在主备服务器上部署nginx ##
### 1.下载 ###
```bash 
wget http://nginx.org/download/nginx-1.0.11.tar.gz
```
###  2.安装 ###
```bash 
 yum  -y install zlib-devel pcre-devel openssl-devel  # 安装依赖
tar -zxvf nginx-1.0.11.tar.gz
cd nginx-1.0.11
./configure --prefix=/usr/local/nginx --with-http_ssl_module --with-http_flv_module --with-http_gzip_static_module
make &amp;&amp; make install
```
3.配置

配置主调度器的nginx,编辑nginx.conf
```bash 
vi /usr/local/nginx/conf/nginx.conf

http {
    include       mime.types;
    default_type  application/octet-stream;

    #log_format  main  '$remote_addr - $remote_user [$time_local] "$request" '
    #                  '$status $body_bytes_sent "$http_referer" '
    #                  '"$http_user_agent" "$http_x_forwarded_for"';

    #access_log  logs/access.log  main;

    sendfile        on;
    #tcp_nopush     on;

    #keepalive_timeout  0;
    keepalive_timeout  65;

    #gzip  on;

    # 添加一组真实的服务器地址池
    # 供proxy_pass和fastcgi_pass指令中使用的代理服务器
    upstream real_server_pool {
      # 后台如果有动态应用的时候,ip_hash指令可以通过hash算法
      # 将客户端请求定位到同一台后端服务器上,解决session共享,
      # 但建议用动态应用做session共享
      # ip_hash;

      # server用于指定一个后端服务器的名称和参数
      # weight代表权,重默认为1,权重越高被分配的客户端越多
      # max_fails 指定时间内对后端请求失败的次数
      # fail_timeout 达到max_fails指定的失败次数后暂停的时间
      server  192.168.3.4:80 weight=1 max_fails=2 fail_timeout=30s;
      # down参数用来标记为离线,不参与负载均衡.在ip_hash下使用
      # 在此做演示,后面测试会去掉
      server  192.168.3.5:80 weight=1 max_fails=2 fail_timeout=30s down;
      # backup仅仅在非backup服务器宕机或繁忙的时候使用
      # (在此做演示,后面测试会去掉)
      server  192.168.3.6:80 weight=1 max_fails=2 fail_timeout=30s backup;
    }
    server {
        listen       192.168.3.1:80;
        server_name  localhost;

        #charset koi8-r;

        #access_log  logs/host.access.log  main;

        location / {
            #root   html;
            #index  index.html index.htm;
            # 使用upstream设置的一组代理服务器
            # 如果后端服务器出现502或504等执行错误时,
            # 将自动将请求转发给负载均衡池中的另一台服务器.
            proxy_next_upstream http_502 http_504 error timeout invalid_header;
            proxy_pass http://real_server_pool;
            proxy_set_header Host $host;
            proxy_set_header X-Forwarded-For $remote_addr;
        }
    }
}
```
(`注意:`配置文件中注释ip_hash,以为ip_hash这个功能将保证这个客户端请求总是被转发到一台服务器上,所以如果启用了ip_hash指令,将不能再使用weight(权重参数),配置文件中加入为解释ip_hash指令)
配置备用nginx,将监听ip改为备用调度器的ip
```bash 
http {
    include       mime.types;
    default_type  application/octet-stream;

    #log_format  main  '$remote_addr - $remote_user [$time_local] "$request" '
    #                  '$status $body_bytes_sent "$http_referer" '
    #                  '"$http_user_agent" "$http_x_forwarded_for"';

    #access_log  logs/access.log  main;

    sendfile        on;
    #tcp_nopush     on;

    #keepalive_timeout  0;
    keepalive_timeout  65;

    #gzip  on;

    upstream real_server_pool {
      #ip_hash;
      server  192.168.3.4:80 weight=1 max_fails=2 fail_timeout=30s;
      server  192.168.3.5:80 weight=1 max_fails=2 fail_timeout=30s;
      server  192.168.3.6:80 weight=1 max_fails=2 fail_timeout=30s;
    }
    server {
        listen       192.168.3.2:80;             # 监听ip改为本地ip
        server_name  localhost;

        #charset koi8-r;

        #access_log  logs/host.access.log  main;

        location / {
            #root   html;
            #index  index.html index.htm;
            proxy_next_upstream http_502 http_504 error timeout invalid_header;
            proxy_pass http://real_server_pool;
            proxy_set_header Host $host;
            proxy_set_header X-Forwarded-For $remote_addr;
        }
```
然后启动主备nginx:
```bash 
/usr/local/nginx/sbin/nginx
```
## 二、在主备服务器上部署keepalived ##
安装
安装依赖:
```bash 
yum -y install kernel-devel              # 安装依赖
```
开启路由转发:
```bash 
vi /etc/sysctl.conf
net.ipv4.ip_forward = 1 # 此参数改为1
sysctl -p # 使修改生效
```

首先安装ipvs:
```bash 
ln -s /usr/src/kernels/2.6.18-194.el5-i686/ /usr/src/linux  # ipvs需要内核文件,做一个软连接
# 下载
wget http://www.linuxvirtualserver.org/software/kernel-2.6/ipvsadm-1.24.tar.gz
tar -zxvf ipvsadm-1.24.tar.gz
cd ipvsadm-1.24
make
make install
```
然后安装keepalived
```bash 
# 下载
wget http://www.keepalived.org/software/keepalived-1.1.19.tar.gz
tar -zxvf keepalived-1.1.19.tar.gz
cd keepalived-1.1.19
./configure --prefix=/ \            # 安装在默认位置(配置文件,二进制文件,启动脚本放到默认位置)
--mandir=/usr/local/share/man/ \
--with-kernel-dir=/usr/src/kernels/2.6.18-194.el5-i686/    # 需要内核的头文件
make &amp;&amp; make install
```
### 配置keepalived
编辑主调度器配置文件/etc/keepalived/keepalived.conf ###
```bash 
global_defs {
   notification_email {
        cold_night@linuxzen.com             # 定义通知邮箱,有多个可以换行添加
}
   notification_email_from root@linuxzen.com# 定义发送邮件的邮箱
   smtp_server www.linuxzen.com             # 定义发件服务器
   smtp_connect_timeout 30                  # 定义连接smtp服务器超时时间
   router_id LVS_DEVEL
}

vrrp_instance VI_1 {
    state MASTER                   # 标示主备,备机上改为BACKUP
    interface eth0                 # HA监测的端口
    virtual_router_id 51           # 主备的virtual_router_id的值必须相同
    priority 100                   # 优先级,通常主要比备稍大
    advert_int 1                   # VRRP Multicast 广播周期秒数
    authentication {               # 定义认证
        auth_type PASS             # 认证方式
        auth_pass 1111             # 认证口令字
    }
    virtual_ipaddress {            # 定义vip
        192.168.3.253              # 多个可换行添加,一行一个
    }
}

virtual_server 192.168.3.253 80 {
    delay_loop 6             # 每隔 6 秒查询 realserver 状态
    lb_algo rr
    lb_kind NAT
    nat_mask 255.255.255.0
    persistence_timeout 50   # 同一IP 的连接50秒内被分配到同一台realserver
    protocol TCP             # 用TCP监测realserver的状态

    real_server 192.168.3.1 80 {
        weight 3                # 权重
        TCP_CHECK {
            connect_timeout 10  # 10秒无响应超时
            nb_get_retry 3
            delay_before_retry 3
            connect_port 80
        }
    }

    real_server 192.168.3.2 80 {
        weight 3
        TCP_CHECK {
            connect_timeout 3
            delay_before_retry 3
            connect_port 80
        }
    }
}
```
配置备用调度器的keepalived,只需要将state MASTER 改为state BACKUP,降低priority 100 的值:

```bash 
global_defs {
   notification_email {
        cold_night@linuxzen.com
}
   notification_email_from root@linuxzen.com
   smtp_server www.linuxzen.com
   smtp_connect_timeout 30
   router_id LVS_DEVEL
}

vrrp_instance VI_1 {
    state BACKUP                   # 备机上改为BACKUP
    interface eth0
    virtual_router_id 51           # 主备的virtual_router_id的值必须相同
    priority 99                    # 备用优先级小于主调度器
    advert_int 1
    authentication {
        auth_type PASS
        auth_pass 1111
    }
    virtual_ipaddress {
        192.168.3.253
    }
}

virtual_server 192.168.3.253 80 {
    delay_loop 6
   lb_algo rr
    lb_kind NAT
    nat_mask 255.255.255.0
    persistence_timeout 50
    protocol TCP        

    real_server 192.168.3.1 80 {
        weight 3
        TCP_CHECK {
            connect_timeout 10
            nb_get_retry 3
            delay_before_retry 3
            connect_port 80
        }
    }

    real_server 192.168.3.2 80 {
        weight 3
        TCP_CHECK {
            connect_timeout 3
            delay_before_retry 3
            connect_port 80
        }
    }
}
```
主备上启动keepalived:
```bash 
service keepalived start
```
## 三、测试-----部署后端服务器

在后端服务器安装nginx,这里仅部署一台然后创建3个基于ip的虚拟主机供测试:
绑定ip:
```bash 
ifconfig eth0:1 192.168.3.4/24
ifconfig eth0:2 192.168.3.5/24
ifconfig eth0:3 192.168.3.6/24
```
安装nginx后编辑配置文件,在http块里添加:
```bash 
http {
    server {
        listen  192.168.3.4:80;
        server_name     192.168.3.4;

        location / {
             root html/s1;
             index index.html index.htm;
        }
    }

    server {
        listen  192.168.3.5:80;
        server_name     192.168.3.5;

        location / {
            root html/s2;
            index index.html index.htm;
        }
    }

    server {
        listen 192.168.3.6:80;
        server_name     192.168.3.5;

        location / {
            root html/s3;
            index index.html index.htm;
        }
    }
}
```
创建虚拟主机根目录,并创建不通的首页文档:
```bash 
cd /usr/local/nginx/html/
mkdir s1 s2 s3
echo server1 &gt; s1/index.html
echo server2 &gt; s2/index.html
echo server3 &gt; s3/index.html
```
启动nginx:
```bash 
/usr/local/nginx/sbin/nginx
```
打开浏览器访问`http://192.168.3.253`

刷新会看到显示不同的内容:server1,server2,server3(生产中的服务器应该是一样的)
[gallery link="file" order="DESC"]
现在停掉主调度器的keepalived
```bash 
pkill keepalived
```
查看备调度器的日志:
```bash 
cat /var/log/messages
Feb 10 16:36:27 cfhost Keepalived_vrrp: VRRP_Instance(VI_1) Transition to MASTER STATE
Feb 10 16:36:28 cfhost Keepalived_vrrp: VRRP_Instance(VI_1) Entering MASTER STATE
Feb 10 16:36:28 cfhost Keepalived_vrrp: VRRP_Instance(VI_1) setting protocol VIPs.
Feb 10 16:36:28 cfhost Keepalived_vrrp: VRRP_Instance(VI_1) Sending gratuitous ARPs on eth0 for 192.168.3.253
Feb 10 16:36:28 cfhost Keepalived_vrrp: Netlink reflector reports IP 192.168.3.253 added
Feb 10 16:36:28 cfhost Keepalived_healthcheckers: Netlink reflector reports IP 192.168.3.253 added
Feb 10 16:36:33 cfhost Keepalived_vrrp: VRRP_Instance(VI_1) Sending gratuitous ARPs on eth0 for 192.168.3.253
```
&nbsp;

现在访问`http://192.168.3.253`依然可以访问.
大家也看到了备机keepalived只有检测主机的keepalived停止的时候才会切换vip,而不是检测一台real server的某一服务(比如检测80端口的HTTP)切换vip,所以在nginx进程停止的时候,如果服务器没有宕机这时候就无法实现故障转移,所以我们编写一个检测nginx状态的脚本结合keepalived实现故障转移:
```bash 
#!/bin/bash
#filename:nsc.sh
ps aux ¦ grep nginx ¦ grep -v grep 2&gt; /dev/null 1&gt;&amp;2   # 过滤nginx进程
if [[ $? -eq 0 ]]               # 如果过滤有nginx进程会返回0则认为nginx存活
then
    sleep 5                     # 使脚本进入休眠
else
# 如果nginx没有存活尝试启动nginx,如果失败则杀死keepalived的进程
    /usr/local/nginx/sbin/nginx
    ps aux ¦ grep nginx ¦ grep -v grep 2&gt; /dev/null 1&gt;&amp;2
    if [[ $? -eq 0 ]]
    then
        pkill keepalived
    fi
fi
```
然后后台运行此脚本:
```bash 
nohup sh nsc.sh &
```
这样就实现了群集的高可靠和高可用.
