Title: lvs+keepalived实现高可用群集配置详解
Tags: 高可用,详解,群集,lvs,Linux,keepalived
Category: 负载均衡
Date: 2012-04-16 14:25
lvs是一个开源的软件，由毕业于国防科技大学的章文嵩博士于1998年5月创立(中国人的项目)，可以实现LINUX平台下的简单负载均衡。LVS是Linux Virtual Server的缩写，意思是Linux虚拟服务器。本文将介绍lvs结合keepalived实现一个高科用的Linux群集系统.

lvs有三种工作模式NAT(地址转换),IP Tunneling(IP隧道)、Direct Routing(直接路由)。
工作效率最低的是NAT模式,但NAT模式可以用于各种系统,各种环境的负载均衡,只需要一个公网ip即可实现
IP Tunneling模式调度器将连接分发到不同的后端real server,然后由real server处理请求直接相应给用户,大大提高了调度器的调度效率,后端real server没有物理位置和逻辑关系的限制,后端real server可以在Lan/Wlan,但是后端real server必须支持IP隧道协议.
DR(Direct Routing)是效率最高的,与IP Tunneling类似,都是处理一般连接,将请求给后端real server,然后由real server处理请求直接相应给用户,Direct Routing与IP Tunneling相比，没有IP封装的开销，但由于采用物理层,所以DR模式的调度器和后端real server必须在一个物理网段里,中间不能过路由器(也就是一个交换机相连).

lvs支持8种不同的调度算法轮叫(rr)、加权轮叫(wrr)、最小连接(lc)、加权最小连接(wlc)、基于局部性最小连接(lblc)、带复制的基于局部性最少链接(lblcr)、目标地址散列(dh)和源地址散列(sh).

下面就介绍如何来安装和配置lvs+keepalived

本文使用环境:
操作系统:CentOS 5.5  32bit

主调度器:192.168.3.101/24

备调度器:192.168.3.102/24

后端real server: 192.168.3.3/24 |192.168.3.102/24(我们这里使用备用lvs作为一个测试

vip(virtual ip):192.168.3.100/24

lvs在2.6的内核中是默认支持的,所以我们就不需要在来安装,但是我们需要安装用户配置工具ipvsadm
```
yum -y install ipvsadm           # 分别在主从lvs上执行安装ipvsadm
```
我们查看lvs是否支持:
```
lsmod ¦ grep ip_vs          #
ip_vs                  78081  1
modprobe -l¦ grep ip_vs
/lib/modules/2.6.18-194.el5/kernel/net/ipv4/ipvs/ip_vs.ko
/lib/modules/2.6.18-194.el5/kernel/net/ipv4/ipvs/ip_vs_dh.ko
/lib/modules/2.6.18-194.el5/kernel/net/ipv4/ipvs/ip_vs_ftp.ko
/lib/modules/2.6.18-194.el5/kernel/net/ipv4/ipvs/ip_vs_lblc.ko
/lib/modules/2.6.18-194.el5/kernel/net/ipv4/ipvs/ip_vs_lblcr.ko
/lib/modules/2.6.18-194.el5/kernel/net/ipv4/ipvs/ip_vs_lc.ko
/lib/modules/2.6.18-194.el5/kernel/net/ipv4/ipvs/ip_vs_nq.ko
/lib/modules/2.6.18-194.el5/kernel/net/ipv4/ipvs/ip_vs_rr.ko
/lib/modules/2.6.18-194.el5/kernel/net/ipv4/ipvs/ip_vs_sed.ko
/lib/modules/2.6.18-194.el5/kernel/net/ipv4/ipvs/ip_vs_sh.ko
/lib/modules/2.6.18-194.el5/kernel/net/ipv4/ipvs/ip_vs_wlc.ko
/lib/modules/2.6.18-194.el5/kernel/net/ipv4/ipvs/ip_vs_wrr.ko
```
本文介绍lvs的`DR`模式,首先部署keepalived.本博前面已经介绍如何来安装keepalived.这里就不在只简单的贴一下步骤:

在主备服务器上部署keepalived(因为前面已经rpm包安装了ipvsadm,所以就不需要重复安装):
```
vi /etc/sysctl.conf
net.ipv4.ip_forward = 1 # 此参数改为1
sysctl -p # 使修改生效
```
安装依赖:
```bash
yum  -y install openssl-devel
# 下载并安装keepalived
wget http://www.keepalived.org/software/keepalived-1.1.19.tar.gz
tar -zxvf keepalived-1.1.19.tar.gz
cd keepalived-1.1.19
./configure --prefix=/ \            # 安装在默认位置(配置文件,二进制文件,启动脚本放到默认位置)
--mandir=/usr/local/share/man/ \
--with-kernel-dir=/usr/src/kernels/2.6.18-194.el5-i686/    # 需要内核的头文件
make && make install
```
在主备lvs上安装keepalived完毕后我们先来配置主lvs上的keepalived:
编辑配置文件`/etc/keepalived/keepalived.conf`:
```
! Configuration File for keepalived

global_defs {
   notification_email {
    coldnight@linuxzen.com                                   # 发生故障时发送的邮箱
   }
   notification_email_from linuxzen@linuxzen.com             # 使用哪个邮箱发送
   smtp_server linuxzen.com                                  # 发件服务器
   smtp_connect_timeout 30
   router_id LVS_DEVEL
}

vrrp_instance VI_1 {
    state MASTER             # 标示为主lvs
    interface eth0           # HA检测端口
    virtual_router_id 51     # 主备的virtual_router_id 必须相同
    priority 100             # 优先级,备lvs要比主lvs稍小
    advert_int 1             # VRRP Multicast 广播周期秒数
    authentication {         # 定义认证
        auth_type PASS       # 认证方式为口令认证
        auth_pass 1111       # 定义口令
    }
    virtual_ipaddress {      # 定义vip
        192.168.3.100        # 多个vip可换行添加
    }
}

virtual_server 192.168.3.100 80 {
    delay_loop 6       # 每隔6秒查看realserver状态
    lb_algo wlc        # 调度算法为加权最小连接数
    lb_kind DR         # lvs工作模式为DR(直接路由)模式
    nat_mask 255.255.255.0
    persistence_timeout 50  # 同一IP 的连接50秒内被分配到同一台realserver(测试时建议改为0)
    protocol TCP            # 用TCP监测realserver的状态

    real_server 192.168.3.3 80 {       # 定义realserver
        weight 3                       # 定义权重
        TCP_CHECK {  # 注意TCP_CHECK和{之间的空格,如果没有的话只会添加第一个realserver
            connect_timeout 3          # 三秒无响应超时
            nb_get_retry 3
            delay_before_retry 3
            connect_port 80
        }
    }
    real_server 192.168.3.102 80 {
        weight 3
        TCP_CHECK {
            connect_timeout 3
            nb_get_retry 3
            delay_before_retry 3
            connect_port 80
        }
    }
}
```
配置备用lvs的keepalived,只需要将state MASTER 改为state BACKUP,降低priority 100 的值:
```
! Configuration File for keepalived

global_defs {
   notification_email {
    coldnight@linuxzen.com                               # 发生故障时发送的邮箱
   }
   notification_email_from linuxzen@linuxzen.com         # 使用哪个邮箱发送
   smtp_server linuxzen.com                              # 发件服务器
   smtp_connect_timeout 30
   router_id LVS_DEVEL
}

vrrp_instance VI_1 {
    state BACKUP              # 标示为备lvs
    interface eth0            # HA检测端口
    virtual_router_id 51      # 主备的virtual_router_id 必须相同
    priority 99               # 优先级,备lvs要比主lvs稍小
    advert_int 1              # VRRP Multicast 广播周期秒数
    authentication {          # 定义认证
        auth_type PASS        # 认证方式为口令认证
        auth_pass 1111        # 定义口令
    }
    virtual_ipaddress {       # 定义vip
        192.168.3.100         # 多个vip可换行添加
    }
}

virtual_server 192.168.3.100 80 {
    delay_loop 6      # 每隔6秒查看realserver状态
    lb_algo wlc       # 调度算法为加权最小连接数
    lb_kind DR        # lvs工作模式为DR(直接路由)模式
    nat_mask 255.255.255.0
    persistence_timeout 50  # 同一IP 的连接50秒内被分配到同一台realserver
    protocol TCP            # 用TCP监测realserver的状态

    real_server 192.168.3.3 80 {       # 定义realserver
        weight 3                       # 定义权重
        TCP_CHECK {    # 注意TCP_CHECK和{之间的空格,如果没有的话只会添加第一个realserver
            connect_timeout 3          # 三秒无响应超时
            nb_get_retry 3
            delay_before_retry 3
            connect_port 80
        }
    }
    real_server 192.168.3.102 80 {
        weight 3
        TCP_CHECK {
            connect_timeout 3
            nb_get_retry 3
            delay_before_retry 3
            connect_port 80
        }
    }
}
```
由于使用keepalived就不需要使用脚本来配置lvs调度器,但是这里我们还是会给出一个脚本内容,但我们不会用到这个脚本:lvs已经内置于内核,配置命令是ipvsadm,所以lvs的一些操作是通过ipvsadm来控制.下面我们就编写脚本来实现lvs的DR模式:

编写脚本lvsdr:

我们把lvs`vi /etc/init.d/lvsdr`添加如下内容
```bash
#!/bin/sh
# 定义虚拟ip
VIP=192.168.3.100
# 定义realserver,并已逗号分开
RIPS=192.168.3.3,192.168.3.102 #,192.168.3.5,192.168.3.6

# 定义提供服务的端口
SERVICE=80

# 调用init.d脚本的标准库
. /etc/rc.d/init.d/functions
case $1 in
        start)
        echo "Start LVS of DR Mode"
        # lvs dr模式不需要路由转发,但是keepalived需要
        #echo "0" > /proc/sys/net/ipv4/ip_forward
        # 开启icmp包重定向
        echo "1" > /proc/sys/net/ipv4/conf/all/send_redirects
        echo "1" > /proc/sys/net/ipv4/conf/default/send_redirects
        echo "1" > /proc/sys/net/ipv4/conf/eth0/send_redirects
        # 绑定虚拟ip
        ifconfig eth0:0 $VIP broadcast $VIP netmask 255.255.255.255 up
        route add -host $VIP dev eth0:0
        # 清除lvs规则
        ipvsadm -C
        # 添加一条虚拟服务器记录
    # -p指定一定的时间内将相同的客户端分配到同一台后端服务器
    # 用于解决session的问题,测试时或有别的解决方案时建议去掉
        ipvsadm -A -t $VIP:$SERVICE -s wlc -p

        # 添加真实服务器记录
        for RIP in `echo $RIPS ¦sed  -e 's/,/\n/g'`
        do
                ipvsadm -a -t $VIP:$SERVICE -r $RIP:$SERVICE -g -w 1
        done
        # 设置tcp tcpfin  udp的超时连接值
        ipvsadm --set 30 120 300
        ipvsadm
        ;;

        stop)
        echo "Stop LVS DR"
        ifconfig eth0:0 down
        ipvsadm -C
        ;;
        *)
        echo "Usage:$0 {start ¦ stop}"
        exit 1
esac
```
编辑完毕保存退出,然后给这个脚本执行权限:
```
chmod +x /etc/init.d/lvsdr
```
然后就可以通过service命令来启动lvs dr模式
```
service lvsdr start
```
将这个脚本分别放到主备lvs的/etc/init.d/下,赋予执行权限.
我们真正需要的是realserver的脚本,下面我们来编写realserver脚本,同样放在/etc/init.d/下,编辑rs脚本:
```bash
vi /etc/init.d/lvsrs
#!/bin/sh
VIP=192.168.3.100
. /etc/rc.d/init.d/functions
case $1 in
        start)
        echo "lo:0 port starting"
        # 为了相应lvs调度器转发过来的包,需在本地lo接口上绑定vip
        ifconfig lo:0 $VIP broadcast $VIP netmask 255.255.255.255 up
        # 限制arp请求
        echo "1" > /proc/sys/net/ipv4/conf/lo/arp_ignore
        echo "2" > /proc/sys/net/ipv4/conf/lo/arp_announce
        echo "1" > /proc/sys/net/ipv4/conf/all/arp_ignore
        echo "2" > /proc/sys/net/ipv4/conf/all/arp_announce
        ;;
        stop)
        echo "lo:0 port closing"
        ifconfig lo:0 down
        echo "0" > /proc/sys/net/ipv4/conf/lo/arp_ignore
    echo "0" > /proc/sys/net/ipv4/conf/lo/arp_announce
    echo "0" > /proc/sys/net/ipv4/conf/all/arp_ignore
    echo "0" > /proc/sys/net/ipv4/conf/all/arp_announce
        ;;
        *)
        echo "Usage: $0 {start ¦ stop}"
        exit 1
esac
```
给脚本赋予执行权限
```
chmod +x /etc/init.d/lvsrs
```
并将这个脚本放到所有的realserver的/etc/init.d/下.下面开始测试:

先来确认下我们做的变动:主从lvs分别安装keepalived,并且在/etc/init.d/下添加了lvsdr脚本(不使用).

后端realserver分别在/etc/init.d/下添加了lvsrs脚本.我们先测试keepalived:

首先在主调度器上启动keepalived:
```
service keepalived start
```
查看日志文件:
```
tail -50 /var/log/message
Mar 21 22:29:10 master kernel: device eth0 left promiscuous mode
Mar 21 22:29:10 master kernel: type=1700 audit(1332340150.598:12): dev=eth0 prom=0 old_prom=256 auid=4294967295 ses=4294967295
Apr 16 13:31:32 master Keepalived: Starting Keepalived v1.1.19 (04/16,2012)
Apr 16 13:31:32 master Keepalived_healthcheckers: Netlink reflector reports IP 192.168.3.101 added
Apr 16 13:31:32 master Keepalived_healthcheckers: Registering Kernel netlink reflector
Apr 16 13:31:32 master Keepalived_healthcheckers: Registering Kernel netlink command channel
Apr 16 13:31:32 master Keepalived_healthcheckers: Opening file '/etc/keepalived/keepalived.conf'.
Apr 16 13:31:32 master Keepalived_healthcheckers: Configuration is using : 8897 Bytes
Apr 16 13:31:32 master Keepalived_healthcheckers: Using LinkWatch kernel netlink reflector...
Apr 16 13:31:32 master Keepalived: Starting Healthcheck child process, pid=5369
Apr 16 13:31:32 master Keepalived: Starting VRRP child process, pid=5370
Apr 16 13:31:32 master Keepalived_vrrp: Netlink reflector reports IP 192.168.3.101 added
Apr 16 13:31:32 master Keepalived_vrrp: Registering Kernel netlink reflector
Apr 16 13:31:32 master Keepalived_vrrp: Registering Kernel netlink command channel
Apr 16 13:31:32 master Keepalived_vrrp: Registering gratutious ARP shared channel
Apr 16 13:31:32 master Keepalived_vrrp: Opening file '/etc/keepalived/keepalived.conf'.
Apr 16 13:31:32 master Keepalived_vrrp: Configuration is using : 36512 Bytes
Apr 16 13:31:32 master Keepalived_vrrp: Using LinkWatch kernel netlink reflector...
Apr 16 13:31:32 master Keepalived_vrrp: VRRP sockpool: [ifindex(2), proto(112), fd(10,11)]
Apr 16 13:31:33 master Keepalived_vrrp: VRRP_Instance(VI_1) Transition to MASTER STATE
Apr 16 13:31:34 master Keepalived_vrrp: VRRP_Instance(VI_1) Entering MASTER STATE
Apr 16 13:31:34 master Keepalived_vrrp: VRRP_Instance(VI_1) setting protocol VIPs.
Apr 16 13:31:34 master Keepalived_healthcheckers: Netlink reflector reports IP 192.168.3.100 added
Apr 16 13:31:34 master Keepalived_vrrp: VRRP_Instance(VI_1) Sending gratuitous ARPs on eth0 for 192.168.3.100
Apr 16 13:31:34 master Keepalived_vrrp: Netlink reflector reports IP 192.168.3.100 added
Apr 16 13:31:39 master Keepalived_vrrp: VRRP_Instance(VI_1) Sending gratuitous ARPs on eth0 for 192.168.3.100
```
然后在备用调度器上启动keepalived然后查看日志:
```
Apr 16 13:33:35 slave Keepalived_vrrp: VRRP_Instance(VI_1) Entering BACKUP STATE
Apr 16 13:33:35 slave Keepalived_vrrp: VRRP sockpool: [ifindex(2), proto(112), fd(11,12)]
Apr 16 13:33:35 slave Keepalived_healthcheckers: Netlink reflector reports IP 192.168.3.102 added
Apr 16 13:33:35 slave Keepalived_healthcheckers: Registering Kernel netlink reflector
Apr 16 13:33:35 slave Keepalived_healthcheckers: Registering Kernel netlink command channel
Apr 16 13:33:35 slave Keepalived_healthcheckers: Opening file '/etc/keepalived/keepalived.conf'.
Apr 16 13:33:35 slave Keepalived_healthcheckers: Configuration is using : 8895 Bytes
Apr 16 13:33:35 slave kernel: IPVS: [wlc] scheduler registered.
Apr 16 13:33:35 slave Keepalived_healthcheckers: Using LinkWatch kernel netlink reflector...
```
在主调度器上执行
```
service keepalived stop
```
查看备用调度器日志:
```
tail -20 /var/log/message
Apr 16 13:39:44 slave Keepalived_vrrp: VRRP_Instance(VI_1) Transition to MASTER STATE
Apr 16 13:39:45 slave Keepalived_vrrp: VRRP_Instance(VI_1) Entering MASTER STATE
Apr 16 13:39:45 slave Keepalived_vrrp: VRRP_Instance(VI_1) setting protocol VIPs.
Apr 16 13:39:45 slave Keepalived_vrrp: VRRP_Instance(VI_1) Sending gratuitous ARPs on eth0 for 192.168.3.100
Apr 16 13:39:45 slave Keepalived_vrrp: Netlink reflector reports IP 192.168.3.100 added
Apr 16 13:39:45 slave Keepalived_healthcheckers: Netlink reflector reports IP 192.168.3.100 added
```
我们看到keepalived已经成功切换.

然后我们使用ipvsadm命令查看(在此之前要确认后端realserver已经启动了web服务):
```
ipvsadm
IP Virtual Server version 1.2.1 (size=4096)
Prot LocalAddress:Port Scheduler Flags
  -> RemoteAddress:Port           Forward Weight ActiveConn InActConn
TCP  192.168.3.100:http wlc
  -> 192.168.3.3:http             Route   3      0          0
  -> 192.168.3.102:http           Route   3      0          0
```
然后分别启动后端realserver的lvsrs服务:
```
servie lvsrs start
```
然后浏览器访问192.168.3.100,如果keepalived的persistence_timeout参数值为0,而且两个后端realserver是不同的内容,刷新就可以看到两个不同的页面交替.
