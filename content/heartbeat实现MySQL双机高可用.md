Title: heartbeat实现MySQL双机高可用
Tags: 高可用,心跳,双机,vip,MySQL,heartbeat
Category: MySQL
Date: 2012-02-12 17:01
对于一个网站或一个企业最重要的无疑就是数据,那么数据库的数据安全无疑就更加重要,所以我们必须保证数据库的数据完整,这里就介绍使用heartbeat来实现MySQL双机高可用.

当我们的MySQL数据库故障或MySQL数据库服务器出现故障的时候我们希望有一个备用能自动代替主MySQL数据来完成当前的任务,当主MySQL服务器恢复故障的时候备用的能切换到备用等待下一次故障出现.这里我们就结合故障检测HA来实现.

HA会定时发送心跳包检测主备服务器的健康状态,当主服务器出现故障时会自动将vip切换到备用服务器,由备用服务器执行主服务器的任务,MySQL要实现这样的功能就必须保证主备服务器的数据一致.这就要用到MySQL主从双机.
<!--more-->
本文使用环境:
系统:CentOS 5.5 32位
主MySQL: ip 192.168.3.101/24 主机名:master.org
备用MySQL:192.168.3.102/24   主机名:slave.org
vip:192.168.3.103/24
MySQL:mysql-5.0.95.tar.gz
heartbeat:Heartbeat-3-0-7e3a82377fa8.tar.bz2
### 一、安装部署MySQL ###
```bash 
yum -y install ncurses-devel openssl-devel
wget http://dev.mysql.com/get/Downloads/MySQL-5.0/mysql-5.0.95.tar.gz/from/http://mysql.cdpa.nsysu.edu.tw/
useradd -M -s /sbin/nologin mysql
tar -zxvf mysql-5.0.95.tar.gz
cd mysql-5.0.95
./configure --prefix=/usr/local/mysql \
--without-debug \
--with-extra-charsets=utf8,gbk \
--enable-assembler \
--with-mysqld-ldflags=-all-static \
--with-client-ldflags=-all-static \
--with-unix-socket-path=/tmp/mysql.sock \
--with-ssl
make &amp;&amp; make install
cp support-files/my-medium.cnf /etc/my.cnf          # 创建配置文件
cp support-files/mysql.server /etc/init.d/mysqld     # 创建启动脚本
chmod +x /etc/init.d/mysqld
echo '/usr/local/mysql/lib/mysql/' &gt;&gt; /etc/ld.so.conf
ldconfig
/usr/local/mysql/bin/mysql_install_db --user=mysql   # 初始化数据库
chown -R root.mysql /usr/local/mysql/
chown -R mysql.mysql /usr/local/mysql/var/
ln -s /usr/local/mysql/bin/* /usr/local/bin/ # 为二进制文件做一个软链接
```
配置MySQl主从实现数据同步,在主从服务器上修改my.cnf(这里是新安装的数据库,如果是仅仅加从库,需要把主库的数据备份导入到从库,这里不再讲述)
```bash 
vi /etc/my.cnf
# [mysqld]里修改:
log_bin = /var/log/mysql/mysql-bin.log      # 启动二进制文件
server-id = 1921683101                      # 设置服务器id
```
启动主库:
```bash 
service mysqld start
```
在主库上创建一个用户授权给从库,用户为backup密码为backup:
```bash 
mysql&gt; grant replication slave on *.* to 'backup'@'192.168.3.102' identified by 'backup';
Query OK, 0 rows affected (0.16 sec)
```
查看主库状态:
```bash 
mysql&gt; show master status;
+------------------+-----------+--------------+------------------+
¦ File             ¦ Position  ¦ Binlog_Do_DB ¦ Binlog_Ignore_DB ¦
+------------------+-----------+--------------+------------------+
¦ mysql-bin.000003 ¦       236 ¦              ¦                  ¦
+------------------+-----------+--------------+------------------+
1 row in set (0.00 sec)
```
修改从库配置文件:
```bash 
server-id = 1921683102                 # server id必须保持唯一
log_bin = /var/log/mysql/mysql-bin.log # 启用二进制日志
master-host = 192.168.3.101       # 主库ip
master-user = backup                  # 账号
master-pass = backup                 # 密码
master-port = 3306                      # 连接主库的端口
master-connect-retry=60             # 连接失败后进行重试等待的描述
```
启动从库,并查看状态:
```bash 
service mysqld start
```
在从库上执行下操作,指定主库的二进制文件名和偏移量(刚才在主库show master status;查看的参数):
```bash 
mysql&gt; show slave status \G;
*************************** 1. row ***************************
             Slave_IO_State: Waiting for master to send event
                Master_Host: 192.168.3.101
                Master_User: backup
                Master_Port: 3306
              Connect_Retry: 60
            Master_Log_File: mysql-bin.000003
        Read_Master_Log_Pos: 236
             Relay_Log_File: cfhost-relay-bin.000002
              Relay_Log_Pos: 235
      Relay_Master_Log_File: mysql-bin.000003
           Slave_IO_Running: Yes
          Slave_SQL_Running: Yes
            Replicate_Do_DB:
        Replicate_Ignore_DB:
         Replicate_Do_Table:
     Replicate_Ignore_Table:
    Replicate_Wild_Do_Table:
Replicate_Wild_Ignore_Table:
                 Last_Errno: 0
                 Last_Error:
               Skip_Counter: 0
        Exec_Master_Log_Pos: 236
            Relay_Log_Space: 235
            Until_Condition: None
             Until_Log_File:
              Until_Log_Pos: 0
         Master_SSL_Allowed: No
         Master_SSL_CA_File:
         Master_SSL_CA_Path:
            Master_SSL_Cert:
          Master_SSL_Cipher:
             Master_SSL_Key:
      Seconds_Behind_Master: 0
1 row in set (0.00 sec)

ERROR:
No query specified
```
如果show slave status \G;<span style="color: #ff0000;">Slave_SQL_Running: No</span><span style="color: #000000;">,则执在从库上执行下面命令(两个参数值通过在主库执行show master status; 命令查看获得):
</span>
```bash 
mysql&gt; stop slave;
Query OK, 0 rows affected (0.00 sec)

mysql&gt; change master to master_log_file='mysql-bin.000003',master_log_pos=236;
Query OK, 0 rows affected (0.01 sec)
```
&nbsp;

在主库上创建一个数据库看看是否同步.
### 二、安装部署heartbeat实现双机热备份 ###
### 安装依赖 ###
```bash 
yum  -y install pkgconfig glib2-devel python-devel pam-devel gnutls-devel swig
```
安装libnet
```bash 
wget http://download.fedora.redhat.com/pub/epel/5/i386/libnet-1.1.5-1.el5.i386.rpm
rpm -ivh libnet-1.1.5-1.el5.i386.rpm
wget http://download.fedora.redhat.com/pub/epel/5/i386/libnet-devel-1.1.5-1.el5.i386.rpm
rpm -ivh libnet-devel-1.1.5-1.el5.i386.rpm
```
### 安装: ###
```bash 
useradd -M -s /sbin/nologin hacluster
useradd -M -s /sbin/nologin haclient
wget http://www.ultramonkey.org/download/heartbeat/2.0.8/heartbeat-2.0.8.tar.gz
tar -zxvf heartbeat-2.0.8.tar.gz
cd heartbeat-2.0.8
./configure --sysconfdir=/etc
make &amp;&amp; make install
```
创建配置文件:
安装后要配置三个文件（如没有可手动建立）：ha.cf、haresources、authkeys。这三个配置文件需要在/etc/ha.d目录下面，但是默认是没有这三个文件的，可以到官网上下这三个文件，也可以在源码包里找这三个文件，在源码目录下的DOC子目录里。
```bash 
cat /usr/local/share/doc/heartbeat-2.0.8/ha.cf | egrep -v '^#\W' | grep -v '^#$' &gt;&gt; /etc/ha.d/ha.cf
cat /usr/local/share/doc/heartbeat-2.0.8/haresources  | egrep -v '^#\W' | grep -v '^#$' &gt;&gt; /etc/ha.d/haresources
cat /usr/local/share/doc/heartbeat-2.0.8/authkeys | egrep -v '^#\W' | grep '^#$' -v &gt; /etc/ha.d/authkeys
```
编辑配置文件:

编辑ha.cf,该文件中包括为Heartbeat使用何种介质通路和如何配置他们的信息.
```bash 
 vi /etc/ha.d/ha.cf 

debugfile /var/log/ha-debug   # 用于记录heartbeat的调试信息
logfile /var/log/ha-log       # 用于记录heartbeat的日志信息
logfacility     local0
keepalive 2         # 设置心跳间隔
watchdog /dev/watchdog
deadtime 30              #  在30秒后宣布节点死亡
warntime 10              # 在日志中发出“late heartbeat“警告之前等待的时间，单位为秒
initdead 120             # 网络启动时间
udpport        694       # 广播/单播通讯使用的udp端口
#baud   19200
#serial  /dev/ttyS0      # 使用串口heartbeat
bcast   eth0             # 使用网卡heartbeat,并在eth0接口上使用广播heartbeat
auto_failback on         # 当主节点从故障中恢复时,将自动切换到主节点
watchdog /dev/watchdog   # 该指令是用于设置看门狗定时器，如果节点一分钟内都没有心跳，那么节点将重新启动
node master.org          # 集群中机器的主机名，与“uname –n”的输出相同。
node slave.org
ping 192.168.3.254       # ping网关来检测链路正常
respawn hacluster /usr/local/lib/heartbeat/ipfail # respawn调用/usr/lib/heartbeat/ipfail来主动进行切换
apiauth ipfail gid=haclient uid=hacluster   # 设置启动ipfail的用户和组
```
配置haresources ,该文件列出所有节点所提供的服务以及服务的默认所有者.所有节点上的该文件必须相同
```bash 
vi /etc/ha.d/haresources

master.org    IPaddr::192.168.3.103 mysql  # vip
```
`注意:!!`haresources最后一个字段是某个服务的心跳,如果mysql,如果主从库使用的是同一台盘阵或者一个分布式文件系统,这里一定要填写真实的启动脚本(/etc/init.d下),如果是主从同步的话请务必不填写真正的启动脚本,因为主库心跳存活的话heartbeat会自动停止从库的mysql,这样就无法同步,主库发生故障时转移故障就没有意义.

配置authkeys, authkeys决定了您的认证密钥。共有三种认证方式：crc，md5，和sha1果您的Heartbeat运行于 安全 网络之上，如本例中的交叉线，可以使用crc，从资源的角度来看，这是代价最低的方法。如果网络并不 安全 ，但您也希望降低CPU使用，则使用md5。最后，如果您想得到最好的认证，而不考虑CPU使用情况，则使用sha1，它在三者之中最难破解。
```bash 
vi /etc/ha.d/authkeys

auth 1
1 crc
chmod 600 /etc/ha.d/authkeys
```
不论您在关键字auth后面指定的是什么索引值，在后面必须要作为键值再次出现。如果您指定“auth 4”，则在后面一定要有一行的内容为“4 ”。
配置从库:
```bash 
scp root@192.168.3.101:/etc/ha.d/ha.cf /etc/ha.d/
scp root@192.168.3.101:/etc/ha.d/authkeys /etc/ha.d/
scp root@192.168.3.101:/etc/ha.d/haresources /etc/ha.d/
vi /etc/ha.d/ha.cf
debugfile /var/log/ha-debug
logfile /var/log/ha-log
logfacility     local0
keepalive 2
deadtime 30
warntime 10
initdead 120
udpport 694
bcast   eth0          
auto_failback on
node    master.org
node    slave.org
ping 192.168.3.254
respawn hacluser /usr/local/lib/heartbeat/ipfail # respawn调用/usr/lib/heartbeat/ipfail来主动进行切换
apiauth ipfail gid=haclient uid=hacluster
```
启动主库heartbeat:
```bash 
server heartbeat start
```
查看日志:
```bash 
cat /var/log/ha-log

heartbeat[32239]: 2012/02/19_13:45:29 info: Link 192.168.3.254:192.168.3.254 up.
heartbeat[32239]: 2012/02/19_13:45:29 info: Status update for node 192.168.3.254: status ping
heartbeat[32239]: 2012/02/19_13:45:29 info: Link master.org:eth0 up.
heartbeat[32239]: 2012/02/19_13:45:41 WARN: node slave.org: is dead
heartbeat[32239]: 2012/02/19_13:45:41 info: Comm_now_up(): updating status to active
heartbeat[32239]: 2012/02/19_13:45:41 info: Local status now set to: 'active'
heartbeat[32239]: 2012/02/19_13:45:41 info: Starting child client "/usr/local/lib/heartbeat/ipfail" (503,503)
heartbeat[32239]: 2012/02/19_13:45:41 WARN: No STONITH device configured.
heartbeat[32239]: 2012/02/19_13:45:41 WARN: Shared disks are not protected.
heartbeat[32239]: 2012/02/19_13:45:41 info: Resources being acquired from slave.org.
heartbeat[32247]: 2012/02/19_13:45:41 info: Starting "/usr/local/lib/heartbeat/ipfail" as uid 503  gid 503 (pid 32247)
harc[32248]:    2012/02/19_13:45:42 info: Running /etc/ha.d/rc.d/status status
mach_down[32275]:       2012/02/19_13:45:42 info: /usr/local/lib/heartbeat/mach_down: nice_failback: foreign resources acquired
mach_down[32275]:       2012/02/19_13:45:42 info: mach_down takeover complete for node slave.org.
heartbeat[32239]: 2012/02/19_13:45:42 info: mach_down takeover complete.
heartbeat[32239]: 2012/02/19_13:45:42 info: Initial resource acquisition complete (mach_down)
IPaddr[32300]:  2012/02/19_13:45:42 INFO:  Resource is stopped
heartbeat[32249]: 2012/02/19_13:45:42 info: Local Resource acquisition completed.
harc[32338]:    2012/02/19_13:45:42 info: Running /etc/ha.d/rc.d/ip-request-resp ip-request-resp
ip-request-resp[32338]: 2012/02/19_13:45:42 received ip-request-resp IPaddr::192.168.3.103 OK yes
ResourceManager[32353]: 2012/02/19_13:45:42 info: Acquiring resource group: master.org IPaddr::192.168.3.103 mysqld
IPaddr[32377]:  2012/02/19_13:45:42 INFO:  Resource is stopped
ResourceManager[32353]: 2012/02/19_13:45:42 info: Running /etc/ha.d/resource.d/IPaddr 192.168.3.103 start
IPaddr[32429]:  2012/02/19_13:45:42 INFO: Using calculated nic for 192.168.3.103: eth0
IPaddr[32429]:  2012/02/19_13:45:42 DEBUG: Using calculated netmask for 192.168.3.103: 255.255.255.0
IPaddr[32429]:  2012/02/19_13:45:42 DEBUG: Using calculated broadcast for 192.168.3.103: 192.168.3.255
IPaddr[32429]:  2012/02/19_13:45:42 INFO: eval /sbin/ifconfig eth0:0 192.168.3.103 netmask 255.255.255.0 broadcast 192.168.3.255
IPaddr[32429]:  2012/02/19_13:45:43 DEBUG: Sending Gratuitous Arp for 192.168.3.103 on eth0:0 [eth0]
IPaddr[32420]:  2012/02/19_13:45:43 INFO:  Success
ResourceManager[32353]: 2012/02/19_13:45:43 info: Running /etc/init.d/mysqld  start
heartbeat[32239]: 2012/02/19_13:45:56 info: Local Resource acquisition completed. (none)
heartbeat[32239]: 2012/02/19_13:45:56 info: local resource transition completed.
```
&nbsp;

从日志中看出来slave.org没起来是死亡的,并添加192.168.3.103vip

启动从库heartbeat
```bash 
server heartbeat start
```
启动之后查看日志信息
```bash 
Feb 19 13:50:22 slave heartbeat: [29159]: info: Local status now set to: 'up'
Feb 19 13:50:23 slave heartbeat: [29159]: info: Link master.org:eth0 up.
Feb 19 13:50:23 slave heartbeat: [29159]: info: Status update for node master.org: status active
Feb 19 13:50:23 slave heartbeat: [29159]: info: Link 192.168.3.254:192.168.3.254 up.
Feb 19 13:50:23 slave heartbeat: [29159]: info: Status update for node 192.168.3.254: status ping
Feb 19 13:50:23 slave heartbeat: [29159]: info: Link slave.org:eth0 up.
Feb 19 13:50:23 slave harc[29171]: info: Running /etc/ha.d/rc.d/status status
Feb 19 13:50:24 slave heartbeat: [29159]: info: Comm_now_up(): updating status to active
Feb 19 13:50:24 slave heartbeat: [29159]: info: Local status now set to: 'active'
Feb 19 13:50:24 slave heartbeat: [29159]: info: Starting child client "/usr/local/lib/heartbeat/ipfail" (501,501)
Feb 19 13:50:24 slave heartbeat: [29159]: WARN: G_CH_dispatch_int: Dispatch function for read child took too long to execute: 140 ms (&gt; 50 ms) (GSource: 0x9b98448)
Feb 19 13:50:24 slave heartbeat: [29182]: info: Starting "/usr/local/lib/heartbeat/ipfail" as uid 501  gid 501 (pid 29182)
Feb 19 13:50:24 slave heartbeat: [29159]: info: remote resource transition completed.
Feb 19 13:50:24 slave heartbeat: [29159]: info: remote resource transition completed.
Feb 19 13:50:24 slave heartbeat: [29159]: info: Local Resource acquisition completed. (none)
Feb 19 13:50:25 slave heartbeat: [29159]: info: master.org wants to go standby [foreign]
Feb 19 13:50:26 slave heartbeat: [29159]: info: standby: acquire [foreign] resources from master.org
Feb 19 13:50:26 slave heartbeat: [29183]: info: acquire local HA resources (standby).
Feb 19 13:50:26 slave heartbeat: [29183]: info: local HA resource acquisition completed (standby).
Feb 19 13:50:26 slave heartbeat: [29159]: info: Standby resource acquisition done [foreign].
Feb 19 13:50:26 slave heartbeat: [29159]: info: Initial resource acquisition complete (auto_failback)
Feb 19 13:50:27 slave heartbeat: [29159]: info: remote resource transition completed.
Feb 19 13:50:36 slave ipfail: [29182]: info: Ping node count is balanced.
Feb 19 13:50:37 slave ipfail: [29182]: info: Giving up foreign resources (auto_failback).
Feb 19 13:50:37 slave ipfail: [29182]: info: Delayed giveup in 4 seconds.
Feb 19 13:50:42 slave ipfail: [29182]: info: giveup() called (timeout worked)
Feb 19 13:50:42 slave heartbeat: [29159]: info: slave.org wants to go standby [foreign]
Feb 19 13:50:43 slave heartbeat: [29159]: info: standby: master.org can take our foreign resources
Feb 19 13:50:43 slave heartbeat: [29194]: info: give up foreign HA resources (standby).
Feb 19 13:50:43 slave ResourceManager[29204]: info: Releasing resource group: master.org IPaddr::192.168.3.103 mysqld
Feb 19 13:50:43 slave ResourceManager[29204]: info: Running /etc/init.d/mysqld  stop
Feb 19 13:50:45 slave ResourceManager[29204]: info: Running /etc/ha.d/resource.d/IPaddr 192.168.3.103 stop
Feb 19 13:50:45 slave IPaddr[29279]: INFO:  Success
Feb 19 13:50:45 slave heartbeat: [29194]: info: foreign HA resource release completed (standby).
Feb 19 13:50:45 slave heartbeat: [29159]: info: Local standby process completed [foreign].
Feb 19 13:50:46 slave heartbeat: [29159]: WARN: 1 lost packet(s) for [master.org] [162:164]
Feb 19 13:50:46 slave heartbeat: [29159]: info: remote resource transition completed.
Feb 19 13:50:46 slave heartbeat: [29159]: info: No pkts missing from master.org!
Feb 19 13:50:46 slave heartbeat: [29159]: info: Other node completed standby takeover of foreign resources.
```
现在尝试停止主库的MySQL服务
```bash 
pkill mysqld
```
查看日志并无变化,所以得出结论heartbeat只检测心跳也就是只检测设备是否宕机,不会检测MySQL服务,所以我们同样要有一个脚本来检测MySQL服务,如果mysql服务宕掉,则尝试启动服务,若启动服务失败则kill掉heartbeat进程实现故障转移(和上一遍nginx+keepalived原理一致),脚本内容如下:
```bash 
#!/bin/bash
# filename:mysqlsc.sh
ps aux ¦ grep mysqld ¦ grep -v grep 2&gt; /dev/null 1&gt;&amp;2   # 过滤mysql进程
if [[ $? -eq 0 ]]               # 如果过滤有mysql进程会返回0则认为mysql存活
then
    sleep 5                     # 使脚本进入休眠
else
# 如果nginx没有存活尝试启动mysql,如果失败则杀死heartbeat的进程
    /etc/init.d/mysqld start
    ps aux ¦ grep mysqld ¦ grep -v grep 2&gt; /dev/null 1&gt;&amp;2
    if [[ $? -eq 0 ]]
    then
        pkill heartbeat
    fi
fi
```
给这个脚本执行权限然后后台运行:
```bash 
chmod +x mysqlsc.sh
nohup sh mysqlsc.sh &amp; # 后台运行
```
下面来尝试停止主库的heartbeat:
```bash 
service heartbeat stop
```
查看从库日志:
```bash 
heartbeat[29159]: 2012/02/19_14:03:05 info: Received shutdown notice from 'master.org'.
heartbeat[29159]: 2012/02/19_14:03:05 info: Resources being acquired from master.org.
heartbeat[29308]: 2012/02/19_14:03:05 info: acquire local HA resources (standby).
heartbeat[29308]: 2012/02/19_14:03:05 info: local HA resource acquisition completed (standby).
heartbeat[29159]: 2012/02/19_14:03:05 info: Standby resource acquisition done [foreign].
heartbeat[29309]: 2012/02/19_14:03:05 info: No local resources [/usr/local/lib/heartbeat/ResourceManager listkeys slave.org] to acquire.
harc[29328]:    2012/02/19_14:03:05 info: Running /etc/ha.d/rc.d/status status
mach_down[29338]:       2012/02/19_14:03:05 info: Taking over resource group IPaddr::192.168.3.103
ResourceManager[29358]: 2012/02/19_14:03:05 info: Acquiring resource group: master.org IPaddr::192.168.3.103 mysqld
IPaddr[29382]:  2012/02/19_14:03:05 INFO:  Resource is stopped
ResourceManager[29358]: 2012/02/19_14:03:06 info: Running /etc/ha.d/resource.d/IPaddr 192.168.3.103 start
IPaddr[29434]:  2012/02/19_14:03:06 INFO: Using calculated nic for 192.168.3.103: eth0
IPaddr[29434]:  2012/02/19_14:03:06 DEBUG: Using calculated netmask for 192.168.3.103: 255.255.255.0
IPaddr[29434]:  2012/02/19_14:03:06 DEBUG: Using calculated broadcast for 192.168.3.103: 192.168.3.255
IPaddr[29434]:  2012/02/19_14:03:06 INFO: eval /sbin/ifconfig eth0:0 192.168.3.103 netmask 255.255.255.0 broadcast 192.168.3.255
IPaddr[29434]:  2012/02/19_14:03:06 DEBUG: Sending Gratuitous Arp for 192.168.3.103 on eth0:0 [eth0]
IPaddr[29425]:  2012/02/19_14:03:06 INFO:  Success
ResourceManager[29358]: 2012/02/19_14:03:06 info: Running /etc/init.d/mysqld  start
mach_down[29338]:       2012/02/19_14:03:07 info: /usr/local/lib/heartbeat/mach_down: nice_failback: foreign resources acquired
mach_down[29338]:       2012/02/19_14:03:07 info: mach_down takeover complete for node master.org.
heartbeat[29159]: 2012/02/19_14:03:07 info: mach_down takeover complete.
heartbeat[29159]: 2012/02/19_14:03:17 WARN: node master.org: is dead
heartbeat[29159]: 2012/02/19_14:03:17 info: Dead node master.org gave up resources.
heartbeat[29159]: 2012/02/19_14:03:17 info: Link master.org:eth0 dead.
```
