Title: MySQL优化笔记
Tags: 编译参数,优化,mysql配置文件,MySQL服务,MySQL,my.cnf
Category: MySQL
Date: 2012-02-04 13:36
之前安装时没注意MySQL的优化,先想对MySQL做一下优化.首先看一下没有优化之前各个参数:
MySQL预编译参数:
```bash 
 ./configure  --prefix=/usr/local/mysql --with-ssl --with-readline --with-big-tables --enable-assembler
```
top
```bash 
32553 mysql     20   0  125m  17m 4064 S  0.0  1.7   5:13.01 mysqld
```
使用mysqlreport获取MySQL运行参数:<!--more-->
```bash 
MySQL 5.0.40-log         uptime 15 22:1:21      Sat Feb  4 10:04:23 2012

__ Key _________________________________________________________________
Buffer used    62.00k of  16.00M  %Used:   0.38
  Current       1.90M            %Usage:  11.89
Write hit      22.29%
Read hit       99.83%

__ Questions ___________________________________________________________
Total          85.58k     0.1/s
  DMS          77.61k     0.1/s  %Total:  90.69
  Com_          5.37k     0.0/s            6.28
  COM_QUIT      2.52k     0.0/s            2.95
  +Unknown         78     0.0/s            0.09
Slow 10 s           0       0/s            0.00  %DMS:   0.00  Log: OFF
DMS            77.61k     0.1/s           90.69
  SELECT       72.77k     0.1/s           85.03         93.76
  UPDATE        2.68k     0.0/s            3.13          3.45
  INSERT        1.09k     0.0/s            1.27          1.41
  DELETE        1.07k     0.0/s            1.25          1.38
  REPLACE           0       0/s            0.00          0.00
Com_            5.37k     0.0/s            6.28
  set_option    2.60k     0.0/s            3.04
  change_db     2.52k     0.0/s            2.94
  show_fields      77     0.0/s            0.09

__ SELECT and Sort _____________________________________________________
Scan            5.17k     0.0/s %SELECT:   7.10
Range           2.75k     0.0/s            3.77
Full join           0       0/s            0.00
Range check         0       0/s            0.00
Full rng join       0       0/s            0.00
Sort scan       5.97k     0.0/s
Sort range      4.30k     0.0/s
Sort mrg pass       0       0/s

__ Table Locks _________________________________________________________
Waited             24     0.0/s  %Total:   0.03
Immediate      91.00k     0.1/s

__ Tables ______________________________________________________________
Open               36 of   64    %Cache:  56.25
Opened             42     0.0/s

__ Connections _________________________________________________________
Max used            5 of  100      %Max:   5.00
Total           2.52k     0.0/s

__ Created Temp ________________________________________________________
Disk table      4.15k     0.0/s
Table           7.11k     0.0/s    Size:  32.0M
File                5     0.0/s

__ Threads _____________________________________________________________
Running             1 of    1
Cached              0 of    0      %Hit:   0.04
Created         2.52k     0.0/s
Slow                0       0/s

__ Aborted _____________________________________________________________
Clients             0       0/s
Connects            0       0/s

__ Bytes _______________________________________________________________
Sent          226.71M   164.8/s
Received       12.59M     9.2/s

__ InnoDB Buffer Pool __________________________________________________
Usage         304.00k of   8.00M  %Used:   3.71
Read hit       84.42%
Pages
  Free            493            %Total:  96.29
  Data             19                      3.71 %Drty:   0.00
  Misc              0                      0.00
  Latched           0                      0.00
Reads              77     0.0/s
  From file        12     0.0/s           15.58
  Ahead Rnd         1     0.0/s
  Ahead Sql         0       0/s
Writes              0       0/s
Flushes             0       0/s
Wait Free           0       0/s

__ InnoDB Lock _________________________________________________________
Waits               0       0/s
Current             0
Time acquiring
  Total             0 ms
  Average           0 ms
  Max               0 ms

__ InnoDB Data, Pages, Rows ____________________________________________
Data
  Reads            25     0.0/s
  Writes            3     0.0/s
  fsync             3     0.0/s
  Pending
    Reads           0
    Writes          0
    fsync           0

Pages
  Created           0       0/s
  Read             19     0.0/s
  Written           0       0/s

Rows
  Deleted           0       0/s
  Inserted          0       0/s
  Read              0       0/s
  Updated           0       0/s
```
首先在预编译参数上进行优化
```bash 
./configure --prefix=/usr/local/mysql \
--without-debug \                                 # 取消调试模式提高性能
--with-extra-charsets=utf8,gbk \                  # 仅仅指定需要的默认字符集提高性能
--enable-assembler \                              # 使用汇编模式提高性能
--with-mysqld-ldflags=-all-static \               # 以静态方式编译提高性能
--with-client-ldflags=-all-static \
--with-unix-socket-path=/tmp/mysql.sock \         # 使用unix socket提高性能
--with-ssl
```
安装完成后进一步优化my.cnf:
因为MySQL 只会 Cache 索引(*.MYI)，因此您只要将数据库中所有的 MYI 档案加总起来就是key buffer 的值,计算MYI档案的总大小:
```bash 
du -hc `find /usr/local/mysql/var/ -name *.MYI`
4.0K    /usr/local/mysql/var/myblog/wp_term_taxonomy.MYI
8.0K    /usr/local/mysql/var/myblog/wp_posts.MYI
8.0K    /usr/local/mysql/var/myblog/wp_usermeta.MYI
8.0K    /usr/local/mysql/var/myblog/wp_commentmeta.MYI
16K     /usr/local/mysql/var/myblog/wp_options.MYI
12K     /usr/local/mysql/var/myblog/wp_postmeta.MYI
8.0K    /usr/local/mysql/var/myblog/wp_comments.MYI
4.0K    /usr/local/mysql/var/myblog/wp_links.MYI
4.0K    /usr/local/mysql/var/myblog/wp_term_relationships.MYI
4.0K    /usr/local/mysql/var/myblog/wp_users.MYI
8.0K    /usr/local/mysql/var/myblog/wp_terms.MYI
16K     /usr/local/mysql/var/mysql/help_relation.MYI
4.0K    /usr/local/mysql/var/mysql/time_zone_name.MYI
16K     /usr/local/mysql/var/mysql/help_keyword.MYI
4.0K    /usr/local/mysql/var/mysql/func.MYI
4.0K    /usr/local/mysql/var/mysql/time_zone.MYI
20K     /usr/local/mysql/var/mysql/help_topic.MYI
4.0K    /usr/local/mysql/var/mysql/columns_priv.MYI
4.0K    /usr/local/mysql/var/mysql/procs_priv.MYI
4.0K    /usr/local/mysql/var/mysql/time_zone_leap_second.MYI
4.0K    /usr/local/mysql/var/mysql/user.MYI
4.0K    /usr/local/mysql/var/mysql/tables_priv.MYI
4.0K    /usr/local/mysql/var/mysql/host.MYI
4.0K    /usr/local/mysql/var/mysql/time_zone_transition_type.MYI
4.0K    /usr/local/mysql/var/mysql/proc.MYI
4.0K    /usr/local/mysql/var/mysql/help_category.MYI
4.0K    /usr/local/mysql/var/mysql/db.MYI
4.0K    /usr/local/mysql/var/mysql/time_zone_transition.MYI
192K    total
```
修改my.cnf参数大小:
```bash 
vi /etc/my.cnf
# 降低key_buffer的值
key_buffer = 4M
```
重启MySQL执行top命令:
```bash 
18125 mysql     20   0  109m  11m 2152 S  0.0  1.1   0:00.08 mysqld
```
看到MySQL的内存利用率降低到1.1,这时候还不适宜执行mysqlreport查看等待启动一天后查看.由于现在访问量较低,所以参数适量调低,需要实时监控MySQL运行状况适当运行参数.
