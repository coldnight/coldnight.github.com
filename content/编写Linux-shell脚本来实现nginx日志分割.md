Title: 编写Linux shell脚本来实现nginx日志分割
Tags: 日志,分割,shell,nginx,log,Linux,cut
Category: shell
Date: 2012-02-17 10:27
nginx的accss日志每天都会产生大量的日志,不过不进行切割会使查看日志变得异常艰难,这里编写一个脚本结合crond来实现nginx的日志切割,切割的格式为日志后缀的数字越小表示离当前日期越近,比如access.log.2存放的内容要比access.log.1的内容要早.

好了,废话不多说,脚本内容如下:
```bash 
#!/bin/sh
# Author   : cold night
# Filename : nglogcut.sh
export PATH=/usr/kerberos/sbin:/usr/kerberos/bin:/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin:/root/bin
LogPath='/usr/local/nginx/logs/access.log'    # 定义日志绝对路径

for i in `ls $LogPath* | awk -F'/' '{print $NF}' | sort -nr`
do
        LastNum=`echo $i | awk -F'.' '{print $NF}'`

        echo $LastNum | grep -E "[0-9]+" 2&gt;/dev/null 1&gt;&amp;2
        if [[ $? -eq 0 ]]
        then
                OnNum=`expr $LastNum + 1`
                mv $LogPath.$LastNum $LogPath.$OnNum
        else
                mv $LogPath $LogPath.1
                kill -HUP `cat /usr/local/nginx/logs/nginx.pid`
        fi
done
```
然后保存为nglogcut.sh存放在/root/下,下面给脚本赋予执行权限
```bash 
chmod +x /root/nglogcut.sh
```
配置crond自动执行:
```bash 
crontab -e    # 执行crontab -e为当前用户添加,但必须要再脚本前面声明PATH路径,或命令用绝对路径

# 添加如下内容:
00 2 */3 * * /bin/sh /root/nglogcut.sh 2&gt; /dev/null 1&gt;&amp;2 # 每3天执行日志分割(可根据自己情况来定义执行周期)
```
