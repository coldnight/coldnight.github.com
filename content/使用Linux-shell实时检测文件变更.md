Title: 使用Linux shell实时检测文件变更
Tags: 检测,文件,实时,变更,uwsgi,shell,python,Linux,bottle
Category: shell
Date: 2012-05-18 11:16
使用python做web开发,现在流行使用uwsgi调用python程序,但是使用uwsgi一段时间发现有一个弊端,就是每次更改源代码后必须重启uwsgi才能生效,包括更改模板文件也是,我是个懒人,再经过一段时间反复的更改-重启后我终于忍受不了,决定写一个脚本来定时程序目录的文件改动,并及时自动重启uwsgi,来解放我的双手可以不用理会这些琐碎的重启工作. 用了点时间来编写了一个脚本用来判断是否更改,然后判断是否需要重启uwsgi.

下面放出脚本内容:
```bash
#!/bin/bash
# Author      : cold
# Homepage    : http://www.linuxzen.com
# Filename    : checkchange.sh
# Useage      : sh checkchange.sh [dir]
checkisdir()
        # Have one argument
        # The argument is a directory
        for i in `ls $1 | sed -e 's/ /\n/g'`
        do
                if [ -d $1/$i ]
                then
                        if [ $i == "bin" -o $i == "lib" -o $i == "include" ]   # 不想检测的目录(这里是使用virtualenv生成的环境文件)
                        then
                                continue
                        fi
                        dir="$1/$i"
                        checkisdir $dir
                else
                        files=$files'\n'$1'/'$i
                fi
        done
        echo -e $files
}
while true
do
        if [ -e /tmp/stat.tmp ]
        then
                for i in `checkisdir $1`
                do
                        if [ -e /tmp/patch.tmp ]
                        then
                                stat $i | grep Change > /tmp/nstat.tmp
                                rm -f /tmp/patch.tmp
                                continue
                        fi
                        stat $i | grep Change >> /tmp/nstat.tmp
                done
                diff /tmp/stat.tmp /tmp/nstat.tmp > /tmp/patch.tmp
                if [ $? -eq 0 ]
                then
                        sleep 10
                else
                        /etc/init.d/uwsgi.py restart                    # 将此处更改为想要做的操作
                        patch /tmp/stat.tmp /tmp/patch.tmp
                fi
        else
                for i in `checkisdir $1`
                do
                        stat $i | grep Change >> /tmp/stat.tmp
                done
                continue
        fi
done
```
这里主要测试变更后重启uwsgi,使用方法:我的bottle程序在/code/python下:
```bash
sh checkchange.sh /code/python &
```
如果使用svn可以参考下面代码:
```bash
#!/bin/bash
# Author        : cold
# Homepage      : http://www.linuxzen.com
# Filename      : checkupdate.sh
# Describle     : To Check update of svn

while true
do
        cd /code/python
        svn up | grep At > /dev/null 2>&1
        if [ $? -eq 0 ]
        then
                sleep 30
        fi

        svn up | grep Updated > /dev/null 2>&1
        if [ $? -eq 0 ]
        then
                /etc/init.d/uwsgi.py restart
        fi
done
```
