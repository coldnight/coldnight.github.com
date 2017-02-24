Title: Linux一些比较实用的小技巧
Tags: 命令,sz,ssh上传下载,shell,sed,rz,lrzsz,Linux,find,awk
Category: shell
Date: 2012-02-20 11:30
在Linux中各种各样的小技巧可以帮助我们更好更快的完成我们的工作,下面就介绍一些我所知道的小技巧

## 文件查找 ##
找出最近修改的文件:
```bash 
find / -ctime 1 # 找出根下最近24小时修改过inode信息的文件(更改权限)
find / -mtime 1 # 找出根下最近24小时修改过的文件(内容)
find / -atime 1 # 找出根下最近24小时访问过的文件
```
使用通配符查找文件
```bash 
find / -name "*.log" # 找出根下以log为后缀的文件,这里必须要加双引号,不然会报错,因为找的是多个文件,需要用双引号引起来
```
## 文本替换 ##
```bash 
sed -e '2s/ext3/ext4/' /etc/fstab # 将第二行的ext3改成ext4
```
## 显示指定行 ##
```bash 
sed -n "3p" /etc/fstab         # 显示第3行
sed -n "3,5p" /etc/gfstab  # 显示第3到5行
sed -n "3p;5p" /etc/fstab    # 显示第3行和第5行
awk 'NR==3' /etc/fstab     # 显示第3行
```
## 在行首或行尾添加: ##
```bash 
sed -e 's/^/hello' test # 在test文件的行首添加hello
sed -e 's/$/hello' test # 在test文件的行尾添加hello
sed -e '3s/^/hello' test # 在test文件的第3行行首添加hello
```
## 在vi里执行命令: ##
```bash 
vi /etc/sysconfig/networ-scripts/ifcfg-eth0
# 打开vi,在末行模式下(ESC-&gt;:)

:r!cat /mnt/ip.txt           # 在当前行的下面输入ip.txt的内容,r代表命令输出放到下一行,!后面是要执行的命令
:.!cat /mnt/ip.txt           # 在当前行输入ip.txt的内容,.代表将命令输出放到当前航
```
## 执行上一条命令: ##
如果刚执行了一条server network restart,如果又做了一些操作,需要再次执行,按上键调处可以,还有更快捷的就是:
```bash 
!ser
```
!会在命令历史找与!后面匹配的最近一条命令.
## 通过ssh连接服务器上传下载文件: ##
当你通过ssh连接你的Linux的时候你想下载一个文件到本地,或者像把本地文件上传到远端,你想到用ftp吗?out了,当你通过ssh连接的时候Linux提供了两个命令 rz/sz,命令依赖于包lrzsz.rz命令可以将本地文件通过ssh上传到Linux上,sz pkg 可以将远端的pkg下载到本地.
```bash 
yum -y install lrzsz           # 安装
```
然后就可以通过rz/sz来上传下载文件,省去了文件服务器,也可更方便更快捷的管理你的服务器
##  windows文件转换为Linux格式的文件 ##
windows的文件格式比Linux格式的文件多了一个回车符\r,可以通过命令来实现转换:
直接转换,dos2unix依赖于包dos2unix:
```bash 
dos2unix file.txt     # 需要安装dos2unix包
```
通过删除\r来实现转换:
```bash 
tr -d '\r' file.txt &gt;&gt; file1.txt   # 通过tr命令删除\r字符,并重定向追加到file1.txt
```
