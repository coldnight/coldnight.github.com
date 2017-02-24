Title: lnmp环境搭建完全手册(三)——应用安装(yum配置和包安装)
Tags: yum,nginx,man,lnmp,Linux,CentOS
Category: Linux
Date: 2012-03-13 10:06
上两篇文章我给大家讲解了系统的安装,网络和ssh的配置和连接,这篇就给大家讲一下Linux如何安装包.CentOS使用rpm包管理,rpm包安装使用rpm命令

比如你有一个包为:
```bash
abc-0.1.3-el5.centos.rpm
```
包的名称由3部分组成,第一部分是包名,第二部分是版本号,第三部分是使用平台.如果想安装这个包使用如下命令安装:
```bash
rpm -ivh abc-0.1.3-el5.centos.rpm    # i是安装 v是显示详细信息,-h显示安装进度
```
卸载这个包使用:
```bash
rpm -e abc
```
卸载只要指定包名即可卸载.

因为Linux下的包依赖关系很复杂,一个包可能会有很多的依赖包,必须把所有依赖包都装了才能安装这个包.所以使用rpm安装包,就变得很麻烦很头疼.如果想工作变的轻松,我们可以借助工具,在rhel5发行的时候就集成了一个工具yum.Yum（全称为 Yellow dog Updater, Modified）是一个在Fedora和RedHat以及SUSE、CentOS中的Shell前端软件包管理器。基於RPM包管理，能够从指定的服务器自动下载RPM包并且安装，可以自动处理依赖性关系，并且一次安装所有依赖的软体包，无须繁琐地一次次下载、安装。

yum的使用需要指定一个yum仓库,我们首先来配置一个yum仓库,yum仓库可以是internet上提供的,但是rhel的yum库是收费的,但是我们用的CentOS的yum库是免费的.yum仓库还可以使用本地的yum库.我们这里使用本地yum库.资源就是使用我们的安装DVD盘.首先将我们的DVD盘放到虚拟光驱,然后在Linux下挂载光盘.
右键虚拟机标签-&gt;Setting(设置),选择CD/DVD(IDE),在右边点选Use ISO image file,然后点"Browse..",浏览找到CentOS 5.5 安装DVD盘.然后选择右上角的connect:
![www.linuxzen.com](/upload/QQ截图20120311164241.jpg)
![wwww.linuxzen.com](/upload/QQ截图20120311164417.jpg)

然后ok保存,在终端或在你的ssh客户端上输入下面命令:
```bash
mount /dev/hdc /mnt    # 将光盘挂载到/mnt下
```
然后修改yum配置文件,首先我们不是用默认提供的官方源,我们把CentOS-Base.repo重命名一下:
```bash
mv /etc/yum.repos.d/CentOS-Base.repo /etc/yum.repos.d/CentOS-Base.repo.bak
```
然后修改本地源配置文件:
```bash
vi /etc/yum.repos.d/CentOS-Media.repo
```
按i进入输入模式,修改成如下内容
```bash
[c5-media]
name=CentOS-$releasever - Media
baseurl=file:///mnt                           # 源为挂载光盘的目录/mnt下
#       file:///media/CentOS/
#       file:///media/cdrom/
#       file:///media/cdrecorder/
gpgcheck=1
enabled=1                                     # 改为1启用这个源
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-CentOS-5
```
```
修改完毕按`Esc`进入命令模式,输入`:`进入末行模式,在末行模式输入`wq`保存退出.

修改完毕后执行:
```bash
yum update
```
会看到类似下面的输入,表示我们的配置是成功的:
![www.linuxzen.com](/upload/QQ截图20120313095321.jpg)

配置完成后我们用他来安装几个常用的工具,
```bash
yum -y install setuptool lrzsz wget gcc gcc-c++ make which bzip2 groff man
```
来介绍一下这几个工具,setuptool可以在终端界面下提供一个类似安装界面的图形化配置界面.
lrzsz可以通过其提供的命令rz/sz过ssh客户端对服务器进行上传和下载
wget是Linux下优秀的下载工具(http/ftp)
gcc是一款C/C++的编译器
make使用gcc根据makefile进行编译源代码
which是一个查找命令的工具
bzip2是一个压缩软件一般供tar调用
man是一个查看手册命令,如果碰到不懂的命令可以man一下.比如你不知道wget命令的用法可以:
```bash 
man wget
```
然后man命令就给提供这个命令的详细手册.好了一些准备工作到这里已经做好了.接下来的文章我们就介绍安装nginx.
