Title: lnmp环境搭建完全手册(一)------系统安装
Tags: 虚拟机,操作系统,安装,VMware,Linux,CentOS
Category: Linux
Date: 2012-03-08 12:05
在之前的文章里也陆陆续续的介绍了nginx/mysql和php的安装和配置,这里做一个整合.也是一个详细的教程,可以让你0基础搭建lnmp环境.
首先选择一个发行版,个人比较喜欢CentOS,CentOS是Red Hat的再发行版,重新编译了Red Hat,修复n多错误,免费的yum库,这里使用CentOS5.5,虚拟机使用VMware Workstation 8.0.VMware是一个“虚拟PC”软件公司.它可以使你在一台机器上同时运行二个或更多Windows、DOS、LINUX系统。与“多启动”系统相比，VMWare采用了完全不同的概念。多启动系统在一个时刻只能运行一个系统，在系统切换时需要重新启动机器。VMWare是真正“同时”运行，多个操作系统在主系统的平台上，就象标准Windows应用程序那样切换.

安装VMware这里就不介绍了,给出VMware Workstation 8.0的[下载地址](http://www.vmware.com/downloads/downloadBinary.do?downloadGroup=WKST-802-WIN&amp;vmware=downloadBinary&amp;file=VMware-workstation-full-8.0.2-591240.exe&amp;pot=1&amp;code=VMware-workstation-full-8.0.2-591240.exe&amp;hashKey=5d70214cc3ce52f8154f38b7cd52efbe&amp;tranId=70310191&amp;downloadURL")

VMware不是免费软件,但是网上会有序列号,大家不想花钱就搜一下就ok了.
我们还要准备CentOS5.5 的iso镜像用来安装系统.[下载地址]("http://download.chinaunix.net/down.php?id=31679&amp;ResourceID=12271&amp;site=6)

下面我们就来新建一个虚拟机安装CentOS
打开虚拟机选择File-&gt;New Virtual Machine
![www.linuxzen.com](/upload/1.jpg)

在接下来的界面选择`Typical`,然后点`Next`:
![www.linuxzen.com](/upload/2.jpg)

然后点选`I will install the operating system later`,然后Next&gt;,选择操作系统,为Linux,Version选择为CentOS:
![www.linuxzen.com](/upload/3.jpg)

点击Next进给虚拟机命名,然后选择保存路径(最好选择一个空闲空间足够的分区)点击Next会让你选择硬盘大小,保持默认即可,点击Next&gt;在弹出的界面可以查看虚拟机的虚拟硬件可以点击"Customize Hardware"进行自定义硬件,建议删除软驱,USB等无用硬件.点击Finish完成虚拟机创建.

创建完成后虚拟机界面的左边双击"CD/DVD(IDE)":
![www.linuxzen.com](/upload/4.jpg)
在弹出界面的右边选择`Use ISO image file`,点击"Browse.."选择下载好的CentOS5.5镜像,点击OK确定:

![www.linuxzen.com](/upload/QQ截图20120308101907.jpg)

然后单击虚拟机界面的左上角的`Power on this virtual machine`启动虚拟机,第一次默认会认到光驱从光驱启动,启动后看到如下界面:

![www.linuxzen.com](/upload/QQ截图20120308102129.jpg)

恭喜你,可以开始你的linux之旅了,我比较喜欢字符界面安装,节省资源,输入`linux text`,敲回车进入字符界面安装,想图形化安装直接回车即可:
在接下来的界面下会弹出下面对话框,按Tab选择Skip跳过检测:
![www.linuxzen.com](/upload/QQ截图20120308102415.jpg)
然后会一次弹出语言,键盘选择框选择默认既可以,然后linux会再你的硬盘上检测不到分区表询问你是否格式化整个硬盘,选择Yes:
![www.linuxzen.com](/upload/QQ截图20120308103037.jpg)
然后就会跳到分区这一块,我们选择第四个自定义分区(Create Custom layout):
![www.linuxzen.com](/upload/QQ截图20120308103159.jpg)

接下来按F2即可建立分区,一般分区有各种各样的建议,Linux必须有一个/(根)分区,建议有一个swap分区是物理内存的两倍.

![www.linuxzen.com](/upload/QQ截图20120308103443.jpg)

上图建立一个/大小为`4G`,`Mount Point`指定挂载点,这里为`/`,`File System Type`指定文件系统类型,这里为`ext3`.大小指定为`4G`.

然后建议一个`swap`分区,同样按`F2`在`File System Type`里选择为`swap`:
![www.linuxzen.com](/upload/QQ截图20120308103741.jpg)
在根据第一步的方法创建一个/usr分区,在Size里选择"Fill all available space"把剩余的大小都给/usr
![www.linuxzen.com](/upload/QQ截图20120308103957.jpg)
创建完毕选择ok完成分区,跳出是否安装引导(Boot loader Configuration),一直选择默认继续.接下来询问是否配置网卡,选择No跳过.会让你填Gateway和DNS不用管直接ok就行,接下来会让输入主机名,输入你想要的主机名即可
![www.linuxzen.com](/upload/QQ截图20120308104357.jpg)

接下来选择时区,选择Asia/Chongqing.然后输入root密码,输入完毕后OK继续,下面就是选择要安装的包,想玩桌面的话就直接默认,这里选择Customize software selection,接下来的界面按空格去掉所有的包前面的*实现最小化安装,选择OK开始安装.完成后重启,linux系统就安装完成
