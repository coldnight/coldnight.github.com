Title: 将cfengine集成到自己定制的发行版
Tags: 集成,定制,发行版,Linux,cfengine
Category: Linux
Date: 2012-01-19 11:35
上一篇文章介绍如何定制自己的发行版,并且完全根据CentOS进行定制,我们看到了用cfengine管理Linux系统的优势,如何快速部署cfengine呢,我们把cfengine集成到自己的定制的Linux中.
本文根据编译好的cfengine来进行定制,大家都知道编译之前会有很多依赖,所以我们需要安装系统的时候把依赖装好,然后把编译好的二进制文件复制到当前系统中.

本文完全根据<a title="根据CentOS定制自己的发行版" href="/post/36" target="_blank">上一篇</a>文章而来,挂载光驱安装工具等等这里就不介绍了.

<!--more-->首先.记录cfengine依赖包的依赖关系,然后把这些包复制到定制发行版的目录下,cfengine所依赖的包如下:
```bash 
# 把依赖包添加到cfpkgs.list
vi cfpkgs.list
# 添加如下内容
keyutils-libs-devel
zlib-devel
e2fsprogs-devel 
libsepol-devel
libselinux-devel
db4-devel
pcre-devel
krb5-devel
flex
openssl-devel 
# 然后根据cfpkgs.list把包复制到发行版的CentOS目录下:
for i in `cat cfpkgs.list `:; do file=`echo $i ¦ sed 's/://g'`; cp /mnt/CentOS/$file* /usr/cold/CentOS/; done
```
复制完包之后就需要编辑comps.xml,
```bash 
cd /usr/cold/
# 编辑comps.xml
vi repodata/comps.xml
```
向comps.xml添加这几个包group节点
```bash 
  &lt;group&gt;
    &lt;id&gt;cfengine&lt;/id&gt;
    &lt;name&gt;Cfengine&lt;/name&gt;
    &lt;name xml:lang="en_GB"&gt;Cfengine&lt;/name&gt;
    &lt;description&gt;Cfengine &lt;/description&gt;
    &lt;description xml:lang="en_GB"&gt;Cfengine&lt;/description&gt;
    &lt;default&gt;true&lt;/default&gt;
    &lt;uservisible&gt;false&lt;/uservisible&gt;
    &lt;packagelist&gt;
      &lt;packagereq type="default"&gt;keyutils-libs-devel&lt;/packagereq&gt;
      &lt;packagereq type="default"&gt;zlib-devel&lt;/packagereq&gt;
      &lt;packagereq type="default"&gt;e2fsprogs-devel &lt;/packagereq&gt;
      &lt;packagereq type="default"&gt;libsepol-devel&lt;/packagereq&gt;
      &lt;packagereq type="default"&gt;libselinux-devel&lt;/packagereq&gt;
      &lt;packagereq type="default"&gt;db4-devel&lt;/packagereq&gt;
      &lt;packagereq type="default"&gt;pcre-devel&lt;/packagereq&gt;
      &lt;packagereq type="default"&gt;krb5-devel&lt;/packagereq&gt;
      &lt;packagereq type="default"&gt;flex&lt;/packagereq&gt;
      &lt;packagereq type="default"&gt;openssl-devel &lt;/packagereq&gt;
    &lt;/packagelist&gt;
  &lt;/group&gt;
```
然后通过命令创建源:
```bash 
createrepo -g repodata/comps.xml  CentOS/
```
计算comps.xml的sha值
```bash 
sha1sum repodata/comps.xml 
f6f086a3c2b7eee2050580aa3e74c841dd406dfc  repodata/comps.xml
```
编辑repomd.xml
```bash 
vi repodata/repomd.xml
```
将新的sha值更新到repomd.xml
```bash 
  &lt;data type="group"&gt;
    &lt;location href="repodata/comps.xml"/&gt;
    &lt;checksum type="sha"&gt;f6f086a3c2b7eee2050580aa3e74c841dd406dfc&lt;/checksum&gt;
    &lt;timestamp&gt;1272586365&lt;/timestamp&gt;
  &lt;/data&gt;
```
接下来把我们编译好的cfengine(根据<a title="安装配置cfengine实现自动化配置Linux/Unix服务器" href="/post/17" target="_blank">安装配置cfengine实现自动化配置Linux/Unix服务器</a>)复制到定制的发行版的目录下:
```bash 
cp -r /usr/local/cfengine/ /usr/cold/
```
然后编辑ks文件:
```bash 
vi isolinux/ks.cfg
# 在末尾添加如下内容
%packages
@core
@useful
@cfengine        # 添加cfengine软件组
# %post是安装后执行的脚本,
# 还有一种标记 %pre是安装前执行的脚本,但不推荐使用这种脚本,
# 因为脚本出错会导致安装失败 
# --nochroot是不切换根目录,这时候安装好的系统会挂载在 /mnt/sysimage下
%post --nochroot

# 创建cfengine的工作目录
mkdir -p /mnt/sysimage/var/cfengine/masterfiles
mkdir -p /mnt/sysimage/var/cfengine/inputs
mkdir -p /mnt/sysimage/var/cfengine/outputs
mkdir -p /mnt/sysimage/var/cfengine/bin
mkdir /a
mount /tmp/cdrom /a
# 复制二进制文件和cfengine
cp /a/cfengine/sbin/cf-* /mnt/sysimage/var/cfengine/bin/
cp -r /a/cfengine /mnt/sysimage/usr/local/
```
然后制作镜像:
```bash 
mkisofs -R -J -T -r -l -d -allow-multidot -allow-leading-dots -no-bak -o /usr/ColdOS-0.1-i386.iso -b isolinux/isolinux.bin -c isolinux/boot.cat -no-emul-boot -boot-load-size 4 -boot-info-table .
```
然后把镜像下载下来进行安装,安装完成后执行
```bash 
/var/cfengine/bin/cf-key              # 生成认证证书
/var/cfengine/bin/cf-agent --bootstrap --policy-server cfhubip
/var/cfengine/bin/cf-agent   # 执行承诺
```
即可从cfhubip那里下载承诺文件执行承诺,根据cfhub进行本地配置
还可以根据%post更加自由的定制自己的系统,比如最小化安装后会无法使用本地镜像来使用yum可以定义一个局域网yum,在%post --nochroot添加如下内容:
```bash 
cat &lt;&lt;__EOF__ &gt; /mnt/sysimage/etc/yum.repos.d/CentOS-Media.repo
# CentOS-Media.repo
#  yum --disablerepo=\* --enablerepo=cold [command]
[cold]
name=CentOS-$releasever - Media
baseurl=http://192.168.0.254/CentOS
gpgcheck=1
enabled=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-CentOS-5
__EOF__

mv /mnt/sysimage/etc/yum.repos.d/CentOS-Base.repo /mnt/sysimage/etc/yum.repos.d/CentOS-Base.repo.bak
```
这里就不详细介绍,大家可根据这个更加自由的来定制自己的Linux发行版.
