Title: 安装配置cfengine实现自动化配置Linux/Unix服务器
Tags: 自动配置,自动化,服务器,Unix,Linux,cfengine
Category: Linux
Date: 2012-01-17 23:06
cfengine（配置引擎）是一种 UNIX 管理工具，其目的是使简单的管理的任务自动化，使困难的任务变得较容易。Cfengine 适用于管理各种环境，从一台主机到上万台主机的机群均可使用. cfengine的版本2和版本3存在很大差异,这里使用最新版本的cfengine3:

cfengine执行和分发的策略被称为承诺,cfengine建议的结构是:

* 一个版本控制器(subversion)用来创建策略
* 多个策略分发器(cfenginehub)用来分发策略----server
* 多个策略执行器(cfengine host)用来执行策略-----client,

本文使用的环境为:

* cfhub: 172.16.1.1/24
* cfhost: 172.16.1.2/24

系统为CentOS 5.5 32位最小化安装,本文将不会解释一些基本命令,如有疑问请移步google.

下载:
```bash 
wget https://cfengine.com/source-code/download?file=cfengine-3.2.3.tar.gz
```
安装依赖:
```bash 
yum -y install db4-devel pcre-devel openssl-devel flex
```
编译安装:
```bash 
tar -zxvf download\?file\=cfengine-3.2.3.tar.gz
cd cfengine-3.2.3/
./configure --prefix=/usr/local/cfengine
make &amp;&amp; make install
```
为了保证cfengine正常工作创建cfengine工作目录:
```bash 
mkdir -p /var/cfengine/masterfiles  # 存放要分发的承诺
mkdir /var/cfengine/inputs          # 存放要执行的承诺
mkdir /var/cfengine/outputs         # 存放执行承诺的输出
mkdir /var/cfengine/bin             # 存放二进制文件
```
复制二进制文件:
```bash 
cp /usr/local/cfengine/sbin/cf-* /var/cfengine/bin/
ls -l /var/cfengine/bin/
total 964
-rwxr-xr-x 1 root root 235958 Jan 13  2012 cf-agent           # 执行承诺
-rwxr-xr-x 1 root root  63095 Jan 13  2012 cf-execd           # 用于替代cron的程序,定时执行cf-agent
-rwxr-xr-x 1 root root  39076 Jan 13  2012 cf-key             # 生成用于认证的证书
-rwxr-xr-x 1 root root  78711 Jan 13  2012 cf-know            # 从大量承诺（知识建模代理）生成一个 ISO 标准的 Topic Map 的命令
-rwxr-xr-x 1 root root 150846 Jan 13  2012 cf-monitord        # 负责收集有关系统状态信息的守护进程
-rwxr-xr-x 1 root root  16078 Jan 13  2012 cf-promises        # 检查承诺语法的程序
-rwxr-xr-x 1 root root 107349 Jan 13  2012 cf-report          # 生成报告
-rwxr-xr-x 1 root root  44160 Jan 13  2012 cf-runagent        # 用来执行远程的cf-agent
-rwxr-xr-x 1 root root 172251 Jan 13  2012 cf-serverd         # 分发承诺的守护进程
```
一个cfengine的hello, world:
```bash 
vi /var/cfengine/inputs/test.cf                                               # 新建一个承诺文件,添加下面内容
###################
# 这里顺便解释一下cfengine的语法,cfengine的语法大都如下
# &lt;它是什么&gt; &lt;它对什么起作用&gt; &lt;它叫什么&gt;
body common control                                   # 一个body 对common组启作用,名字是control(名字为control的common的组是最重要的一个组,cfengine以这个组为起点
{
bundlesequence =&gt; { "test" };                         # 定义要执行的承诺束为test
}

bundle agent test                                     # 一个承诺束,对cf-agent起作用,叫test
{
reports:                                              # 一个报告(仅仅显示消息,不对系统做改变)
  cfengine_3::                                        # 一个类,对cfengine 版本3的策略执行点起作用(cfengine用这个来替代程序语言里的if-else)
      "Hello world!";                                 # 承诺内容,显示"hello world!"
}

/var/cfengine/bin/cf-promises -f /var/cfengine/inputs/test.cf        # 检查承诺语法
/var/cfengine/bin/cf-agent -f /var/cfengine/inputs/test.cf           # 执行承诺
R: Hello world!                                                      # R:代表一个report.
```
为策略分发点创建配置文件:
```bash 
cp /usr/local/cfengine/share/cfengine/masterfiles/* /var/cfengine/masterfile
```
编辑策略文件:
新建一个cftest1.cf承诺文件
```bash 
vi /var/cfengine/masterfiles/cftest1.cf                  # 新建一个cftest1.cf
# 添加如下内容:
bundle agent test
{
reports:
  cfengine_3::
    "I'am cfengine 3 client";
}
```
然后编辑promises.cf文件加入执行这个承诺束的支持:
```bash 
vi /var/cfengine/masterfiles/promises.cf
body common control
{
 bundlesequence =&gt; { "main", "test" };                                # 将test加入的承诺束队列

 inputs =&gt; {
            "cfengine_stdlib.cf",
            "cftest1.cf",                                             # 将cftest1.cf引入进来
           };

 version =&gt; "Community Promises.cf 1.0.0";
}
.......
```
&nbsp;

做好之后先本地同步,然后启动server:
<pre>/var/cfengine/bin/cf-agent  --bootstrap --policy-server 172.16.1.1
```
到如下提示表示成功:
```bash 
-&gt; Bootstrap to 172.16.1.1 completed successfully
netstat -antlp | grep cf
tcp        0      0 :::5308                     :::*                        LISTEN      20173/cf-serverd
```
然后执行策略:
```bash 
/var/cfengine/bin/cf-agent
R: --&gt; CFE is running on cfhub
R: I'am cfengine 3 client
```
&nbsp;

cfengine默认是让本机地址的16位网络连接同步的,如果新加入一个另一网段的设备允许同步,比如允许192.168.1.网段同步,编辑/var/cfengine/masterfiles/promises.cf,找到bundle common def
```bash 
vi /var/cfengine/masterfiles/promises.cf
bundle common def
{
 vars:                                            # 定义变量
   ... ...
    "acl" slist =&gt; {
                "$(sys.policy_hub)/16", "192.168.1.",          # 这这个后面添加
  ... ...
}
```
服务端基本配置完成,客户端(cfengine host),按照本文前面安装的部分进行安装,创建工作目录,复制二进制文件,不用创建配置文件和承诺,然后执行:
```bash 
/var/cfengine/bin/cf-agent  --bootstrap --policy-server 172.16.1.1
```
会出现和服务器执行一样的提示,就表示成功,如果提示连接不成功,尝试关闭防火墙.同步后执行本地策略:
```bash 
/var/cfengine/bin/cf-agent
```
cfengine官方还是建议在生产环境加一个版本控制器用来创建承诺文件,然后给策略分发点用来分发给策略执行点.更多cfengine的语法请参见手册.
