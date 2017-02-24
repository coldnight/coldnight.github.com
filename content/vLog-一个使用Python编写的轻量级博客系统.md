Title: vLog 一个使用Python编写的轻量级博客系统
Tags: vLog, tornado, blog, 博客, 轻量, markdown
Category: Python
Date: 2013-02-05 16:43
## 介绍
### 何为vLog
大家有人可能注意到博客改变了,是的前面也有文章提到从wordpress迁移到vlog,但是何为vLog这里给大家简要的说明一下,vLog是我使用`Python`的`tornado`框架和`Jinja2`模板引擎,基于MySQL数据的一个轻量级的博客系统,此系统功能比较薄弱,处于开发初期,使用`Markdown`的格式来抒写博文.

### 为什么vLog
vLog后台十分简单(可以说是简陋),功能也简单,就是一款简单的博客系统,提供了Python终端脚本,可以在终端来抒写博文, vLog使用一套非常简单的缓存系统,缓存使用memcached使得页面加载速度非常快.

### 为什么不vLog
相对与wordpress vlog非常简陋,仅仅提供简单的博客功能,而且使用Python编写主机方面支援不太多,虽然有`SAE`和`GAE`的支援,但是我没弄过,所以没有支援`SAE`和`GAE`(如果你有兴趣,可以添加相关支持)


## 安装
### 平台
* Linux
* python2.7
* MySQL
* Memcached 1.4.5


### 依赖库
* tornado
* jinja2
* MySQLdb
* pylibmc

### 开始安装
首先确认config.py的DEBUG是打开的,然后执行run.py,打开浏览器输入当前地址,会跳转到安装页面.按照提示安装,安装完毕后可以关闭DEBUG


## 从Wordpress中导入
### 从Wordpress导出
在wordpress管理后台选择工具->导出,下载导出文件可以导出一份xml

### 移动媒体文件
将/path/to/your/wordpress/wp-content/uploads/下的所有文件移动到/path/to/your/vlog/web/static/upload 下即可

务必要先执行这一步然后再在后台里导入xml

### 导入到vLog
进入vLog后台,选择导入,浏览选中导出的xml, 然后选择开始,等待提示成功后即导入成功


### 手动更改没有生效的链接
虽然我已经竭尽所能的让你手头的工作更少,但是还不够,还是存在许多需要手动更改的地方,
比如每篇文章的没有替换掉的图片链接

### 不足
网站迁移后我已经尽力的来保持原来的链接有效,但是我仅仅知道我原来的wordpress的链接,所以仅仅兼容了我原来使用wordpress的旧链接,如果没能兼容您的wordpress的链接在此表示歉意,您可以自己添加提交给我,或者将您的链接提交给我由我来给您添加

## 结合nginx
参阅[tornado文档](http://www.tornadoweb.cn/documentation#_14)

## 源代码
代码放在github上: [vLog](https://github.com/coldnight/vlog)
