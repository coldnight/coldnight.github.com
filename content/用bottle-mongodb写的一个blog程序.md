Title: 用bottle+mongodb写的一个blog程序
Tags: 项目,实例,web,python,mongodb,bottle
Category: Python
Date: 2012-06-16 16:20
我个人觉得更好更快的学习和掌握某个东西最好的方法就是使用它,多使用它.然后在一次次的解决问题中来快速掌握和了解它.你觉得呢?前段时间接触了bottle这个轻量web框架,和nosql数据库mongodb,为了掌握和了解这她们,我自己做了一个blog程序,参照了vimer.cn里的设计的物理设计.是用bottle作为web开发框架,mongodb作为后台数据库.主要实现功能:

前台显示文章:
按分类显示
按标签显示
按月份归档显示
最新文章
评论
后台管理:
管理文章
管理分类
管理评论
发表文章
用户验证

目录结构


water

+-app           程序目录

|----admin.py  后台管理

|----blog.py   前台显示

|----dbconn.py 数据库连接

|----encrypt.py包含加密函数

+-static       静态文件目录:包括js css image

+-views        模版目录

+-index.py     用于启动整个程序

+-initiate.py  初始化脚本,用于创建一个管理用户

+-setting.py   设置文件,各种设置

+-static.py    用于程序处理静态文件

项目代码放在了google code上,可以访问下面链接浏览:
http://code.google.com/p/sharepythoncode/source/browse/water/
