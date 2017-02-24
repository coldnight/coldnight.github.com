title: clubot更新: 使用SQLAlchemy重写数据库部分和改用Tornado MainLoop
date: 2013-04-26 15:40
category: Python
tags: clubot, pyxmpp2, gtalk, xmpp, 更新, 重写, SQLAlchemy, tornado

[clubot](/python-shi-yong-pyxmpp2bian-xie-gtalkqun.html)在我的vps上跑了有一段时间了, 最近接触了`SQLAlchemy` 然后反观`clubot`的数据库代码部分, 感觉代码又遭有乱实在看不过眼, 所以就使用`SQLAlchemy`重写了数据库模块, 并将`epoll`的MainLoop改成[仙子君](http://lilydjwg.is-programmer.com/)所写的[TornadoMainLoop](https://github.com/lilydjwg/pyxmpp2)

## 更新内容
1. 数据库使用`SQLAlchemy`重写
2. `MainLoop`改用`TornadoMainLoop`
3. 改变代码结构, 清理部分代码
4. 将`history`命令改为`old`, 并支持时间查询
5. 废弃一些不常用的命令
6. 改变数据库表结构
7. 废弃`channel`功能, `cd`命令仅支持切换聊天和安静模式
8. 删除一些不用的配置

## 如何升级
数据库表结构做了更改, 所以为了兼容之前的数据库本次表名前加上`clubot_`前缀, 并配以`update.py`脚本用以支持将旧的数据导入.

## 新的依赖
本次更新添加了依赖, 现在依赖包括:

* pyxmpp2
  * dnspython
* tornado
* sqlalchemy
  * MySQL-python


## 乱码
如果数据导入后乱码, 可以参考[这篇文章](/sqlalchemy-mysqlshu-ju-ku-luan-ma-jie-jue.html)

## 项目地址
最后放上[项目地址](https://github.com/coldnight/clubot)
