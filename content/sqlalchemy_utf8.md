title: SQLAlchemy MySQL数据库乱码解决
date: 2013-04-26
category: Python
tags: sqlalchemy, MySQL, 数据库, 乱码, utf8


今天对[clubot](/python-shi-yong-pyxmpp2bian-xie-gtalkqun.html)进行了[升级](/clubotgeng-xin-shi-yong-sqlalchemyzhong-xie-shu-ju-ku-bu-fen-he-gai-yong-tornado-mainloop.html), 但是导入数据后中文乱码, 一开是找资料说是在创建引擎的时候添加编码信息:
```python
engine = create_engine("mysql://root:@localhost:3306/clubot?charset=utf8")
```
但是这并不行, 然后查看表信息:
```mysql
> show create table clubot_members;
clubot_members | CREATE TABLE `clubot_members` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `email` varchar(100) DEFAULT NULL,
  `nick` varchar(50) DEFAULT NULL,
  `last_say` timestamp NULL DEFAULT NULL,
  `last_change` timestamp NULL DEFAULT NULL,
  `isonline` int(11) DEFAULT NULL,
  `join_date` timestamp NULL DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `email` (`email`),
  UNIQUE KEY `nick` (`nick`)
) ENGINE=InnoDB AUTO_INCREMENT=20 DEFAULT CHARSET=latin1;
```
发现原来创建表的时候用的`latin1`编码, 而老的表是用`utf-8`编码创建的, `SQLAlchemy`中并没有发现有创建表时指定指定编码的方法. 所以只能在`MySQL`本身来找:
```mysql
> show VARIABLES like "character%%";
+--------------------------+-----------------------------+
| Variable_name            | Value                       |
+--------------------------+-----------------------------+
| character_set_client     | utf8                        |
| character_set_connection | utf8                        |
| character_set_database   | latin1                      |
| character_set_filesystem | binary                      |
| character_set_results    | utf8                        |
| character_set_server     | latin1                      |
| character_set_system     | utf8                        |
| character_sets_dir       | /data/share/mysql/charsets/ |
+--------------------------+-----------------------------+
8 rows in set (0.00 sec)

> show create database clubot;
+----------+-------------------------------------------------------------------+
| Database | Create Database                                                   |
+----------+-------------------------------------------------------------------+
| clubot   | CREATE DATABASE `clubot` /*!40100 DEFAULT CHARACTER SET latin1 */ |
+----------+-------------------------------------------------------------------+
1 row in set (0.00 sec)

```
发现 `MySQL`默认的和数据库都是`latin1`的编码, 所以更改数据库配置
```bash
vi /etc/mysql/my.cnf      # MySQL配置文件在Ubuntu上的位置, 其他系统可能有差异
```
分别在`[client]` `[mysqld]`下添加
```conf
default-character-set = utf8
```
这时重启MySQL居然起不来, 说`default-character-set`是无效的变量, 查看`MySQL`版本发现是5.5, 找资料说5.5的服务端编码设置变量是`character-set-server`, 所以将`[mysqld]`上的`default-character-set = utf8`改为 `character-set-server = utf8`, 并重启`MySQL`

然后更改数据库编码:
```mysql
alter database clubot character set utf8;
```

删除新建的表, 并重新导入数据中文就正常了
```mysql
> use clubot;
> drop table clubot_status;
> drop table clubot_infos;
> drop table clubot_history;
> drop table clubot_members;
```
