title: logging 模块误用导致的内存泄露
tags: Python, logging, 内存泄露
category: Python
date: 2015-01-31 10:58

首先介绍下怎么发现的吧, 线上的项目日志是通过 `logging` 模块打到 syslog 里, 
跑了一段时间后发现 syslog 的 UDP 连接超过了 8W, 没错是 8 W. 主要是 logging 
模块用的不对

我们之前有这么一个需求, 就是针对每一个连接日志输出当前连接的信息, 所以每一个
连接就创建了一个日志实例, 并分配一个 `Formatter`, 创建日志实例为了区分其他连接
所以我就简单粗暴的用了当前对象的 id 来作为日志名称:

```python
import logging


class Connection(object):
    def __init__(self):
        self._logger_name = "Connection.{}".format(id(self))
        self.logger = logging.getLogger(self._logger_name)
```

当然测试环境是开 DEBUG, 开 DEBUG 就不会往 syslog 里打, 所以不会出现 UDP 连接数
过多, 也就不会知道有内存泄露的, 我们来看看这样为什么会导致内存泄露, 首先看看 
`getLogger` 的代码:
```python
def getLogger(name=None):
    """
    Return a logger with the specified name, creating it if necessary.

    If no name is specified, return the root logger.
    """
    if name:
        return Logger.manager.getLogger(name)
    else:
        return root
```


主要调用了 `Logger.manager.getLogger`, 这个函数有下面一段代码片段

```python
            if name in self.loggerDict:
                rv = self.loggerDict[name]
                if isinstance(rv, PlaceHolder):
                    ph = rv
                    rv = (self.loggerClass or _loggerClass)(name)
                    rv.manager = self
                    self.loggerDict[name] = rv
                    self._fixupChildren(ph, rv)
                    self._fixupParents(rv)
            else:
                rv = (self.loggerClass or _loggerClass)(name)
                rv.manager = self
                self.loggerDict[name] = rv
                self._fixupParents(rv)
```

logging 模块为了保证同一个名称引用同一个日志实例,所以就把所有的日志实例全部存
在了一个 `loggerDict` 的字典里, 所以除非程序退出, 创建的日志实例引用是不会释放的,
所以日志实例里的 `handlers` 也不会释放. 之前我又用的对象的 id 来作为日志名称
的一部分, 所以 `SyslogHandler` 创建的 UDP 连接就一直被占用导致了过多的 UDP 连接. 

为了解决这个问题我在连接关闭的时候加入了如下代码:
```python
logging.Logger.manager.loggerDict.pop(self._logger_name)
self.logger.manager = None
self.logger.handlers = []
```

按说只加上上面第一行的代码就应该释放了, 但是没有, 所以又有了第三行代码, SyslogHandler
才最终释放, 这个问题暂时还不知道为什么, 还需要再查查.


**2015-03-30 更新**
如果日志名称是以 `.` 分隔, logging 模块则会将最后一部分作为日志名, 并往上去寻找 `父 Logger`,
如果找不到则创建 `PlaceHolder` 对象作为父, 并引用 `Logger`.

比如创建的 Logger 名称为 `a.b.c`, 那么实际的名称则为 `c`, 并将 `b` 作为 `c` 的父, `a` 作为 `b` 的
父, 如果没有该名称的 `Logger` 则创建 `PlaceHolder` 对象作为代替, `PlaceHolder` 会创建对当前 `Logger`
的引用. 所以需要被回收的日志对象名称里不应包含 `.`
