title: 基于 Python 生成器的 Tornado 协程异步 
tags: Python, generator, coroutine, 协程, 生成器, Tornado
category: Python
date: 2014-12-19 17:15


[Tornado 4.0](http://www.tornadoweb.org/en/branch4.0/releases/v4.0.0.html) 已经发布了很长一段时间了,
新版本广泛的应用了协程(``Future``)特性.  我们目前已经将 Tornado 升级到最新版本, 而且也大量的使用协程特性. 

很长时间没有更新博客, 今天就简单介绍下 Tornado 协程实现原理, Tornado 的协程是基于 Python 的生成器实现的, 
所以首先来回顾下生成器.

## 生成器
Python 的生成器可以保存执行状态 并在下次调用的时候恢复, 通过在函数体内使用 ``yield`` 关键字
来创建一个生成器, 通过内置函数 ``next`` 或生成器的 ``next`` 方法来恢复生成器的状态. 

```python
def test():
    yield 1
```

我们调用 ``test`` 函数, 此时并不会返回结果, 而是会返回一个生成器
```python
>>> test()
<generator object test at 0x100b3b320>
```

我们调用其 ``next`` 方法则返回 ``yield`` 关键字之后的内容.
```python
>>> t = test()
>>> t.next()
1
```

如果我们接着调用  ``next`` 方法, 后面又没有 ``yield`` 关键字继续返回的话, 会抛出一个
``StopIteration`` 异常.

``yield`` 关键字不仅仅能从生成器内部返回状态, 同时也可以将外部信息传递到生成器内部,
通过将 ``yeild`` 关键里赋值给变量, 并调用生成器的 ``send`` 方法来将对象传递到生成器
内部. 需要注意的是生成器的开始必须调用其 ``next`` 方法, 后面 ``send`` 方法调用的同时
也会触发 ``next`` 动作. 如果没有变量接收 ``yield`` 关键字那么 ``send`` 传递的值将会
被丢弃.

```python
>>> def test():
    a = yield
    print(a)
```
首先调用 ``next`` 上面函数返回的生成器将返回 ``None``, 如果这时候直接调用 ``next`` 将
会给生成器发送 ``None``, 如果调用 ``send`` 发送一个值, 将打印这个值并抛出 ``StopIteration``
异常.

## 一个简单地协程
以上就是实现协程的所有基础, 为了加深理解, 我们这里写一个小例子, 例子我们只使用协程
开启两个甚至多个死循环, 下面就是一个极其简单地例子::
```python
#!/usr/bin/env python
# -*- coding:utf-8 -*-

from __future__ import absolute_import, print_function, division, with_statement


def loop1():
    """ 循环1负责抛出一个函数和对应的参数, 并接收结果
    """
    a = 0
    ret = 1
    while True:
        ret = yield sum, [a, ret]
        a, ret = ret, a
        print("Loop1 ret", ret)


def loop2():
    """ 循环2 负责接收函数并计算结果, 然后 yield 出结果
    """
    while True:
        func, args = yield
        yield func(args)
        print("Loop2")


l1 = loop1()
l2 = loop2()
tmp = l1.next()

for i in range(10):
    l2.next()
    ret = l2.send(tmp)
    tmp = l1.send(ret)
```
上面例子里 loop1 负责产生任务, loop2 负责执行任务, 主循环负责调度任务并将任务结果发回给
任务产生者.


## Tornado 如何做的
我们首先看一个使用 Tornado 协程异步的例子
```python
#!/usr/bin/env python
# -*- coding:utf-8 -*-

from __future__ import absolute_import, print_function, division, with_statement

from tornado import gen
from tornado import web
from tornado import httpclient


class ActionHandler(web.RequestHandler):

    @gen.coroutine
    def get(self):
        response = yield httpclient.AsyncHTTPClient().fetch("http://www.linuxzen.com")

        # ...

```
其实原理在上面简单地例子里已经讲清楚了, 我们来简单分析一遍上面的例子, 首先 Tornado 得到
`ActionHandler.get` 方法抛出(`next`)的一个任务, 然后异步的去执行任务, 当任务(网络请求)结束或
异常时 Tornado 取得事件通知然后将结果放回(`send`)到该方法中让该方法继续执行.

由于是异步的, 调用这个方法并不会阻塞其他任务执行.

这时候我们的方法其实就是上个例子 `loop1` 函数, 而 `Tornado` 调度并执行了其抛出的任务.

## 总结
Tornado 的协程异步可以让异步看起来是顺序执行的, 可以从一大串的 `callback` 中解脱出来.

`Tornado` 的协程异步并不是这三言两语能说清楚的, 其中有很复杂的封装和传递, 有兴趣可以自己
阅读源码.
