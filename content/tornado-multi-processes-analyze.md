Title: Tornado 多进程实现分析
Date: 2014-04-11 14:35
Tags: Python, fork_processes, tornado, 多进程, web, 提升, 效率
Category: Python

## 引子
Tornado 是一个网络异步的的web开发框架, 并且可以利用多进程进行提高效率, 
下面是创建一个多进程 tornado 程序的例子.
```python
#!/usr/bin/env python
# -*- coding:utf-8 -*-
import os
import time

import tornado.web
import tornado.httpserver
import tornado.ioloop
import tornado.netutil
import tornado.process


class LongHandler(tornado.web.RequestHandler):

    def get(self):
        self.write(str(os.getpid()))
        time.sleep(10)


if __name__ == "__main__":
    app = tornado.web.Application(([r'/', LongHandler], ))
    sockets = tornado.netutil.bind_sockets(8090)
    tornado.process.fork_processes(2)
    server = tornado.httpserver.HTTPServer(app)
    server.add_sockets(sockets)
    tornado.ioloop.IOLoop.instance().start()
```
上面代码使用 `tornado.process.fork_processes` 创建了2个子进程, 同时用时访问这个
服务两次,  分别会返回两个相邻的pid. 可以看到 tornado 确实使用了两个进程来同时完成任务.

我一直很好奇 tornado 是如何将请求调度到子进程, 多个子进程又如何不同时处理一个请求呢?

## 探究

我们首先是调用 `tornado.netutil.bind_sockets` 来创建一个 socket(或一个 socket 列表),

接着我们调用 `tornado.process.fork_processes` 来 fork 子进程, 
阅读此函数的代码会发现这个函数仅仅是创建子进程, 然后主进程负责等待子进程, 如果子进
程退出则会根据条件重启子进程, 如果子进程全部退出并不符合重启条件,则主进程退出.

调用这个函数之后, 子进程中函数会返回, 子进程则继续执行调用这个函数之后的代码.

我们在 fork 子进程后做了如下操作.
```python
    server = tornado.httpserver.HTTPServer(app)
    server.add_sockets(sockets)
    tornado.ioloop.IOLoop.instance().start()
```
我们先看看 `tornado.httpserver.HTTPServer.add_sockets` 发现`HTTPServer`是继承的
`tornado.netutil.TCPServer`, add_sockets 也是实现在 `TCPServer` 中

`tornado.netutil.TCPServer.add_sockets`

```python
    def add_sockets(self, sockets):
        if self.io_loop is None:
            self.io_loop = IOLoop.instance()

        for sock in sockets:
            self._sockets[sock.fileno()] = sock
            add_accept_handler(sock, self._handle_connection,
                               io_loop=self.io_loop)


```
主要是映射了下 socket 和 socket 对应的文件描述符, 我们看看它调用的
`add_accept_handler`
```python
def add_accept_handler(sock, callback, io_loop=None):
    if io_loop is None:
        io_loop = IOLoop.instance()

    def accept_handler(fd, events):
        while True:
            try:
                connection, address = sock.accept()
            except socket.error as e:
                if e.args[0] in (errno.EWOULDBLOCK, errno.EAGAIN):
                    return
                raise
            callback(connection, address)
    io_loop.add_handler(sock.fileno(), accept_handler, IOLoop.READ)
```
我们知道 **I/O多路复用** 在处理服务端 socket 时, 当有连接请求过来时, 会触发
可读的事件, 此函数将 socket 在主事件循环中注册读事件(IOLoop.READ), 它的回调
会创建连接, 我注意到回调里的异常捕获有这样几行
```python
                if e.args[0] in (errno.EWOULDBLOCK, errno.EAGAIN):
                    return
                raise
```
发现在创建连接的时候会跳过这个异常呢, 为什么?那么 `EWOULDBLOCK` 和 `EAGAIN` 是是什么呢?
通过查找知道它的意思是在非阻塞模式下, 不需要重读或重写, `EAGAIN` 是 `EWOULDBLOCK` 在
Windows 上的名字, 所以看到这里就很明确了.

## 结论
Tornado 多进程的处理流程是先创建 socket, 然后再 fork 子进程, 这样所有的子进程实际都监听
一个(或多个)文件描述符, 也就是都在监听同样的 socket.

当连接过来所有的子进程都会收到可读事件, 这时候所有的子进程都会跳到 `accept_handler`
回调函数, 尝试建立连接.

一旦其中一个子进程成功的建立了连接, 当其他子进程再尝试建立这个连接的时候就会触发
`EWOULDBLOCK`(或 EAGAIN) 错误. 这时候回调函数判断是这个错误则返回函数不做处理.

当成功建立连接的子进程还在处理这个连接的时候又过来一个连接, 这时候就会有另外一个
子进程接手这个连接.

Tornado 就是通过这样一种机制, 利用多进程提升效率, 由于连接只能由一个子进程成功创建,
同一个请求也就不会被多个子进程处理.


## 后记
写完才发现, 我所使用的代码是 tornado-2.4.post2 版本, 当前最新代码是 3.3.0, 
查看了下最新代码, 最新代码`TCPServer` 写到单独 `tornado.tcpserver` 里了, 其他和本文
相关的并没有什么大的变化.
