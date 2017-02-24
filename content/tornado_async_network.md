Title: 使用Tornado进行网络异步编程
Date: 2013-04-15 16:09
Category: Python
Tags: Python, tornado, 网络, 异步, 编程

## Tornado
`Tornado` 是一款非阻塞可扩展的使用Python编写的web服务器和Python Web框架, 可以使用`Tornado`编写Web程序并不依赖任何web服务器直接提供高效的web服务.所以`Tornado`不仅仅是一个web框架而且还是一款可以用于生产环境的高效的web服务器

Torando 在Linux和FreeBSD上使用高效的异步I/O模型 `epoll` 和`kqueue`来实现高效的web服务器, 所以 tornado在Linux上和FreeBSD系列性能可以达到最高

## 接口
当然我们可以不仅仅把`Tornado`看作是一个web框架和web服务器, 我们可以利用`Tornado`提供的接口进行高效的网络异步编程,

`tornado.ioloop.IOLoop` 提供了三个接口可以用于网络编程:

### add_handler
```python
def add_handler(self, fd, handler, events):
    self._handlers[fd] = stack_context.wrap(handler)
    self._impl.register(fd, events | self.ERROR)
```

`add_handler`用于添加socket到主循环中, 接受三个参数:
* fd 是socket的文件描述符
* handler 是处理此socket的 callback函数
* events 是此socket注册的事件

### update_handler
```python
def update_handler(self, fd, events):
    self._impl.modify(fd, events | self.ERROR)
```
`update_handler`用于更新住循环中已存在的socket响应事件, 接受两个参数:
* fd 是socket对应的文件描述符
* events 是注册的新事件

### remove_handler
```python
def remove_handler(self, fd):
    self._handlers.pop(fd, None)
    self._events.pop(fd, None)
    try:
        self._impl.unregister(fd)
    except Exception:
        gen_log.debug("Error deleting fd from IOLoop", exc_info=True)
```
`remove_handler`用于移除主循环中已存在的socket

## 事件
`tornado.ioloop.IOLoop`同时提供了4种响应事件:

事件                       |       描述
-------------------------- | --------------
tornado.ioloop.IOLoop.NONE |    无事件
tornado.ioloop.IOLoop.READ |    读事件
tornado.ioloop.IOLoop.WRITE |    写事件
tornado.ioloop.IOLoop.ERROR |   发生错误的事件

## 实例
根据上面的接口和事件我们就可以写出一个简单的 echo server
```python
#!/usr/bin/env python
# -*- coding:utf-8 -*-
#
#   Author  :   cold
#   E-mail  :   wh_linux@126.com
#   Date    :   13/04/15 15:08:51
#   Desc    :   Tornado Echo Server
#   HOME    :   http://www.linuxzen.com
#
import Queue
import socket

from functools import partial

from tornado.ioloop import IOLoop

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.setblocking(0)              # 将socket设置为非阻塞

server_address = ("localhost", 10000)

sock.bind(server_address)
sock.listen(5)

fd_map = {}              # 文件描述符到socket的映射
message_queue_map = {}   # socket到消息队列的映射

fd = sock.fileno()
fd_map[fd] = sock

ioloop = IOLoop.instance()

def handle_client(cli_addr, fd, event):
    s = fd_map[fd]
    if event & IOLoop.READ:
        data = s.recv(1024)
        if data:
            print "     received '%s' from %s" % (data, cli_addr)
            # 接收到消息更改事件为写, 用于发送数据到对端
            ioloop.update_handler(fd, IOLoop.WRITE)
            message_queue_map[s].put(data)
        else:
            print "     closing %s" % cli_addr
            ioloop.remove_handler(fd)
            s.close()
            del message_queue_map[s]

    if event & IOLoop.WRITE:
        try:
            next_msg = message_queue_map[s].get_nowait()
        except Queue.Empty:
            print "%s queue empty" % cli_addr
            ioloop.update_handler(fd, IOLoop.READ)
        else:
            print 'sending "%s" to %s' % (next_msg, cli_addr)
            s.send(next_msg)

    if event & IOLoop.ERROR:
        print " exception on %s" % cli_addr
        ioloop.remove_handler(fd)
        s.close()
        del message_queue_map[s]


def handle_server(fd, event):
    s = fd_map[fd]
    if event & IOLoop.READ:
        conn, cli_addr = s.accept()
        print "     connection %s" % cli_addr[0]
        conn.setblocking(0)
        conn_fd = conn.fileno()
        fd_map[conn_fd] = conn
        handle = partial(handle_client, cli_addr[0])   # 将cli_addr作为第一个参数
        # 将连接和handle注册为读事件加入到 tornado ioloop
        ioloop.add_handler(conn_fd, handle, IOLoop.READ)
        message_queue_map[conn] = Queue.Queue()   # 创建对应的消息队列


ioloop.add_handler(fd, handle_server, IOLoop.READ)

ioloop.start()
```

上面代码就建立了一个非阻塞的高效的异步的echo server
