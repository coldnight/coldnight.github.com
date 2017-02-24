title: 借用Tornado实现高效的WebQQ机器人
date: 2013-04-23 13:36
category: Python
tags: tornado, 高效, webqq, 并发, 协议

之前有写过一篇文章介绍使用`Pyxmpp2`桥接QQ和xmpp的文章([这里](/shi-yong-webqqxie-yi-qiao-jie-xmpphe-qqqun.html)).后来我打算将WebQQ单独出来运行, 一开始直接拷贝了`pyxmpp2`的mainloop, 但是跑起来问题多多, 所以我又研究了利用`Tornado`进行网络编程([这里](/shi-yong-tornadojin-xing-wang-luo-yi-bu-bian-cheng.html)), 所以我放弃了`Pyxmpp2`的mainloop,使用`Tornado`进行重写

首先放出[项目代码](https://github.com/coldnight/pual_bot)

## 引子
WebQQ协议是一套基于`HTTP`的QQ协议, 而用`Python`的`urllib2`库进行请求太慢, 因为HTTP本身就使用socket请求, 所以改用多路复用I/O模型, 而`Tornado`简单高效, 看过代码后可以轻松上手.平台兼容性很好, 所以选择`Tornado`作为网络框架.

## 原理
首先实现了一个 `HTTPStream`类, 其主要接口是`add_request`方法, 它接受一个必选参数:`request` 是一个 `urllib2.Request`的实例, 和一个可选参数:`readback`是一个接受一个`urllib2.urlopen(request)`返回的`Response`参数的读取函数, 代码如下:
```python
class HTTPStream(object):
    # 省略若干代码
    def add_request(self, request, readback = None):
        if not isinstance(request, urllib2.Request):
            raise ValueError, "Not a invaid requset"

        # 此处易触发timeout异常, 省略处理异常代码
        sock, data = self.http_sock.make_http_sock_data(request)

        fd = sock.fileno()
        self.fd_map[fd] = sock
        self.fd_request_map[fd] = request
        callback = partial(self._handle_events, request, data, readback)
        self.ioloop.add_handler(fd, callback, IOLoop.WRITE)
```

`HTTPStream.add_request`将`urllib2.Request`的实例解析出一个`socket`和一个用于`socket`发送的数据.前面文章介绍过了, `tornado.ioloop.IOLoop.add_handler`用于将注册socket, 其需要三个参数: socket的文件描述符, 接受文件描述符和事件参数的回调, 和注册的事件.

我们用到的回调是`HTTPStream._handle_events`:
```python
class HTTPStream(object):
    # 省略若干代码
    def _handle_events(self, request, data, readback, fd, event):
        """ 用于处理Tornado事件
        Arguments:
            `request`   -   urllib.Request
            `data`      -   socket要写入的数据
            `readback`  -   读取函数
            以上参数应当使用partial封装然后将此方法作为IOLoop.add_handler的callback
            `fd`        -   IOLoop传递 文件描述符
            `event`     -   IOLoop传递 tornado
        """
        s = self.fd_map[fd]

        if event & IOLoop.READ:
            # 省略错误处理
            resp = self.http_sock.make_response(s, request)
            args = readback(resp)
            s.setblocking(False)
            if args and len(args) == 3:
                t = threading.Thread(target = self.add_delay_request, args = args)
                t.setDaemon(True)
                t.start()

            if args and len(args) == 2:
                self.add_request(*args)
            self.ioloop.remove_handler(fd)

        if event & IOLoop.WRITE:
            s.sendall(data)
            if readback:
                self.ioloop.update_handler(fd, IOLoop.READ)
            else:
                self.ioloop.remove_handler(fd)

        if event & IOLoop.ERROR:
            pass
```
它接受的参数上面注释写的很清楚, 不做解释, 所以将此方法通过`functools.partial`封装做为`callback`传递给`tornado.ioloop.IOLoop.add_handler`, 并注册为`写`事件, 以便发送`HTTP`请求.

`HTTPStream._handle_events`用于处理事件, 当事件为写时就发送`HTTP`请求(根据`urllib2.Request`生成的用于发送的数据), 并判断是否有读取函数, 有则注册`读`事件, 当事件为读时就从socket中构建一个`Response`并传递给读取函数, 读取函数会返回3个值, 分别为: 下一个请求, 请求的读取函数(可为None, 为None则只请求不读取), 下一个请求的延迟(多长事件后添加此请求, 可选, 单位为秒)

依据读取函数返回的三个值来确定下一个请求, 并完成一系列的请求. 更加完整的代码请参见文章开头给出的项目代码

`HTTPStream.http_sock.make_response`执行时会将`socket`设为阻塞, 因为不设置阻塞会出现`httplib.BadStatusLine`异常.读取函数执行完毕,重新将`socket`设置为非阻塞, 并移除此`socket`(虽然做了这样的处理但是QQ连接时间稍长还是会触发`httplib.BadStatusLine`异常)

## 2013-04-26 更新
* 解决 在线时间稍长, 当经过多次请求后会触发`socket.gaierror(-2, 'Name or service not known')` 异常

## 存在问题
1. 没有重试机制
