Title: 使用更加高效的epoll作为pyxmpp2的主循环
Tags: epoll, python, gtalk, 群, Linux, clubot
Category: Python
Date: 2013-02-05 16:06
## 引子
之前[clubot](/python-shi-yong-pyxmpp2bian-xie-gtalkqun.html)使用的pyxmpp2的默认mainloop也就是一个poll的主循环,但是`clubot`上线后资源占用非常厉害,使用`strace`跟踪发现`clubot`在不停的`poll`,查看`pyxmpp2`代码发现`pyxmpp2`的`poll`在使用超时阻塞时使用`最小`超时时间,而`最小`超时时间一直是0,所以会变成一个没有超时的非阻塞`poll`很浪费资源,不打算更改库代码,所以自己仿照poll的mainloop写了一个更加高效的`epoll`的mainloop

## 实现
```python
#!/usr/bin/env python
# -*- coding:utf-8 -*-
#
#   Author  :   cold
#   E-mail  :   wh_linux@126.com
#   Date    :   13/01/06 10:41:31
#   Desc    :   Clubot epoll mainloop
#
from __future__ import absolute_import, division

import select

from pyxmpp2.mainloop.interfaces import HandlerReady, PrepareAgain
from pyxmpp2.mainloop.base import MainLoopBase

from plugin.util import get_logger



class EpollMainLoop(MainLoopBase):
    """ Main event loop based on the epoll() syscall on Linux system """
    READ_ONLY = (select.EPOLLIN | select.EPOLLPRI | select.EPOLLHUP |
                 select.EPOLLERR |select.EPOLLET)
    READ_WRITE = READ_ONLY | select.EPOLLOUT
    def __init__(self, settings = None, handlers= None):
        self.epoll = select.epoll()
        self._handlers = {}
        self._unprepared_handlers = {}
        self._timeout = None
        self._exists_fd = {}
        self.logger = get_logger()
        MainLoopBase.__init__(self, settings, handlers)

        return

    def _add_io_handler(self, handler):
        self._unprepared_handlers[handler] = None
        self._configure_io_handler(handler)

    def _configure_io_handler(self, handler):
        if self.check_events():
            return
        if handler in self._unprepared_handlers:
            old_fileno = self._unprepared_handlers[handler]
            prepared = self._prepare_io_handler(handler)
        else:
            old_fileno = None
            prepared = True
        fileno = handler.fileno()
        if old_fileno is not None and fileno != old_fileno:
            del self._handlers[old_fileno]
            self._exists.pop(old_fileno, None)
            self.epoll.unregister(old_fileno)
        if not prepared:
            self._unprepared_handlers[handler] = fileno

        if not fileno:
            return

        self._handlers[fileno] = handler
        events = 0
        if handler.is_readable():
            events |= self.READ_ONLY
        if handler.is_writable():
            events |= self.READ_WRITE

        if events:
            if fileno in self._exists_fd:
                self.epoll.modify(fileno, events)
            else:
                self._exists_fd.update({fileno:1})
                self.epoll.register(fileno, events)

    def _prepare_io_handler(self, handler):
        ret = handler.prepare()
        if isinstance(ret, HandlerReady):
            del self._unprepared_handlers[handler]
            prepared = True
        elif isinstance(ret, PrepareAgain):
            if ret.timeout is not None:
                if self._timeout is not None:
                    self._timeout = min(self._timeout, ret.timeout)
                else:
                    self._timeout = ret.timeout
            prepared = False
        else:
            raise TypeError("Unexpected result from prepare()")

        return prepared

    def _remove_io_handler(self, handler):
        if handler in self._unprepared_handlers:
            old_fileno = self._unprepared_handlers[handler]
            del self._unprepared_handlers[handler]
        else:
            old_fileno = handler.fileno()
        if old_fileno is not None:
            try:
                del self._handlers[old_fileno]
                self._exists.pop(old_fileno, None)
                self.epoll.unregister(old_fileno)
            except KeyError:
                pass

    def loop_iteration(self, timeout = 60):
        next_timeout, sources_handled = self._call_timeout_handlers()
        if self.check_events():
            return
        if self._quit:
            return sources_handled
        for handler in list(self._unprepared_handlers):
            self._configure_io_handler(handler)
        if self._timeout is not None:
            timeout = min(timeout, self._timeout)
        if next_timeout is not None:
            timeout = min(next_timeout, timeout)

        if timeout == 0:
            timeout += 1    # 带有超时的非阻塞,解约资源
        events = self.epoll.poll(timeout)
        for fd, flag in events:
            if flag & (select.EPOLLIN | select.EPOLLPRI | select.EPOLLET):
                self._handlers[fd].handle_read()
            if flag & (select.EPOLLOUT|select.EPOLLET):
                self._handlers[fd].handle_write()
            if flag & (select.EPOLLERR | select.EPOLLET):
                self._handlers[fd].handle_err()
            if flag & (select.EPOLLHUP | select.EPOLLET):
                self._handlers[fd].handle_hup()
            #if flag & select.EPOLLNVAL:
                #self._handlers[fd].handle_nval()

            sources_handled += 1
            self._configure_io_handler(self._handlers[fd])

        return sources_handled
```

## 使用
如何使用新的mainloop?只需在实例化Client时传入
```python
mainloop = EpollMainLoop(settings)
client = Client(my_jid, [self, version_provider], settings, mainloop)
```
这样就会使用epoll作为mainloop

## 注意
epoll仅仅在`Linux`下支持

## 写在后面的话
欢迎大家加入到`clubot@vim-cn.com`中来讨论有关Python/vim/Linux/开源等话题
