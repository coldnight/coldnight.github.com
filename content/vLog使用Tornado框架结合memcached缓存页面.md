Title: vLog使用Tornado框架结合memcached缓存页面
Tags: Linux,python,Memcached,pylibmc,缓存,vLog
Category: Python
Date: 2013-01-16 16:14
## 原因
Blog是一个更新并不很频繁的一套系统,但是每次刷新页面都要更新数据库反而很浪费资源,添加静态页面生成是一个解决办法,同时缓存是一个更好的主意,可以结合Memcached添加少量的代码进行缓存,而且免去去了每次更新文章都要重新生成静态页面,特别当页面特别多时.

## 实现
主要通过页面的uri进行缓存,结合tornado.web.RequestHandler的prepare和on_finish方法函数,
prepare 主要是请求前执行,on_finish()是请求结束之前执行.在渲染模板时缓存页面内容,然后在请求前检测是否有缓存,如果有直接输出缓存,结束请求,在POST提交之后清空所有缓存,重新生成缓存,从而保证内容实时性.由于登录用户和普通用户的页面不相同,所以不缓存登录用户页面(代码中没有体现,请自行实现).主要python代码(省略了模板渲染的代码):
```python
#!/usr/bin/env python
# -*- coding:utf-8 -*-
#
#   Author  :   cold
#   E-mail  :   wh_linux@126.com
#   Date    :   13/01/14 09:57:31
#   Desc    :   
#
import config
import pylibmc
from tornado.web import RequestHandler
#### 省略Cache类定义 #####

class Memcached(object):
    _mc = pylibmc.client.Client(config.CACHE_HOST, binary = True)

    def __enter__(self):
        if config.CACHED:
            return Memcached
        else:
            return Cache()

    def __exit__(self, exc_type, exc_val, exc_tb):
        pass

    @classmethod
    def get_cache(cls):
        return cls._mc

    @classmethod
    def get(cls, key, default = None):
        r = cls._mc.get(key)
        if not r:
            r = default
        return r

    @classmethod
    def set(cls, key, value, timeout = 0):
        timeout = timeout if timeout else config.CACHE_TIMEOUT
        return cls._mc.set(key, value, timeout)

    @classmethod
    def delete(cls, key):
        return cls._mc.delete(key)

    @classmethod
    def flush(cls):
        return cls._mc.flush_all()

    def __getattr__(self, key):
        return Memcached.get(key)

    def __setattr__(self, key, value):
        return Memcached.set(key, value)


class BaseHandler(RequestHandler):
    """ 继承tornado请求基类,重写 prepare和on_finish方法 """
    cache = Memcached

    def render(self, template_path, *args, **kwargs):
        """ 渲染模板 """
        # 省略渲染模板代码
        content = ''     # 渲染模板后的内容
        if self.request.method == "GET" and CACHED and \
           not self.request.path.startswith("/admin"):
            self.cache.set(self.request.uri, content) # 将渲染后的内容缓存起来
        self.write(content)

    def prepare(self):
        super(BaseHandler, self).prepare()
        # 如果请求是GET方法,而且不是请求后台
        if self.request.method == "GET" and CACHED and \
           not self.request.path.startswith("/admin"):

            # 尝试获取当前页面的缓存
            cache = self.cache.get(self.request.uri)
            # 获取缓存则输出页面,结束请求
            if cache:
                return self.finish(cache)

    def on_finish(self):
        """ 重写结束请求前的方法函数 """
        if self.request.method == "POST":
            # 如果遇到POST提交则清空缓存
            self.cache.flush()
```

缓存系统在`redis`和`Memcached`选择了很久,因为只是单纯的缓存页面所以最后选择了`memcached`,使用`pylibmc` python库.
## 测试
使用webbench 网站压力测试对比了缓存前后的结果:
使用缓存前
```bash
$ webbench -c 500 -t 30 http://www.linuxzen.com/
Webbench - Simple Web Benchmark 1.5
Copyright (c) Radim Kolar 1997-2004, GPL Open Source Software.

Benchmarking: GET http://www.linuxzen.com/
500 clients, running 30 sec.

Speed=54 pages/min, 38160 bytes/sec.
Requests: 27 susceed, 0 failed.
```
使用缓存后:
```bash
$ webbench -c 500 -t 30 http://www.linuxzen.com/
Webbench - Simple Web Benchmark 1.5
Copyright (c) Radim Kolar 1997-2004, GPL Open Source Software.

Benchmarking: GET http://www.linuxzen.com/
500 clients, running 30 sec.

Speed=256 pages/min, 238544 bytes/sec.
Requests: 128 susceed, 0 failed.
```
明显快了很多...
