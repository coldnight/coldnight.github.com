Title: 使用WebQQ协议桥接XMPP和QQ群
Date: 2013-03-14 16:09
Tags: WebQQ, xmpp, python, XMPP, pyxmpp2
Category: Python

## 介绍
无意中看见有人利用WebQQ协议开发出Linux下Pidgin的插件, 让Pidgin来收发QQ消息, 突然想将[clubot](/python-shi-yong-pyxmpp2bian-xie-gtalkqun.html)和QQ群来桥接起来一定非常有趣,这样就可以通过gtalk收发QQ来的消息, 不过前期还是想将`clubot`和QQ群桥接起来.

## 实施
想到了就开始弄呗, 于是上网找了写有关WebQQ的协议, 首先写出了一个根据`urllib2`的版本并使用线程同时跑WebQQ和xmpp, 源码可以查看:
[thread_version](https://github.com/coldnight/qxbot/tree/threading_version)

## 优化
上面的线程版效率不是很高, 由于都是网络请求, 所以想加入可以加入到pyxmpp2的mainloop中, 使用复用I/O模型来提高效率, 首先需要解决的是将http请求通过urllib2改为socket, 于是写出HTTPSock类来实现这个需求:
```python
#!/usr/bin/env python
# -*- coding:utf-8 -*-
#
#   Author  :   Wood.D
#   E-mail  :   wh_linux@126.com
#   Date    :   13/03/04 09:58:26
#   Desc    :   Http Socket 实现
#
import ssl
import socket
import urllib
import urllib2
import httplib
import urlparse
import tempfile
import cookielib
from lib.utils import Form

class HTTPSock(object):
    """ 构建支持Cookie的HTTP socket
    供可复用的I/O模型调用"""
    def __init__(self):
        cookiefile = tempfile.mktemp()
        self.cookiejar = cookielib.MozillaCookieJar(cookiefile)

    def make_request(self, url, form, method = "GET"):
        """ 根据url 参数 构建 urllib2.Request """
        request = urllib2.Request(url)
        if isinstance(form, Form):
            request.add_header("Content-Type", form.get_content_type())
            request.add_header("Content-Length", len(str(form)))
            request.add_data(str(form))
        elif isinstance(form, (dict, list, tuple)):
            params = urllib.urlencode(form)
            if method == "GET":
                url = "{0}?{1}".format(url, params)
                request = urllib2.Request(url)
            else:
                request = urllib2.Request(url, params)
                request.add_header("Content-Type", "application/x-www-form-urlencoded")

        self.cookiejar.add_cookie_header(request)
        request.headers.update(request.unredirected_hdrs)
        return request

    def make_response(self, sock, req, method):
        """ 根据socket和urlib2.Request 构建Response """
        r = httplib.HTTPResponse(sock, 0, strict = 0, method = method, buffering=True)
        r.begin()

        r.recv = r.read
        fp = socket._fileobject(r, close=True)

        resp = urllib.addinfourl(fp, r.msg, req.get_full_url())
        resp.code = r.status
        resp.msg = r.reason
        self.cookiejar.extract_cookies(resp, req)
        self.cookiejar.save()
        return resp


    def make_http_sock_data(self, request):
        """ 根据urllib2.Request 构建socket和用于发送的HTTP源数据 """
        url = request.get_full_url()
        headers = request.headers
        data = request.get_data()
        parse = urlparse.urlparse(url)
        host, port = urllib.splitport(parse.netloc)
        typ = parse.scheme
        port = port if port else getattr(httplib, typ.upper() + "_PORT")
        data =  self.get_http_source(parse, data, headers)
        if hasattr(self, "do_" + typ):
            return getattr(self, "do_"+typ)(host, port), data

    def do_http(self, host, port):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(3)
        sock.connect((host, int(port)))
        sock.setblocking(0)
        return sock

    def do_https(self, host, port, keyfile = None, certfile = None):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(3)
        sock.connect((host, int(port)))
        sock = ssl.wrap_socket(sock, keyfile, certfile)
        sock.setblocking(0)
        return sock

    def get_http_source(self, parse, data, headers):
        path = parse.path
        query = parse.query
        path = path + "?" + query if query else path
        path = path if path else "/"
        method = "POST" if data else "GET"
        _buffer= ["{0} {1} HTTP/1.1".format(method, path)]
        e_headers = [(k.lower(), v) for k, v in headers.items()]
        headers = []
        headers.append(("Host", parse.netloc))
        headers.append(("Connection", "keep-alive"))
        headers.append(("Accept", "*/*"))
        headers.append(("Accept-Charset", "UTF-8,*;q=0.5"))
        headers.append(("Accept-Encoding", "gzip,deflate,sdch"))
        headers.append(("Accept-Language", "zh-CN,zh;q=0.8"))
        headers.append(("User-Agent", "Mozilla/5.0 (X11; Linux x86_64)"\
                        " AppleWebKit/537.11 (KHTML, like Gecko)"\
                        " Chrome/23.0.1271.97 Safari/537.11"))
        headers+= e_headers
        if data:
            headers.append(("Content-Length",   len(data)))
        for key, value in headers:
            _buffer.append("{0}: {1}".format(key.title(), value))
        _buffer.extend(("", ""))
        result = "\r\n".join(_buffer)
        if isinstance(data, str):
            result += data
        return result

    @property
    def cookie(self):
        return self.cookiejar._cookies
```
主要是根据`urllib2.Request`构建socket和socket要发送的数据, 然后将socket返回的数据构建成`response`, 然后编写一些handlers来加入到mainloop中去,优化后的版本:
[epoll_version](https://github.com/coldnight/qxbot/tree/master)

这个版本使用了epoll作为主循环, 更加高效.

## 最新版本
最新版本分离了WebQQ作为一个包, 如仅需WebQQ的功能可以很方便的分离出来(当然要仿照pyxmpp2来实现一套事件机制), 源码:
[last](https://github.com/coldnight/qxbot/)
