Title: PyQt 中用 QtNetwork 异步发起HTTP请求
Date: 2014-05-07 15:52
Category: PyQt
Tags: PyQt, QtNetwork, QHttp, QNetworkAccessManager, HTTP, 网络, 异步

## 引子
最近有需求要在 PyQt 中请求一个链接, 因为比较简单直接用 urllib2 处理了, 但是 urllib2 在
有延时的时候会造成 GUI 界面卡死. 所以今天研究研究 QtNetwork 模块.

QtNetwork 中的请求在 PyQt 中都是异步的.

## 简单的请求 QHttp
### 发起一个GET请求
`PyQt4.QtNetwork.QHttp` 可以发起一个简单请求, 需要注意的是这个对象需要通过调用
`setHost` 设置请求主机, 然后 调用 `get`/`post` 传入 `path` 才能正常使用.

```python
#!/usr/bin/env python
# -*- coding:utf-8 -*-
from PyQt4 import QtGui, QtCore, QtNetwork


class MainWidget(QtGui.QWidget):

    def __init__(self, parent=None):
        super(MainWidget, self).__init__(parent=parent)

        self.http = QtNetwork.QHttp(parent=self)

        # 绑定 done 信号
        self.http.done.connect(self.on_req_done)

        self.url = QtCore.QUrl("http://linuxzen.com/")

        # 设置主机
        self.http.setHost(self.url.host(), self.url.port(80))
        self.getId = self.http.get(self.url.path())

    def on_req_done(self, error):
        if not error:
            print "Success"
            print self.http.readAll()
        else:
            print "Error"


if __name__ == "__main__":
    app = QtGui.QApplication([])
    main = MainWidget()
    main.show()
    app.exec_()
```

### 保存文件
如果想要下载文件, 可以给 `get` 方法传一个 `QtCore.QFile` 对象, 会将请求内容保存
到文件
```python
#!/usr/bin/env python
# -*- coding:utf-8 -*-

from PyQt4 import QtGui, QtCore, QtNetwork


class MainWidget(QtGui.QWidget):

    def __init__(self, parent=None):
        super(MainWidget, self).__init__(parent=parent)

        self.http = QtNetwork.QHttp(parent=self)
        self.http.done.connect(self.on_req_done)

        self.url = QtCore.QUrl("http://www.linuxzen.com/")
        self.http.setHost(self.url.host(), self.url.port(80))
        self.out = QtCore.QFile("./test")
        self.getId = self.http.get(self.url.path(), self.out)

    def on_req_done(self, error):
        # print self.http.readAll(), error
        if not error:
            print "Success"
        else:
            print "Error"


if __name__ == "__main__":
    app = QtGui.QApplication([])
    main = MainWidget()
    main.show()
    app.exec_()
```
上面代码将请求下来的内容保存到当前目录的 test 文件里.

### 处理头部信息
现在有一个需求就是有一个链接会返回一个 301 或 302 的跳转, 但是 `PyQt4.QtNetwork.QHttp` 没有实现自动跳转, 需要我们自动判断头信息进行跳转.

我们可以绑定`PyQt4.QtNetwork.QHttp.responseHeaderReceived`的信号来处理头部信息,
这个信号将给槽传递一个 `PyQt4.QtNetwork.QHttpResponseHeader` 的实例.

通过判断状态吗, 并抓取 `Location` 头实现跳转
```python
#!/usr/bin/env python
# -*- coding:utf-8 -*-
#
from PyQt4 import QtGui, QtCore, QtNetwork


class MainWidget(QtGui.QWidget):

    def __init__(self, parent=None):
        super(MainWidget, self).__init__(parent=parent)

        self.http = QtNetwork.QHttp(parent=self)
        self.http.done.connect(self.on_req_done)
        self.http.responseHeaderReceived.connect(self.on_response_header)

        self.url = QtCore.QUrl("http://t.cn/zTocACq")
        self.http.setHost(self.url.host(), self.url.port(80))
        self.out = QtCore.QFile("./test")
        self.getId = self.http.get(self.url.path(), self.out)

    def on_response_header(self, response_header):
        if response_header.statusCode() in [301, 302]:
            location = response_header.value("Location")
            print "Redirect to: ", location
            self.getId = self.http.get(location, self.out)
            tmp = QtCore.QUrl(location)
            if str(tmp.host()):
                self.url = tmp
                self.http.setHost(self.url.host(), self.url.port(80))
            else:
                self.url.setPath(location)
            self.http.get(self.url.path() or "/", self.out)

    def on_req_done(self, error):
        if not error:
            print "Success"
            print self.http.readAll()
        else:
            print "Error"


if __name__ == "__main__":
    app = QtGui.QApplication([])
    main = MainWidget()
    main.show()
    app.exec_()
```
运行上面代码, 你将看到下面输出
```
Redirect to: http://www.linuxzen.com
Success
```

### 处理参数
如果你的 GET 请求的 url 是带着参数传给 `QUrl` 的, 那么 `QUrl.path()` 将不会返回带参数的
路径需要自己处理. 可以参见下面的处理函数

```python
    def get_query_string(self):
        return self.url.queryPairDelimiter().join(
            "{0}{1}{2}".format(k, self.url.queryValueDelimiter(), v)
            for k, v in self.url.queryItems()
        )
```

请求的时候加上就行了
```python
    self.http.get(self.url.path() + "?" + self.get_query_string())
```

## 处理 Cookie ---- QNetworkAccessManager

QHttp 是无法自动记录 Cookie 和设置 Cookie的, 如果有这个需求就需要 `PyQt4.QtNetwork.QNetworkAccessManager`

`QNetworkAccessManager` 的请求方法不在是传入一个 QString, 而是需要传入一个 
`PyQt4.QtNetwork.QNetworkRequest` 的实例.

`QNetworkAccessManager` 同样是异步的, 可以绑定 `QNetworkAccessManager.finished` 信号, 
这个信号将会给槽传递一个 `PyQt4.QtNetwork.QNetworkReply` 的实例.

`QNetworkAccessManager.setCookieJar` 可以设置一个 `PyQt4.QtNetwork.QNetworkCookieJar` 对象来
自动保存和设置 Cookie.

```python
#!/usr/bin/env python
# -*- coding:utf-8 -*-
#
from PyQt4 import QtGui, QtCore, QtNetwork


class MainWidget(QtGui.QWidget):
    def __init__(self, parent=None):
        super(MainWidget, self).__init__(parent)
        self._cookiejar = QtNetwork.QNetworkCookieJar(parent=self)

        self.manager = QtNetwork.QNetworkAccessManager(parent=self)

        self.manager.setCookieJar(self._cookiejar)

        self.manager.finished.connect(self.on_reply)

        self.req = QtNetwork.QNetworkRequest(
            QtCore.QUrl("http://www.google.com.hk"))

        self.manager.get(self.req)

    def on_reply(self, reply):
        print reply, self._cookiejar.allCookies()
        print reply.rawHeaderList()
        # print reply.readAll()


if __name__ == "__main__":
    app = QtGui.QApplication([])
    widget = MainWidget()
    widget.show()
    app.exec_()
```

这样就可以携带 Cookie 去请求一些需要 Cookie 认证的请求.


## 总结
还有很多内容就不一一介绍, 比如配合 `QProgressBar`, 添加头部信息, POST 请求, 等.

这里只是简单介绍下 Qt 自己的网络请求处理方法. 在 PyQt 里用 urllib 等库明显不是好的
解决办法, 因为会造成界面卡死. 但是 PyQt4 的方式也实在不是很优雅. 要来来回回的调很多
东西. 但是最起码不会造成界面的卡死.
