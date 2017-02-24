Title: Linux 下使用Python截图自动分享
Tags: Linux,scrot,截图,python
Category: Python
Date: 2013-01-22 16:59
## 引子
Linux下不支持QQ等功能丰富的IM,虽然可以通过wine运行QQ2012,但是还是喜欢在gtalk群中聊天,gtalk群不支持图片方式,这就要靠我们大家自己来解决了,[eleven](http://eleveni386.7axu.com)开放了一个Image上传和显示接口,提供了使用`curl`来解决,但是我们公司的网络使用`squid`禁止了`curl`的访问,所以整天看他们这么爽的分享图片我也不甘心阿,所以就使用Python写了一个分享图片的脚本

## 实现
使用scrot截图,然后使用urllib2库上传图片,如果存在PyQt4库则会将结果放到剪贴板上,如果不存在则输出,自行复制

## 代码
```python
#!/usr/bin/env python
# -*- coding:utf-8 -*-
#
#   Author  :   cold
#   E-mail  :   wh_linux@126.com
#   Date    :   13/01/21 09:54:39
#   Desc    :   贴代码和图片
#
import urllib2, json
import mimetools
import mimetypes
import itertools

__host__ = "http://eleveni386.7axu.com"

class Form(object):
    def __init__(self):
        self.form_fields = []
        self.files = []
        self.boundary = mimetools.choose_boundary()
        self.content_type = "application/x-www-form-urlencoded"
        return

    def get_content_type(self):
        return self.content_type

    def add_field(self, name, value):
        self.form_fields.append((name, value))
        return

    def add_file(self, fieldname, filename, fileHandle, mimetype=None):
        body = fileHandle.read()
        if mimetype is None:
            mimetype = ( mimetypes.guess_type(filename)[0]
                         or
                         'applicatioin/octet-stream')
        self.files.append((fieldname, filename, mimetype, body))
        self.content_type = 'multipart/form-data; boundary=%s' % self.boundary

        return

    def __str__(self):
        parts = []
        part_boundary = '--' + self.boundary

        parts.extend(
            [ part_boundary,
             'Content-Disposition: form-data; name="%s"' % name,
             '',
             value,
             ]
            for name, value in self.form_fields)
        if self.files:
            parts.extend([
                part_boundary,
                'Content-Disposition: form-data; name="%s"; filename="%s"' %\
                (field_name, filename),
                'Content-Type: %s' % content_type,
                '',
                body,
            ] for field_name, filename, content_type, body in self.files)

        flattened = list(itertools.chain(*parts))
        flattened.append('--' + self.boundary + '--')
        flattened.append('')
        return '\r\n'.join(flattened)


class HttpHelper(object):
    def __init__(self, url = None, form = None, method = 'GET'):
        self._url = url
        self._form = form
        self._body = str(form)
        self._method = method
        self._dst_url = None
        if url:
            self.make_request()

    def make_request(self):
        url = self._url
        if not self._url.startswith('http://'):
            url = 'http://' + self._url
        self.request = urllib2.Request(url)
        if self._form:
            self.add_header("Content-Type", self._form.get_content_type())
            self.add_header("Content-Length", len(self._body))
            self.request.add_data(self._body)

    def add_header(self, key, val):
        self.request.add_header(key, val)

    def change(self, url, params = {}, method = 'GET'):
        self._url = url
        self._params = params
        self._method = method
        self.make_request()

    def open(self):
        response = urllib2.urlopen(self.request)
        content = response.read()
        self._dst_url = response.geturl()
        try:
            return json.loads(content)
        except:
            return content

if __name__ == "__main__":
    import argparse
    import os
    parser = argparse.ArgumentParser()
    parser.add_argument(dest="path", nargs="?")
    args = parser.parse_args()
    if args.path:
        path = args.path
    else:
        path = r"/tmp/tmpscrot.png"
        os.system("scrot -s {0}".format(path))
    form = Form()
    filename = os.path.split(path)[-1]
    form.add_file(fieldname='mypic', filename=filename,
                  fileHandle=open(path))
    http = HttpHelper( __host__ + '/Image/', form)
    url = http.open()
    try:
        from PyQt4.QtGui import QApplication
        app = QApplication([])
        cb = QApplication.clipboard()
        cb.setText(url)
    except:
        print url
```

## 安装
将上面代码保存一个文件,放在`PATH`路径里,赋予`执行权限`即可

## 使用
默认的不跟图片地址则会截图,截图完毕后自动分享,如安装了PyQt4库则会将结果放到剪贴板,如没有则输出结果.如果脚本给了图片路径参数则上传给定路径的图片
