Title: Python超简单截取中文字符串
Tags: 截取,字符串,utf8,python
Category: Python
Date: 2012-07-11 17:55
web应用难免会截取字符串的需求,Python中截取英文很容易:
```python
>>> s = 'abce'
>>> s[0:3]
'abc'
```
但是截取utf-8的中文机会截取一半导致一些不是乱码的乱码.其实utf8截取很简单,这里记下来作为备忘
```python
#-*- coding:utf8 -*-
s = u'中文截取'
s.decode('utf8')[0:3].encode('utf8')
# 结果u'中文截取
```
就这么简单
