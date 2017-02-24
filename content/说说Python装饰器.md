title: 说说Python装饰器
date: 2013-05-20 14:52
category: Python
tags: Python, 装饰器

装饰器对与Python新手以至于熟悉Python的人都是一个难理解, 难写的东西. 那么今天就分享一下我对Python 装饰器的理解

所谓装饰器仅仅是一种语法糖, 可作用的对象可以是函数也可以是类, 装饰器本身是一个函数, 其主要工作方式就是将被装饰的类或者函数当作参数传递给装饰器函数, 比如定义如下装饰器
```python
import time

def run_time(func):
    def wrapper(*args, **kwargs):
        start = time.time()
        r = func(*args, **kwargs)
        print time.time() - start
        return r
    return wrapper
```
我们用这个装饰器装饰一个`test`函数
```python
@run_time
def test():
    print "just a test"
```
前面说过其实装饰器就是一个语法糖, 就是将被装饰的函数作为参数传递给装饰器函数, 所以上面可以展开为
```python
test = run_time(test)
```
装饰器将在解释器运行一开始就被加载, 从而将被装饰的函数将被展开成如上方式, 因为 `run_time`装饰器返回`wrapper`函数, 所以当调用`test`函数时其实就是对`wrapper`的调用

如果你在Python shell下执行以上语句就会发现定义完`test`函数然后查看`test`时, shell所展示的是wrapper函数:
[![说说Python装饰器](/static/upload/pyshell.png)](http://www.linuxzen.com)

-----
接下来说说如何编写带参数的装饰器, 大家如果细心的话就可以发现其实带参数的装饰器是经过调用"装饰器"函数返回的一个装饰器, 之所以装饰器上打引号是说明其实这个所谓的"装饰器"只不过是一个普通的函数, 但这个普通的函数返回一个装饰器, 可以参看下面例子:
```python
import time

def route(url):
    def decorator(func):
        func.__url__ = url
        return func
    return decorator

@route(r"/")
def index():
    return "Hi"
```

大家可以发现在使用`route`装饰器时我们其实是调用了`route`函数, `route`函数返回一个`decorator`装饰器, 因为我们不需要在装饰器内运行函数, 所以不需要一个`wrapper`函数来收集参数.

-----
以上就是全部内容, 希望对装饰器一知半解的人有些许帮助
