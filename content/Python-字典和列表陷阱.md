Title: Python 字典和列表陷阱
Tags: 陷阱,引用,字典,列表,python
Category: Python
Date: 2012-09-17 10:22
Python 中有三个非常好用的数据结构,列表,元组和字典,
元组是不可变的,列表可以保存任意类型的Python对象,并可以随意扩展没有大小限制,
字典是一个key-value的键值映射的类型,可以存放任何Python对象,可以嵌套字典,
值可以是字典元组或者字典

这里说是Python 字典和列表的陷阱不如说是Python的一些特性,如果不了解这些特性
就会引发一些难以寻找的bug

下面我们来介绍这些特性

Python中所有对列表和字典的使用仅仅是对原来对象的引用而不是创建一个新的对象
如下面代码:
```python
>>> info = dict(name='cold', blog='www.linuxzen.com') # 创建字典{'name':'cold', 'blog':'www.linuxzen.com'}
>>> info2 = info     # 赋值给info2
>>> info2['name'] = 'cold night'
>>> info
>>> info2
{'blog': 'www.linuxzen.com', 'name': 'cold night'}
>>> info
{'blog': 'www.linuxzen.com', 'name': 'cold night'}
>>> names = ['cold', 'night', 'linuxzen']
>>> names2 = names
>>> names2.append('cold night')
>>> names
['cold', 'night', 'linuxzen', 'cold night']
>>> names2
['cold', 'night', 'linuxzen', 'cold night']
```
大家看到如果将列表或者字典重新赋值给另外一个变量并没有达到预想的效果,
我们更改一个的同时另外一个也在同时更改,如果我们想保留一个快照,很明显我们
没有达到我们想要的效果,另外还有一种常见的使用,因为我们知道普通变量传递给
函数,函数在内部更改是不会影响到外部变量的,那么列表和字典呢?
我们来看如下代码,我们创建一个函数,是字典就添加一个键和值,是列表就在尾部添加一个元素
```python
>>> def add_something(info):
...     if type(info) == dict:
...             info['msg'] = 'Hello,'+ info['name']
...     elif type(info) == list:
...             info.append('add to the list')
... 
>>> info = {'name':'cold', 'blog':'www.linuxzen.com'}
>>> add_something(info)
>>> info
{'blog': 'www.linuxzen.com', 'msg': 'Hello,cold', 'name': 'cold'}
>>> names = ['cold', 'night', 'linuxzen.com']
>>> add_something(names)
>>> names
['cold', 'night', 'linuxzen.com', 'add to the list']
```
如上代码明显不是我们想要的结果,如果这个列表/字典仅仅用在一个地方可能不会发生什么
如果我们其他地方需要同样的列表进行处理,如果你不知道这个特性就会产生很难寻找的bug
当上面并不是我们想要的我们该如何避免上面呢,我们可以对列表/字典做一个拷贝,而不是
简单的引用
```python
>>> names = ['cold', 'night', 'linuxzen.com']
>>> names2 = names[:]
>>> names2.append('cold night')
>>> names
['cold', 'night', 'linuxzen.com']
>>> names2
['cold', 'night', 'linuxzen.com', 'cold night']
>>> info = {'name':'cold night', 'blog':'www.linuxzen.com'}
>>> info2 = info.copy()
>>> info2['name'] = 'cold'
>>> info
{'blog': 'www.linuxzen.com', 'name': 'cold night'}
>>> info2
{'blog': 'www.linuxzen.com', 'name': 'cold'}
```
上面代码列表使用[:]可以创建一个列表的副本而不是引用
字典的copy方法同样可以创建一个字典的副本而不是引用
这样就可以避免之前所说的引用的情况
