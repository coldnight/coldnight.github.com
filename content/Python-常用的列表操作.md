Title: Python 常用的列表操作
Tags: 重复,相加,添加,列表,python
Category: Python
Date: 2012-08-10 15:48
这里介绍几个常用的列表操作
### 添加元素
添加元素使用列表的内置方法append
```python
number = [1, 2, 3, 4]
number.append(5) # number = [1, 2, 3, 4, 5]
number.append([6,7]) # number = [1, 2, 3, 4, 5, [6, 7]]
number.append({'a':'b'}) # number = [1, 2, 3, 4, [6, 7], {'a', :'b'}
```
可以看到强大的python列表可以嵌套任意类型

### 列表相加
要想连接两个列表,可以使用+号连接
```python
a = [1, 2, 3]
b = [4, 5, 6]
c = a + b # c = [1, 2, 3, 4, 5, 6]
```
也可以使用列表内置方法extend连接两个列表
```python
a = [1, 2, 3]
b = [4, 5, 6]
a.extend(b) # a = [1, 2, 3, 4, 5, 6]
```
用+号会创建一个新通对象,使用extend则在原来的对象上面修改

### 列表去重复
列表本身没有去除重复的功能,但是可以借助python的另外一个类型set(help(set)查看)
```python
a = [1, 2, 3, 3,2, 1]
b = list(set(a)) # b = [1, 2, 3]
```
也可以借助字典类型的内置方法
```python
a = [1, 2, 2, 3, 1, 3]
b = {}.fromkeys(a).keys() # b = [1, 2, 3]
```
