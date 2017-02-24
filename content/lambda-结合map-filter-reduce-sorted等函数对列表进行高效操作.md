Title: lambda 结合map/filter/reduce/sorted等函数对列表进行高效操作
Tags: 排序,列表,sorted,reduce,python,map,list,lambda,filter
Category: Python
Date: 2012-08-10 16:02
lambda 结合map/filter/reduce/sorted等函数对列表进行高效操作
## 介绍
### lambda
Python用于支持将函数赋值给变量的一个操作符
默认是返回的,所以不用再加return关键字,不然会报错
```python
result = lambda x:  x * x
result(2) # return 4
map()/filter()/reduce()
```
需要两个参数,第一个是一个处理函数,第二个是一个序列(list,tuple,dict)

### map()
将序列中的元素通过处理函数处理后返回一个新的列表

### filter()
将序列中的元素通过函数过滤后返回一个新的列表

### reduce()
将序列中的元素通过一个二元函数处理返回一个结果

## 将上面三个函数和lambda结合使用
```python
li = [1, 2, 3, 4, 5]
# 序列中的每个元素加1
map(lambda x: x+1, li) # [2,3,4,5,6]

# 返回序列中的偶数
filter(lambda x: x % 2 == 0, li) # [2, 4]

# 返回所有元素相乘的结果
reduce(lambda x, y: x * y, li) # 1*2*3*4*5 = 120
```

## sorted() 结合lambda对列表进行排序
sorted 用于列表的排序,比列表自带的更加智能
有两个列表,每个列表中都有一个字典([{},{}])要求将两个这样的列表合并后按照时间排序,
两个列表中的时间为了能够通过json输出已经由时间格式转变为字符串格式.字段名为 sort_time
现在将他们按照倒序排列

### sorted 的用法
sorted(iterable, cmp=None, key=None, reverse=False) --> new sorted list
* terable：是可迭代类型;
* cmp：用于比较的函数，比较什么由key决定,有默认值，迭代集合中的一项;
* key：用列表元素的某个属性和函数进行作为关键字，有默认值，迭代集合中的一项;
* reverse：排序规则. reverse = True 或者 reverse = False，有默认值。
* 返回值：是一个经过排序的可迭代类型，与iterable一样。

### sorted()结合lambda对可迭代类型用sort_time排序
```python
sorted(data, key=lambda d: d['sort_time'], reverse=True)
```
