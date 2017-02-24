Title: python里的三目运算
Tags: 单行,三目,python,or,if,else,and
Category: Python
Date: 2012-08-10 16:28
下面说的和三目运算有点相似,但又不一样,实在不知道该如何拟定标题,先就是这个标题吧,大家都知道python中没有三目运算,但是`and`/`or`有点类似三目运算:
## and/or
单独使用表示逻辑关系与和或,也可以组和使用,用法如下
### and
and前后如果某一个值为假(False, '', [], {}, None…)则返回第一个假值
如果所有值都为真则返回最后一个真值

### or
如果or任意一个值为真,则立刻返回这个值
如果所有值都为假,则or返回最后一个假值

### 例子
```python
result = 'test' and True # result = True
result = 'test' and 'ortest' # result = ortest
result = False and 'ortest' # result = False
result = '' and None  # result = ''

result = '' or "Hall" # result = Hall
result = False or None # result = None
result = 'test' or 'nottest' # result = test
```

## 使用单行if else 模拟三目运算
result if True / False else fresult
if为真时候结果为result,为假的时候结果为fresult
```python
result = 'test' if True else 'not test' # result = 'test'
result = 'test' if False else 'not test' # result = 'not test'
```
