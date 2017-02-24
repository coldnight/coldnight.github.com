Title: mongoengine使用笔记
Tags: python,pymongo,nosql,mongoengine,mongodb
Category: Python
Date: 2012-06-23 10:02
最近重新拾起Django,但是Django并不支持mongodb,但是有一个模块mongoengine可以实现Django Model类似的封装.但是mongoengine的中文文档几乎没有,有的也是简短的几句介绍和使用.下面我就分享一下我在使用过程中所记录下的一些笔记,可能有点乱.大家可以参考一下.
## 安装mongoengine
```python
easy_install pymongo # 依赖库
easy_install mongoengine
```
## 基本使用
```python
from mongoengine import *
from datetime import datetime
# 连接数据库
connect('blog')   # 连接本地blog数据库
# 如需验证和指定主机名
# connect('blog', host='192.168.3.1', username='root', password='1234')

# 定义分类文档
class Categories(Document):
    ' 继承Document类,为普通文档 '
    name = StringField(max_length=30, required=True)
    artnum = IntField(default=0, required=True)
    date = DateTimeField(default=datetime.now(), required=True)
```
和Django的model使用很类似,所以也不解释什么.

### 插入
```python
cate = Categories(name="Linux")   # 如果required为True则必须赋予初始值,如果有default,赋予初始值则使用默认值
cate.save() # 保存到数据库
```

### 查询和更新
文档类有一个 objects 属性.我们使用它来查询数据库.
```python
# 返回集合里的所有文档对象的列表
cate = Categories.objects.all()

# 返回所有符合查询条件的结果的文档对象列表
cate = Categories.objects(name="Python")
# 更新查询到的文档:
cate.name = "LinuxZen"
cate.update()
查询数组 默认查询数组"="代表的意思是in:
class Posts(Document):
    artid = IntField(required=True)
    title = StringField(max_length=100, required=True)
    content = StringField(required=True)
    author = ReferenceField(User)
    tags = ListField(StringField(max_length=20, required=True), required=True)
    categories = ReferenceField(Categories), required=True)
    comments = IntField(default=0, required=True)

# 将会返回所有tags包含coding的文档
Posts.objects(tags='coding')
```
### ReferenceField 引用字段:
通过引用字段可以通过文档直接获取引用字段引用的那个文档:
```python
class Categories(Document):
    name = StringField(max_length=30, required=True)
    artnum = IntField(default=0, required=True)
    date = DateTimeField(default=datetime.now(), required=True)

class Posts(Document):

    title = StringField(max_length=100, required=True)
    content = StringField(required=True)
    tags = ListField(StringField(max_length=20, required=True), required=True)
    categories = ReferenceField(Categories)
```
#### 插入引用字段
```python
cate =Categories(name="Linux")
cate.save()
post = Posts(title="Linuxzen.com", content="Linuxzen.com",tags=["Linux","web"], categories=cate)
post.save()
```

#### 通过引用字段直接获取引用文档对象
一般文档查询会返回一个列表(尽管只有一个结果),我们想要获得一个文档对象可以使用索引获取第一个文档对象,但是mongoengine建议使用first()来获取第一个:
```python
>>> cate = Posts.objects.all().first().categories
>>> cate

>>> cate.name
u'Linux'
```
查询包含Linux分类的文章
```python
>>> cate = Categories.objects(name="Linux").first()
>>> Posts.objects(categories=cate)
```
### EmbeddedDocument 嵌入文档
继承EmbeddedDocument的文档类就是嵌入文档,嵌入文档用于嵌入其他文档的EmbeddedDocumentField 字段,比如上面例子的tags字段如果改成嵌入文档的话可以将Posts文档类改成如下方式:
```python
class Posts(Document):

    title = StringField(max_length=100, required=True)
    content = StringField(required=True)
    tags = ListField(EmbeddedDocumentField('Tags')required=True)
    categories = ReferenceField(Categories)
```
还需要添加一个Tags嵌入文档类:
```python
class Tags(EmbeddedDocument):
name = StringField()
date = DateTimeField(default=datetime.now())
```
我们像如下方式插入Posts文档中的Tags
```python
>>> tag = Tags(name="Linuxzen")
>>> post = Posts(title="Linuxzen.com", content="Linuxzen.com", tags=[tag], categories=cate)
>>> tag = Tags(name="mysite")
>>> post.tags.append(tag)
>>> post.save()
>>> tags = post.tags
>>> for tag in tags:
print tag.name

Linuxzen
mysite
```
### 时间段查询
```python
    start = datetime(int(year), int(month), 1)
    if int(month) + 1 > 12:
        emonth = 1
        eyear = int(year) + 1
    else:
        emonth = int(month) + 1
        eyear = int(year)
    end = datetime(eyear, emonth, 1)
    articles = Posts.objects(date__gte=start, date__lt=end).order_by('-date')
```
### 分片
slice用于分片
```python
# comments - skip 5, limit 10
Page.objects.fields(slice__comments=[5, 10])

# 也可以使用索引值分片

# limit 5
users = User.objects[:5]

# skip 5
users = User.objects[5:]

# skip 10, limit 15
users = User.objects[10:15]
```
### 使用原始语句查询
如果想使用原始的pymongo查询方式可以使用__raw__操作符 Page.objects(__raw__={'tags':'coding'})
使用$inc和$set操作符
```python
# 更新嵌入文档comments字段by的值为joe的文档字段votes增加1
Page.objects(comments_by="joe").update(inc__votes=1)

# 更新嵌入文档comments字段by的值为joe的文档字段votes设置为1
Page.objects(comments_by="joe").update(set__votes=1)
```
### 其他技巧
```python
#查询结果转换成字典
users_dict = User.objects().to_mongo()

# 排序,按日期排列
user = User.objects.order_by("date")

# 按日期倒序

user = User.objects.order_by("-date")
```
