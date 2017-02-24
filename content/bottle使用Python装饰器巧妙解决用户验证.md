Title: bottle使用Python装饰器巧妙解决用户验证
Tags: 验证,装饰器,用户,python,bottle
Category: Python
Date: 2012-06-16 17:33
上篇文章发布了一个自己写的用bottle写的web程序,其中收获最大就是Python装饰器的使用.前几天也是比较忙,所以没能分享出来,今天就给大家分享一下.

首先来分析下需求,web程序后台需要认证,后台页面包含多个页面,最普通的方法就是为每个url添加认证,但是这样就需要每个每个绑定url的后台函数都需要添加类似或者相同的代码,但是这样做代码就过度冗余,而且不利于扩展.

接下来我们先不谈及装饰器,我们都知道Python是个很强大的语言,她可以将函数当做参数传递给函数,最简单的:
```python
def p():
    print 'Hello,world'

def funcfactor(func):
    print 'calling function named', func.__name__
    func()
    print 'end'

funcfactor(p)
# 输出为:
# calling function named p
# Hello,world
# end
```
一目了然的程序,定义一个函数p(),将函数p当做参数传递给喊出funcfactor,在执行p函数前后加上一些动作.

我们还可以这么做:
```python
def p():
    print 'Hello,world'
def funcfactor(func):
    print 'calling function named', func.__name__
    return func

func = funcfactor(p)
func()
# 输出为:
# calling function named p
Hello,world
```
正如你看到的,我们可以将函数返回然后赋予一个变量,留待稍后调用.但是这种情况下我们要想在函数执行后做点什么就不可能,但是我们的Python是强大的,Python可以在函数中再嵌套一个函数,我们可以像下面这么做:
```python
def p():
    print 'Hello, world'

def funcfactor(func):
    def wrapper():
        print 'do something at start'
        func()
        print 'do something at end'
    return wrapper

func = funcfactor(p)
func()
#输出为:
# do something at start
# Hello, world
# do something at end
```
下面我们来看看装饰器,上面的代码虽然实现的一个很困难的任务,但是还不够优雅,而且代码不符合Python的哲学思想,所以装饰器就应声而出,装饰器没有和上面的原理相同,同样用于包装函数,只是代码实现上更加优雅和便于阅读.装饰器以@开头后面跟上装饰器的名称,紧接着下一行就是要包装的函数体,上面的例子用装饰器可用如下方式实现:
```python
def decorator(func):
    def wrapper():
        print 'do something at start'
        func()
        print 'do something at end'
    return wrapper

@decorator
def p():
    print 'Hello, world'

p()
#输出为:
# do something at start
# Hello, world
# do something at end
```
实际上装饰器并没有性能方面或其他方面的提升,仅仅是一种语法糖,就是上面一个例子的改写,这样更加优雅和便与阅读.
如果我们的p()函数不想仅仅只输Hello,world,我们想向某些我们指定的人打招呼:
```python
def decorator(func):
    def wrapper(*args, **kargs):
        print 'do something at start'
        func(**kargs)
        print 'do something at end'
    return wrapper

@decorator
def p(name):
    print 'Hello', name

p(name="Jim")
#输出为:
# do something at start
# Hello Jim
# do something at end
```
装饰器在装饰不需要参数的装饰器嵌套函数不是必须得,如果被装饰的函数需要参数,必须嵌套一个函数来处理参数.
写到这里想必大家也知道装饰器的用法和作用.现在回到正题,如何优雅的给后台url加上验证功能?毫无疑问我们使用装饰器来处理:
```python
def blog_auth(func):
    '''
    定义一个装饰器用于装饰需要验证的页面
    装饰器必须放在route装饰器下面
    '''
    # 定义包装函数
    def wrapper(*args, **kargs):
        try:
            # 读取cookie
            user = request.COOKIES['user']
            shell = request.COOKIES['shell']
        except:
            # 出现异常则重定向到登录页面
            redirect('/login')

        # 验证用户数据
        if checkShell(user, shell):
            # 校验成功则返回函数
            return func(**kargs)
        else:
            # 否则则重定向到登录页面
            redirect('/login')
    return wrapper
```
可以再需要验证的地方添加blog_auth装饰器:
```python
@route('/admin:#/?#')
@blog_auth
def admin():
    '''
    用于显示后台管理首页
    '''
    TEMPLATE['title'] = '仪表盘 | ' + TEMPLATE['BLOG_NAME']
    TEMPLATE['user'] = request.COOKIES['user']
    articles = []
    for article in db.posts.find().sort("date",DESCENDING).limit(10):
        articles.append(article)

    # 将文章列表交给前台模版
    TEMPLATE['articles'] = articles
    return template('admin.html',TEMPLATE)
```
至此bottle验证的问题就很优雅的用装饰器解决了.
