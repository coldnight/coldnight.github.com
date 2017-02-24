Title: Python 内存泄露实战分析
Date: 2015-03-30 17:29
Tags: Python, 内存, 泄露, 引用, 回收, 交叉
Category: Python


## 引子
之前一直盲目的认为 Python 不会存在内存泄露, 但是眼看着上线的项目随着运行时间的增长
而越来越大的内存占用, 我意识到我写的程序在发生内存泄露, 之前 debug 过 
[logging 模块导致的内存泄露](http://www.linuxzen.com/logging-mo-kuai-wu-yong-dao-zhi-de-nei-cun-xie-lu.html).

目前看来, 还有别的地方引起的内存泄露. 经过一天的奋战, 终于找到了内存泄露的地方, 目前项目
跑了很长时间, 在业务量较小的时候内存还是能回到刚启动的时候的内存占用.

## 什么情况下不用这么麻烦
如果你的程序只是跑一下就退出大可不必大费周章的去查找是否有内存泄露, 因为 Python 在退出时
会释放它所分配的所有内存, 如果你的程序需要连续跑很长时间那么就要仔细的查找是否
产生了内存泄露.

## 场景
如何产生的内存泄露呢, 项目是一个 TCP server, 每当有连接过来时都会创建一个连接实例来进行
管理, 每次断开时连接实例还被占用并没有释放. 没有被释放的原因肯定是因为有某个地方对连接
实例的引用没有释放, 所以随着时间的推移, 连接创建分配内存, 连接断开并没有释放掉内存, 所以
就会产生内存泄露.

## 调试方法
由于不知道具体是哪里引起的内存泄露, 所以要耐心的一点点调试. 

由于知道了断开连接时没有释放, 所以我就不停的模拟创建连接然后发送一些包后断开连接, 
然后通过下面一行 shell 来观察内存占用情况:
```shell
PID=50662;while true; do; ps aux | grep $PID | grep -v grep | awk '{print $5" "$6}' >> t; sleep 1; done
```

如果在增长了一定的量后保持住就说明已经没有产生泄露.

同时可以在对象该释放的时候查看对象的引用计数, 通过 `sys.getrefcount(obj)`. 如果引用计数变为了 `2` 
则说明该对象在跳出命名空间后就会被正确回收.

## 产生原因

项目中两种情况导致对象没有被正确回收:

* 被退出才回收的对象引用
* 交叉引用


### 被退出才回收的对象引用
为了追踪连接所以把连接对象同时放在一个列表里, 而这个列表只有在程序退出时才会被回收, 
如果不正确处理, 那么分配的对象将也会只在程序退出时才会被回收.

全局变量和类变量都只会在程序退出的时候才会被回收:
```python

_CONNECTIONS = []

# ...
class Connection(object):
    def __init__(self, sock, address)
        pass

def server_loop():
    # ...
    sock, address = server_sock.accept()
    connection = Connection(sock, address)
    _CONNECTIONS.append(connection)
    # ...
    sock.close()
```

上面把所有建立的连接都放在全局变量 `_CONNECTIONS` 里, 如果在关闭的时候不从这个列表
里取出(减少引用)则 connection 对象就不会被回收, 则每建立一次连接就会有个连接对象和连接
对象引用的对象不会被回收.

如果把对象放在一个类属性里也是一样的, 因为类对象在程序一开始就分配, 并在程序退出时才被回收.

解决办法就是在退出时从列表(或其他对象)里解除对对象的引用(删除)
```python

_CONNECTIONS = []

# ...
class Connection(object):
    def __init__(self, sock, address)
        pass

def server_loop():
    # ...
    sock, address = server_sock.accept()
    connection = Connection(sock, address)
    _CONNECTIONS.append(connection)
    try:
        # ...
        sock.close()
    finally:
        _CONNECTIONS.remove(connection) # XXX
```

### 交叉引用
有时候我们为对象分配一个实例属性时需要将自己本身赋值给实例属性, 作为实例属性的实例属性,
说着很拗口, 看一下代码:
```python

class ConnectionHandler(object):
    def __init__(self, connection):
        self._conn = connection


class Connection(object):
    def __init__(self, sock, address)
        self._conn_handler = ConnectionHandler(self) # XXX
```

上面的代码就会产生交叉引用, 交叉引用会让解释器困惑, 从而之后只能靠2代和3代回收, 这个过程可能会很慢.

解决这种问题的方法就是使用 `弱引用`
```python
import weakref

class ConnectionHandler(object):
    def __init__(self, connection):
        self._conn = connection


class Connection(object):
    def __init__(self, sock, address)
        self._conn_handler = ConnectionHandler(weakref.proxy(self)) # XXX
```

### 日志模块
参见 [logging 模块导致的内存泄露](http://www.linuxzen.com/logging-mo-kuai-wu-yong-dao-zhi-de-nei-cun-xie-lu.html).

## 参考
[Python 垃圾回收](http://www.cnblogs.com/vamei/p/3232088.html)
