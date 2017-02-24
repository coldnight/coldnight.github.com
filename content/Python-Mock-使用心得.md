title: Python mock 使用心得
tags: Python,mock,unittest
category: Python
date: 2016-04-03 11:42

好久没有更新博客, 趁着清明节小长假和我儿子正在睡觉更新一篇刷刷存在感.
近来变化很多, 儿子也有了, 工作上也有很多收获. 这篇博客就分享一下关于 `mock` 的使用的心得体会.

很长一段时间以来写单元测试都类似写执行脚本, 运行一下然后看一下结果. 
这里面有一部分原因是因为无法规避外部的依赖组件, 比如:

- 数据库操作
- 外部接口调用
- 外部其他不可控因素

这样写测试只关心当前测试的结果, 而不去管其他测试是否 `passed`.

后面随着团队开始进新人, 由于团队里每个人的标准和水平不同, 
开始不得不重视整体项目的质量, 发现没有好的测试就没有统一的标准来衡量提交代码的质量,
当然说到代码质量还有另外一个和测试放在一起的标准就是代码风格, 这不是本文的主题所里这里就暂且不提.

为了能写好测试就不得不面对现实项目的复杂性, 诸如外部接口数据库操作等. 
这时开始将目光转向 `mock`, 因为之前有听过类似概念, 但是还是有误解, 
以为把要测的东西都模拟掉了还测试什么呢? 但是真正的了解 `mock` 之后才完整的理解了单元测试.


**单元测试应该只针对当前单元进行测试, 所有的外部依赖应该是稳定的, 在别处进行测试过的.**
使用 `mock` 就可以对外部依赖组件实现进行模拟并且替换掉, 从而隐藏外部组件的实现,
使得单元测试将焦点只放在当前的逻辑(当前单元),

## 安装

`mock` 在 Python3 中是内置的, 直接 `import unittest.mock` 即可, 
但是在 Python2 中是需要额外安装的, 安装完 `import mock` 即可

```shell
pip install -U mock
```

## 技巧

在本文不详细介绍如何使用, 具体请参见[官方文档](https://docs.python.org/3/library/unittest.mock.html).
这里分享几个技巧.

安装官方文档给的示例一开始像下面这么使用 mock

```python
class DemoTestCase(unittest.TestCase):
    @mock.patch("pkg.mod.dep_mod.func", autospec=True)
    def test_demo(self, func_mock):
        func_mock.return_value = False

        # real test code
        func_mock.assert_called_with(arg1, arg2)
```

但是当整个测试都依赖这个组件时上面的使用方式就会产生大量的相同的初始化代码, 
所以定义了一个装饰器向下面这样

```python
import unittest
import functools

import mock

def _mock_wrapper(func):
    @functools.wraps(func)
    @mock.patch("pkg.mod.dep_mod.func", autospec=True)
    def wrapper(self, func_mock)
        func_mock.return_value = False
        return func(self, func_mock)

    return wrapper

class DemoTestCase(unittest.TestCase):
    @_mock_wrapper
    def test_demo(self, func_mock):
        # real test code
        func_mock.assert_called_with(arg1, arg2)
```

上面操作是很方便, 但是有几个缺点:

- 代码不够清晰
- 如果增加 mock 组件则需要修改每一个被装饰函数接收的参数


所以在仔细阅读官方文档后发现, `mock.patch` 返回一个对象, 可以通过 `start`/`stop` 方法来应用.
所以产生了下面的代码.

```python
class DemoTestCase(unittest.TestCase):
    def setUp(self):
        super(DemoTestCase, self).setUp()
        self._patcher = mock.patch("pkg.mod.dep_mod.func", autospec=True)
        self._func_mock = self._patcher.start()
        self._func_mock.return_value = 1

    def tearDown(self):
        super(DemoTestCase, self).tearDown()
        # cleanup
        self._patcher.stop()

    def test_demo(self):
        # real test code
        self._func_mock.assert_called_with(arg1, arg2)
```

当然如果有多个依赖组件需要 `mock` 可以将 `patcher` 存在一个列表里在 `tearDown` 方法里统一清理.

**不要忘记清理, 因为当前 `mock` 的组件应当仅在当前测试里生效, 如果忘记了清理可能会影响到其他测试.**

如果当前单元测试仅有部分测试依赖该组件也可以通过上下文管理的方式进行管理, 更加灵活.

```python
import contextlib

class DemoTestCase(unittest.TestCase):
    @contextlib.contextmanger
    def _mock_context(self):
        patcher = mock.patch("pkg.mod.dep_mod.func", autospec=True)
        try:
            func_mock = patcher.start()
            func_mock.return_value = 1
            yield func_mock
        finally:
            patcher.stop()

    def test_demo(self):
        with self._mock_context() as func_mock:
            # real test code
            func_mock.assert_called_with(arg1, arg2)
```

## 写在后面的话
测试对一个项目很重要, 特别是对上了规模的项目, 上了 `mock` 之后我花了很长时间将已有项目的测试整理完毕, 
使测试有效并且可以全部 `passed`. 
有了良好的基础之后接下来就很方便的使用了一些集成工具来限制不规范的代码提交到长期分支, 
这部分内容将放在下一篇去分享.

