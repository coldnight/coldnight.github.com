Title: 迁移到 Python 3
Date: 2017-07-13
Category: Python
Tags: 2to3

前段时间(2017-06-07)我开始决定将公司现有的项目逐渐的迁移到 Python 3. 主要原因有一下几点:

- Python 3.6 新增了一些新的特性我很喜欢

    包括:

    - [PEP484 类型注解](https://www.python.org/dev/peps/pep-0484/)
    - [PEP492 原生的协程异步: async and await](https://www.python.org/dev/peps/pep-0492/)
    - [PEP498 格式化字符串](https://www.python.org/dev/peps/pep-0498/)
- Python 3 的生态已经完善, 我们所使用的一些第三方库都已支持 Python 3(或有其他成熟的替代)
- Python 2 到 2020 年就不在维护

**促成我决定迁移到 Python 3 的主要原因是公司最大的项目的单元测试覆盖率经过一段时间的迭代终于达到了 80% 以上.**

## 迁移方案

由于项目巨大任务艰巨无法短时间内就将项目迁移到 Python 3, 而且当前还有产品上的功能需要迭代.
所以迁移方案是同时兼容 Python 2 和 3, 并在迁移完成之后移除对 Python 2 的支持.

## 搭建 CI

由于之前本身就有持续集成服务(参见[上一篇](https://www.linuxzen.com/python-github-si-you-xiang-mu-tong-guo-buildbot-jin-xing-review.html)), 所以目前需要做的就是在现有的基础增加一套针对 Python 3 的持续集成服务.
并且在没有完全兼容 Python 3 之前针对 Python 3 的持续集成不影响最终构建结果, 仅作为参考.
这里根据不同的 CI 需要进行不同的操作不在本文讨论范围之内就不在深入.

## 兼容库的选择

针对一些 Python 2 和 3 的差异我不打算自己封装, 所以找了一些成熟的第三方库, 我对比了目前比较流行的两个兼容库:

- [future](http://python-future.org/)
- [six](https://pythonhosted.org/six/)

future 看起来使用比较多的黑魔法, 好多兼容都是侵入式的修改. 所以我选择了更加清晰的 six.

## 兼容性问题及解决

可以从 `__future__` 导入如下特性来解决一些常见的不兼容:

```python
from __future__ import print_function 
from __future__ import division   # 解决除法行为不一致
from __future__ import unicode_literals   # 字符串字面量默认为 Unicode
```

### 标准库和内建函数不兼容

Python 3 中更改了一些标准库的名字和一些函数归属的模块, 同时也移除了一些关键字如 `round`.
此种类型的问题可以通过 `six.moves` 来进行解决:


```python
from six.moves.urllib_parse as urlparse
from fix.moves.builtins import round
```

### 字符串

以我的感受解决 Python 2 和 3 之间的兼容性字符串是难度最大的问题之一, 因为 Python 3 重新实现了字符串相关, 
并增加了一些限制, 主要体现在:

1. 移除了 unicode 关键字
2. 原有的 str(bytes) 变成了 unicode
3. bytes 不在支持 format/decode 等方法
4. 不支持 Unicode 和字节序直接拼接和对比(`in` 操作), 一些标准库也明确指定了接收的是字节序还是 Unicode.
5. 通过字符串格式化拼接 Unicode 和字节序会产生不符合预期的结果

  ```python
  >>> '{}'.format(b'test')
  "b'test'"
  ```

这里面第 `1` 条和第 `2` 条是比较容易, 可以通过批量的查找和替换来解决. 3-4 是比较难解决, 但是单元测试基本可以覆盖到.
5 会产生一些非常难以发现的 Bug, 如果一些单元测试没覆盖到的或者本身逻辑就是被 mock 掉就更难排查.
比如我们内部服务通信大量的使用了签名机制, 在进行单元测试时又对这部分逻辑进行了 mock, 
所以在我们在测试环境用 Python 3 启动项目后主要就是解决此类问题.

### 迭代器

Python 3 中除了字符串这一改动难以兼容, 还有一个就是之前返回列表的一些函数或方法改为返回迭代器, 如

- dict.keys/dict.values/dict.items
- map/filter/zip

如果没有对一些迭代器进行展开而是当做列表使用就会产生异常, 或者对迭代器展开没有及时进行收集就会产生一些难以排查的 Bug,
考虑如下代码:

```python
def foo():
    data = {"a": {"n": "1"}  "b": {"n": "2"}}
    digits = data.values()

    for item in digits:
        item["n"] = int(item["n"])
    return digits
```

上面代码在 Python 2 中可以达到预期的行为, 返回 `[{"n": 1}, {"n": 2}]`, 但在 Python 3 会返回一个消耗完的迭代器, 转换之后结果为 `[]`.

下面代码在 Python2 是可以正常工作的, 但是 Python 3 下不行:

```python
digits = {"a": "1", "b": "2"}

for k in digits.keys():
    digits[k] = int(digits[k])
```

主要是 Python 2 该方法返回一个列表是对字典 key 的一次拷贝, 所以在更改字典原有值就不会有问题,
但是 Python 3 中该方法返回了一个迭代器, 是对字典 key 的引用. 如果这时候更新字典就会触发异常.


## 工具

### pylint

`pylint` 有一个选项 `--py3k` 选项可以打开检测一些不兼容 Python 3 的地方, 比如 `map/filter/zip` 没有展开等.

### pre-commit

准确的说应该是 `pre-commit` 下有一些工具可以帮助我们自动修复一些 Python 3 的兼容性问题,
并且可以方便的放在持续集成服务上运行监测, 这里列出一些我们用于帮助 Python 3 迁移的工具:


- [reorder\_python\_imports](https://github.com/asottile/reorder_python_imports) 主要可以通过 `--add-import` 为每次修改的文件增加从 `__future__` 模块引入一些特性.
- [pyupgrade](https://github.com/asottile/pyupgrade) 在提交时自动修正一些不兼容的地方
- 我对 pylint 简单的包装[pre-commit-pylint](https://github.com/coldnight/pre-commit-pylint)


最后贴上我们项目所使用的 `.pre-commit-config` 文件

```yaml
-   repo: git@github.com:pre-commit/pre-commit-hooks.git
    sha: v0.8.0
    hooks:
    -   id: flake8
    -   id: check-docstring-first
    -   id: debug-statements
-   repo: https://github.com/asottile/reorder_python_imports
    sha: v0.3.5
    hooks:
    -   id: reorder-python-imports
        language_version: python2.7
        args:
        - --separate-relative
        - --separate-from-import
        - --add-import
        - from __future__ import absolute_import
        - --add-import
        - from __future__ import division
        - --add-import
        - from __future__ import print_function
        - --add-import
        - from __future__ import unicode_literals
        - --remove-import
        - from __future__ import with_statement
-   repo: https://github.com/asottile/pyupgrade
    sha: v1.1.1
    hooks:
    -   id: pyupgrade
-   repo: git@github.com:coldnight/pre-commit-pylint.git
    sha: 630e2662aabf3236fc62460b163d613c4bd1cfbc
    hooks:
    -   id: pylint-py3k
    -   id: pylint-score-limit
        args:
        - --limit=8.5
        - --rcfile=./.pylintrc
        additional_dependencies:
        - enum34; python_version<='3.4'
        - mock
        - coverage
```

## 后记

目前我们已经完成了公司最大的一个项目的 Python 3 兼容, 并于今天在测试环境使用 Python 3 运行.

## 参考

- (官方指南)[https://docs.python.org/3/howto/pyporting.html]
- [Instagram 在 PyCon 2017 的演讲摘要](http://www.zlovezl.cn/articles/instagram-pycon-2017/)
