Title: Python 入门指南
Date: 2014-05-23 14:29
Tags: Python, 入门, 指南, 新手, 错误, 书籍, 工具
Category: Python

# 引子
经常能在 Python 群里看到很多新人在问一些非常基础的问题, 基本每天都在重复的问这些问题,
在这里就总结一下这些问题.

首先声明, 本文不打算教会你 Python, 本文力图陈列一些新手容易遇到的问题, 并企图教会你
如何学习 Python, 在遇到问题的时候如何提问.


# 关于版本
学习 Python 的第一步需要选择版本, Python 3.x 和 2.x 的断层较大, 3.x 不向后兼容 2.x.
Python 现在主流应该还是 Python 2.7, Python 2.7 将会是 Python 2.x 的最后一个版本, 并且
会支持到 2020 年. 但是 Python 3 也在健康发展, 会慢慢取代 Python 2.7 成为主流版本.

## 选择版本

在你要开始学习 Python 之前就是要确定要学习的版本,
不管你是选择 2 还是 3, 虽然有差别, 但不是很大, 等你熟悉了之后就可以触类旁通.
不必太害怕选择了一个版本到时候无法兼顾另一个版本.

如果你无法确定要学习的版本,可以根据以下方法来确定要学习的版本.

### 手上已有的教程
如果你手上已经有了一本关于 Python 基础的书, 那么书的开头应该会交代 Python 版本.
那就根据这个教程选择要学习的版本.

### 要上手的项目
如果你已经有一个 Python 项目等着你去上手, 那么先了解项目需要什么版本. 然后根据
需要的版本找支持对应版本的基础书籍.

### 直接选择 Python 3
如果你没有以上的负担, 那么推荐你直接学习 Python 3, 但是你要找到一本支持 Python 3
的入门书籍, 不然你前期你会发现所有的都是错的, 会直接打消你的自信心.

Python 3 的底层全部用 unicode 实现, 所以不会遇到 Python2 烦人的 `UnicodeDecodeError`
类似的异常, 关于这个后面会讲到.

## 版本差异
Python 官网已经有详细的版本差异([这里](https://docs.python.org/3/whatsnew/3.0.html))

这里简单列出几个主要差异

### print 的改变
在 Python2 里 print 是一个语句, 用以下方式输出
```python
print 'Hello world!'
```

在 Python 3 里 print 变成了一个 函数
```python
print('Hello world')
```

### 输入函数的改变
在 Python 2 里用 `raw_input` 函数获取输入
```python
raw_input("Enter your name: ")
```

在 Python 3 里用 `input` 函数代替 
```python
input("Enter your name: ")
```


当然还有很多, 这里不一一列举, 如果你以后对 Python 有了一定了解, 可以看看
[这篇](https://www.ibm.com/developerworks/cn/linux/l-python3-1/)文章

## 多版本共存
Python 是可以多个版本共存的, 如果可以你可以同时安装 Python 2 和 3, 自己
动手亲自比较一下.

## 代码兼容
Python 2 和 3 是可以通过一些技巧来实现兼容的, 这点超出了本文讨论的范畴, 
如想了解可以上网搜索.

# 关于 Python Shell
Python Shell 就是你在命令行下运行 `python` 指令后出来的一个交互式 shell,
或者运行 Windows 下的 `IDLE` 出来的窗口叫做 Python Shell, Python Shell 
提供一种 "所见即所得" 的方式来运行 Python 语句, 这将是你学习 Python
的一种重要工具.

## 区分 Python Shell 和 命令
但是我见到很多人问像下面那样运行脚本为什么会出错
```python
>>> python script.py
```
这样是错误的, Python Shell 是运行 Python 语句的, 而 `python script.py` 是
一条命令, 意为运行 `script.py` 这个文件里的 Python 语句.

我们真正要做的是在命令行下执行这个命令, 所谓命令行就是 Windows 下
Win+R 输入 cmd 回车弹出的窗口.

如果你在命令行下运行失败请上网搜索了解关于 `PATH` 的相关知识.

# 关于 Python 2
如果你选择了 Python 2, 那么就有需要面对一些问题.

## 中文字符
如果你的 Python 源码文件里出现了中文字符, 你就会发现无法运行出现
```
SyntaxError: Non-ASCII character '\xe5' in file xx.py on line 8, but no encoding declared; see http://www.python.org/peps/pep-0263.html for details
```
你只需要在文件的最上面加上一行
```python
#-*- coding: utf-8 -*-
```
即可

## UnicodeDecodeError 异常
如果你不幸遇到了这个错误, 那么一般是因为字符串连接引起的, 比如下面这样的代码
```python
>>> '中国' + u'a'
```
如果你学过了基础你就会知道 `u''` 包围的字符串是 `unicode`, Python 2 里有两种类型的
字符串 `str` 和 `unicode`, 上面的 `'中国'` 就是 `str` 类型, `u'a'` 就是 `unicode`
类型.

如果这两种类型的字符串相连, `str` 类型的字符串会向 `unicode` 做隐式转化, 而隐式转换
默认的编码是 `ascii`, 明显 `ascii` 编码不可能包含任何汉字, 所以就会抛出这个异常.

如果上面反过来就不会抛出异常, 因为 `ascii` 里包含 `a` 这个字符
```python
>>> u'中国' + 'a'
```

在文件中使用统一的类型的字符串可以规避这个问题, 要么都使用 `u''` 包围的字符串, 
要么都使用 `''` 包围的字符串

当然上面单引号是可以换成双引号的.

# 关于 Traceback
Python 在程序出错的时候会向终端打印一串略长的信息叫做 `Traceback`, 像下面这样:
```python
Traceback (most recent call last):
  File "test.py", line 16, in <module>
    main()
  File "test.py", line 14, in main
    test()
  File "test.py", line 10, in test
    '中国' + u'a'
UnicodeDecodeError: 'ascii' codec can't decode byte 0xe4 in position 0: ordinal not in range(128)
```
这一段信息很详细的描述了出错的地方和详细的调用信息, 当然还有错误描述.

看懂 Traceback 将会有助于你更好的学习 Python. 这段 Traceback 说明

在 test.py 的第 16 行 main 函数里, 调用了在 14 行的 test 函数, 
test 函数里文件的第 10 行的语句触发了 UnicodeDecodeError 异常.

简直太清晰了, 如果你觉得不清晰的话就怪我描述的不好吧.

# 关于第三方库
Python 有大量的第三方库, 并且有 `setuptools` 工具可以安装这些库, setuptools
提供了 `easy_install` 命令可以从网上自动下载并安装第三方库, 可以参见
[这里](https://pypi.python.org/pypi/setuptools#installation-instructions)

# 关于提问
如果你遇到了问题, 解决不了需要提问的时候, 请尽量的提供你的代码和详细的 Traceback.
代码直接发出来不是很好的方式, 请尽量贴到支持代码高亮的网站上, 并保持缩进.

推荐的下面两个贴代码的网站:

* [http://pastebin.com](http://pastebin.com)
* [http://paste.ubuntu.org.cn](http://paste.ubuntu.org.cn)

# 关于工具
## 编辑器
如果在学习初期并不推荐 IDE 作为开发工具, 使用文本编辑器可能有助于你的学习,
按照困难程度由低到高推荐下面几种文本编辑器:

* [ulipad](https://code.google.com/p/ulipad/)
* [notepad++](http://notepad-plus-plus.org/)
* [Sublime Text](http://www.sublimetext.com/)
* [Vim](http://vim.org) / [Emacs](http://www.gnu.org/software/emacs/)

如果你想要一款功能强大的 IDE 那么推荐你 [PyCharm](http://www.jetbrains.com/pycharm/)

## 其他工具

* [IPython](http://ipython.org) 一个 Python Shell 增强工具
* [virtualenv](http://virtualenv.readthedocs.org/en/latest/) 可以用它获取一个干净的 Python 开发环境

# 关于书籍
Python 基础书籍有很多: 《简明 Python 教程》， 《Python 核心编程》《Python 学习手册》《Python参考手册》
等都是很不错的入门书籍.

如果你已经掌握了 Python 基础, 想继续阅读深入 Python, 这里推荐两本 Python 进阶书籍:
《Python 高级编程》《Python 标准库》


# 关于代码规范
良好的代码规范可以让你的程序更加的简洁、美观和易读. Python 有自己的代码
规范, 可以参见 [PEP8](http://legacy.python.org/dev/peps/pep-0008/)

# 写在后面的话
零零散散的介绍了一些, 在此抛砖引玉欢迎大家补充. 编写 Python 是快乐的, 
希望本文可以对开始学 Python 的朋友有点帮助, 祝你们学习愉快.
