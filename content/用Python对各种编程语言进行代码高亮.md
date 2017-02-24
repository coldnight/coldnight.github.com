Title: 用Python对各种编程语言进行代码高亮
Tags: 高亮,代码,python,pygments
Category: Python
Date: 2012-09-29 15:11
做了一个在线代码高亮的项目,因为写的Gtalk群Bot需要这个功能支持,贴到第三方怕被人给封,所以干脆想自己写一个,强大的Python一如既往没让我失望,一个强大的Pygments模块可以对多种(很多)语言进行代码高亮

下面来介绍一下它:

首先安装很简单,使用easy_install来进行安装:
```bash
easy_install pygments
```
安装完后我们来使用,Python的简单不会让大家失望:
```python
from pygments.lexers import PythonLexver
from pygments.formatters import HtmlFormatter
from pygments import highlight

formatter = HtmlFormatter(encoding='utf-8', style = 'emacs', linenos = True)
code = highlight('print "hello, world"', PythonLexer(), formatter)

print code
```
##### 结果 ################
```
'<table class="highlighttable"><tr><td class="linenos"><div class="linenodiv"><pre>1</pre></div></td><td class="code"><div class="highlight"><pre><span class="k">print</span> <span class="s">&quot;hello, world&quot;</span>\n</pre></div>\n</td></tr></table>'
```
这样就简单的对代码进行了高亮,当然如果你做了上面操作,然后把内容输入到一个文件里查看,肯定大呼坑爹,因为根本没高亮,因为默认是不会输出css的 我们还要获取css加入到html中去:
```python
css = formatter.get_style_defs()
```
然后把css内容和上面的html一起写入到html文件就可以看到高亮的代码了(千万不要告诉我你不知道css应该放在什么位置)

欢迎大家加入到我们的gtalk群来讨论Python/vim/Linux 或者蛋疼的时候聊聊人生,有很多好玩的功能等着大家.
使用gtalk添加:clubot@vim-cn.com
