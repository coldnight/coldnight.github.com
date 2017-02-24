推荐几款最近发现非常酷的Vim插件
###############################
:tags: Vim, 插件, 酷, 缩进对齐线, Git, surround, delimitMate
:date: 2013-10-18 13:14
:category: Vim
:author: cold


最近看一个github上的Vim配置, 发现了几款非常酷而且非常有用的Vim插件:

* delimitMate
  用于补全括号和引号

* vim-surround
  用于快速切换括号/引号或者标签

* GitGutter
  实时显示git更改

* Gitv
  查看Git详细提交日志(类似gitk)

* vim-commentary
  Vim批量注释工具, 可以注释多行和去除多行注释

* indentLine 
  更加美观的显示缩进对齐线

先放上录屏:

.. raw:: html

    <div style="width:800; height: 500">
        <script type="text/javascript" src="http://asciinema.org/a/5981.js" id="asciicast-5981" async></script>
    </div>


安装
====
上面插件可以通过 Vundle 来安装 (了解Vundle猛击 `这里 <http://www.linuxzen.com/vimpei-zhi-xi-lie-cha-jian-guan-li.html>`_), 下面是 ``.vimrc`` 的配置

.. code-block:: vim

    Bundle "Yggdroot/indentLine"
    Bundle "airblade/vim-gitgutter"
    Bundle "gregsexton/gitv"
    Bundle "tpope/vim-commentary"
    Bundle "tpope/vim-surround"
    Bundle "Raimondi/delimitMate"

然后重新打开 Vim, 执行 ``:BundleIntall`` 等待安装完成


配置使用
========
delimitMate和GitGutter安装完成不用任何配置即可使用, 下面我们先介绍 ``vim-surround`` 插件的使用

vim-surround
------------
这个插件可以快速的为字符串包围/改变或去除引号/括号或者HTML标签

为单个单词包围
^^^^^^^^^^^^^^
在命令模式下, 使用 ``ysiw`` + ``'/"/(/[/{`` 就可以为光标下的一个单词包围上 ``'/"/(/[/{`` 

比如 ``ysiw'`` 为光标下的单词包围上单引号, ``ysiw"`` 为光标下单词包围上双引号, 依此类推.

vim-surround 同时还支持包围html标签, 将光标放到某单词试试下面指令

.. code-block:: vim

    ysiw<p>
    ysiw<p class="meta">


包围一行
^^^^^^^^

``yssb`` 可以快速为一行包围圆括号,  ``yss`` + ``'/"/(/[/{`` 可以为正行快速包围相应的引号/括号

比如 ``yss"``  为一行包围双引号

更改包围
^^^^^^^^
``cs`` 指令可以更改包围, 比如 ``cs'"`` 是将单引号变成双引号, ``cs"(`` 是将双引号变成圆括号

vim-surround支持将括号或者引号变更为html标签, 试试下面命令

.. code-block:: vim

    cs'<p>

上面命令将单引号换成 ``<p>`` 标签

去除包围
^^^^^^^^
``ds`` 指令可以取出包围, 后面需跟包围的内容, ``ds"`` 是去除双引号包围, ``


indentLine
----------
这个插件安装成功后就会显示缩进对齐线, 我们仅仅在 ``.vimrc`` 里加一行来切换是否显示

.. code-block:: vim

    map <leader>il :IndentLinesToggle<CR>

这样我们就可以通过 ``<leader> il`` (我的leader映射的,)来切换是否显示对齐线


Gitv
----
Gitv 实现了可以用Vim来查看Git的详细提交信息, 只需要打开Vim 执行 ``:Gitv``

vim-commentary
--------------
这个插件可以快速注释与反注释多行内容, 但是它的注释符使用的是 ``commentstring``, 默认是 ``/* %s */``, 但这个值满足不了Python 和 Shell这样的语言,
在 ``.vimrc`` 添加如下内容

.. code-block:: vim

    autocmd FileType python,shell set commentstring=#\ %s                 " 设置Python注释字符
    autocmd FileType mako set cms=##\ %s


``Visual`` 模式下 ``gc`` 命令可以注释选中的行

普通模式下  ``gcc`` 指令可以快速注释一行

``gcu`` 可以撤销注释


最后
====
有什么没介绍到的大家可以看看帮助, 大家也可以围观这个强大Vim配置: `<https://github.com/liangxianzhe/dotvim>`_
