title: 使用 Pygments 对 Vimwiki 进行代码高亮
tags: vim, vimwiki, pygments, python, highlight
category: Vim
date: 2013-12-27
Vimwiki 推荐的代码高亮机制是通过一个 JavaScript 插件来完成的, 那样需要加载很多 js,
所以不想使用, 比较倾向使用 Pygments 在 Vimwiki 生成 HTML 的时候对代码进行高亮.

# 尝试
## 使用 custom\_wiki2html 选项
仔细的看了 Vimwiki 的帮助文档, 发现有一个 custom\_wiki2html 
(`:h vimwiki-option-custom_wiki2html`) 的选项可以指定自己
的脚本来处理 wiki2html, 尝试了一下, 发现这个脚本是在生成 HTML 之前调用,
而且如果对 wiki 文件处理之后无法替换回原来的内容(后来发现这个仅仅是对使用 Markdown
语法作为 Wiki 语法设定的), 所以放弃了.

## Fork 仓库, 更改代码
后来想想既然原生的没有解决办法, 所以就干脆在 github 上 fork 了仓库
clone 到本地进行修改, 通过直接 hack 代码在 Vimwiki 处理之前对代码进行高亮.


### 思路
粗略的看了下代码, Vimwiki 是将文件读入, 然后逐行处理, 看来只能在文件读取之后
对内容做一些操作.

Vimscript 的 readfile 返回一个列表, 每一个元素代表一行, 编写一个函数处理这个
列表, 并返回, 在 autoload/vimwiki/html.vim 里的 第 1350 行找到
`vimwiki#html#CustomWiki2HTML` 函数, 并在其上面添加一个函数
```vim
function! s:highlight_code_with_pygments(lsource) "{{{
  if !has('python')
    return a:lsource
  endif
  let s:lsource = deepcopy(a:lsource)
  let s:content = ''
python <<EOF
def handle():
  import vim

  import re
  import os

  try:
    import pygments
  except ImportError:
    vim.command("echoerr 'Cannot import pygments library, please install it.'")
    return

  from pygments.lexers import get_lexer_by_name
  from pygments.formatters import HtmlFormatter
  from pygments import highlight
  from pygments.util import ClassNotFound

  CODE_RE = re.compile(r'\n({{{(\w*?)\s(.*?)\s}}})', re.M|re.U|re.S)

  data = vim.eval("s:lsource")
  content = "\n".join(data)
  new = False
  css_class = vim.eval("VimwikiGet('pygments_class')")
  for source, lang_type, code in CODE_RE.findall(content):
    lang_type = lang_type or "text"
    try:
      lexer = get_lexer_by_name(lang_type)
    except ClassNotFound:
      lexer = get_lexer_by_name("text")

    formatter = HtmlFormatter(encoding="utf8", cssclass=css_class,
                              noclasses=False, style="default",
                              linenos = None, nowrap = True)

    hcode = highlight(code, lexer, formatter)
    content = content.replace(source, hcode)
    if new is False:
      new = True

  if new:
    vim.command("let s:content='%s'" % content.replace("'", "\'"))

handle()

EOF

  if s:content != ''
    let s:lsource = split(s:content, '\n')
  endif

  return s:lsource
endfunction "}}}
```
然后在 `vimwiki#html#Wiki2HTML` 函数内读取文件的下面调用此函数
```vim
...
    let lsource = readfile(wikifile)

    let lsource = s:highlight_code_with_pygments(lsource)
...

```
当然需要还需要拷贝 css 文件, 修改模板等, 这里不一一详述

### 结果
好吧, 确实工作了, 但是新的问题出来了, 高亮后替换成HTML的内容会被 Vimwiki 第二次处理
所以 HTML 的格式全乱了,  CSS 样式也肯定不行了. 找了一下没有办法能避免 Vimwiki 不
处理一段内容(为什么已经是 HTMl 了 Vimwiki 还要处理呢)

上述方法不行, 那只能再想想办法了

### 使用特殊的注释: %nowiki
为了解决上面重复处理已转化成 HTML 内容的问题, 我添加 %nowiki 语法,
以 %nowiki 这个开头的行将不处理. 然后将高亮的代码替换成 %nowiki 开始的行,
将上面 Python 相关代码加上如下内容:
```python
...
hcode = highlight(code, lexer, formatter)

# 添加以下语句
hcode = "%nowiki" + '\n%nowiki'.join(hcode.split("\n"))

content = content.replace(source, hcode)
..
```
然后找到 `s:parse_line` 函数, 在处理 toc 的下面添加
```vim
  " toc -- placeholder "{{{
  if !processed
    if line =~ '^\s*%toc'
      let processed = 1
      let param = matchstr(line, '^\s*%toc\s\zs.*')
      let state.placeholder = ['toc', param]
    endif
  endif
  "}}}
  
  " 添加处理 nowiki
  " nowiki "{{{
  if !processed
    if line=~ '^\s*%nowiki'
      let processed = 1
      call add(res_lines, substitute(line, '^\s*%nowiki', '', ''))
    endif
  endif "}}}

```

然后找到 `s:safe_html` 函数, 添加如下语句
```vim
function! s:safe_html(line) "{{{
  " 不处理 %nowiki 
  if a:line=~ '^\s*%nowiki'
    return a:line
  endif

  ...
endfunction
```

大功告成, 这样就可以正常的使用 pygments 进行高亮代码

# 成果
修改后的代码放在了 github 上, 在 [这里](https://github.com/coldnight/vimwiki), 
有需要的可以安装, 但这个更改仅仅是对我个人使用, 可能不会跟随主线程版本的更新.

当然我添加了一些选项来开启/关闭使用pygments, 安装后在 Vim 中执行 `:h vimwiki-option-use_pygments`

大家通过代码也看到了, 要使用改变之后的插件 Vim 需要编译 +python 并且需要安装 pygments Python 库.
