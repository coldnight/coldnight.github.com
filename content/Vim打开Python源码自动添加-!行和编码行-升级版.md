Title: Vim打开Python源码自动添加#!行和编码行 升级版
Tags: 自动,编码信息,添加,vim,shbang line,python
Category: Vim
Date: 2012-12-13 14:31
之前给大家分享过一个打开Python源代码时自动添加#!行和编码行来避免一些重复的工作,那个是因为需要大量编写时临时的解决方案,后来使用中会出现一些问题,比如查看别人源码时也会更改一些东西,从而造成git不必要的更新和手动删除的额外动作,所以又写了一个,只是在文件是新打开文件时或者空文件才自动添加的方法,同时再打开python源文件将这个方法绑定要F4上可以手动添加,并会判断是否有这两行,如果有则不执行动作,同时也添加了一些辅助性注释, 比如 作者/邮箱/创建日期和描述,代码实现如下:
```vim
"Python 注释
function InsertPythonComment()
    exe 'normal'.1.'G'
    let line = getline('.')
    if line =~ '^#!.*$' || line =~ '^#.*coding:.*$'
        return
    endif
    normal O
    call setline('.', '#!/usr/bin/env python')
    normal o
    call setline('.', '# -*- coding:utf-8 -*-')
    normal o
    call setline('.', '#')
    normal o
    call setline('.', '#   Author  :   '.g:python_author)
    normal o
    call setline('.', '#   E-mail  :   '.g:python_email)
    normal o
    call setline('.', '#   Date    :   '.strftime("%y/%m/%d %H:%M:%S"))
    normal o
    call setline('.', '#   Desc    :   ')
    normal o
    call setline('.', '#')
    normal o
    call cursor(7, 17)
endfunction
function InsertCommentWhenOpen()
    if a:lastline == 1 && !getline('.')
        call InsertPythonComment()
    end
endfunc
au FileType python :%call InsertCommentWhenOpen()
au FileType python map <F4> :call InsertPythonComment()<cr>
```
将上面的代码放到你的vimrc中,同时在vimrc添加:
```vim
let g:python_author = 'cold'               # 姓名
let g:python_email  = 'wh_linux@126.com'   # 邮箱
```
这样在每次打开空的python源文件时就会添加这些注释信息,并可以在非空没有这些注释的情况下按F4添加,配置玩后,打开空的python源文件效果如下:
![好看的statusline](/upload/VimPythonComment1.png)
