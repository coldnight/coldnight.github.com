Title: 用Vim为Python源码自动添加#!行和编码行
Tags: 自动,编码信息,添加,vim,shbang line,python
Category: Vim
Date: 2012-09-29 18:56
每次开始写Python打开文件第一件事就是写上`#!/usr/bin/env python和`编码之类的东西,

太多了,写烦就,写了一个打开Python自动填充的函数,将下面内容添加到`~/.vimrc`下即可每次打开如果没有上述行则会自动填充:
```vim
function InsertPythonHeader()
    let l1 = getline(1)
    let l2 = getline(2)
    if  match('\#!/', l1) == 0
        exec 1
        normal O
        call setline(1,'#!/usr/bin/env python')
    endif
    if match("\#", l2) == 0 && (match("-", l2)  != 2 ¦¦ (match("code", l2) != 2))
        exec 2
        normal O
        call setline(2,'#-*- coding:utf-8 -*-')
    endif
endfunction

au FileType python call InsertPythonHeader()
```
