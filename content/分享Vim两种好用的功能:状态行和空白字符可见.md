Title: 分享Vim两种好用的功能:状态行和空白字符可见
Tags: 空白,字符,可见,vim,statusline
Category: Vim
Date: 2012-09-29 17:30
马上要放假了,没事折腾了一下Vim发现了两个非常棒的功能,一个是给Vim添加一个状态栏,一个是可以在编辑的时候显示空白,

我的状态栏显示了:
正在编辑的文件名,
选项
是Git显示git分支(需要fugitive插件)
文件类型
当前目录
当前字符的ASCII和16进制码
右边是当前光标所在行/列,文件的位置的百分比,和文件的长度
fugitive 可以在git获得:
```
git clone http://github.com/tpope/vim-fugitive.git
```
将plugin目录下的fugitive.vim复制到~/.vim/plugin下
在~/.vimrc添加如下内容
```vim
if has('statusline')
    set laststatus=2
    set statusline=%<%f\   " 文件名
    set statusline+=%w%h%m%r " 选项
    set statusline+=%{fugitive#statusline()} "Git
    set statusline+=\ [%{&ff}/%Y]            " filetype
    set statusline+=\ [%{getcwd()}]          " current dir
    set statusline+=\ [A=\%03.3b/H=\%02.2B] " ASCII / Hexadecimal value of char
    set statusline+=%=%-14.(%l,%c%V%)\ %p%%\ %L  " Right aligned file nav info
endif
```
要想在编辑时将空白可见可以在.vimrc中添加如下内容:
```vim
set listchars=tab:>-,trail:-,extends:#,nbsp:-
```
即可tab显示为>---,空格显示-,行尾的空白显示-,

设置完后非常的cool,上图一张:
![vim空白符](/upload/Screenshot-2012-09-29-173517.png)
