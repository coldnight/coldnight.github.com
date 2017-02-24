Title: Vim配置系列(一) ---- 插件管理
Tags: 配置,系列,管理,插件,Vundle,vim,Bundle
Category: Vim
Date: 2012-12-14 08:57
最近对Vim进行了一番较大的配置变动,所以就想写出一个系列来将配置过程分享下来,供需要的朋友参考.我们之前配置Vim插件是一大助力,可以帮助我们做一些比较cool或这比较实用的功能,但是我之前都是直接搜索插件然后下载下来,手动拷贝到相应的插件,这种感觉肯定是不爽,不管是Linux还是Python/Ruby都有一套自己的包管理器,可以比较智能的搜索/安装/升级/卸载包.Vim也有类似功能的插件Vundle,他是一款Vim插件管理器,依赖于git,git是一款非常棒的VCS这里不做介绍,有兴趣的可以了解一下.Vundle可以根据配置文件的github或其他git的路径自行安装/升级插件.下面我们来介绍如何安装:
首先在你的~/.vim下或者$VIM/vimfiles($VIM是vim的安装路径)创建bundle目录
```bash
mkdir ~/.vim/bundle
```
然后使用git克隆Vundle项目:
```bash
git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
```
然后在你的.vimrc里添加下面内容:
```vim
set nocompatible
filetype off                              " 先关闭文件类型
set rtp+=~/.vim/bundle/vundle             " 将vundle路径添加到插件vim路径
call vundle#rc()                          " 执行Vundle初始化
Bundle 'gmarik/vundle'                    " 将Vundle加入到bundle

filetype indent plugin on                 " 安装完后打开文件类型
```
如需加上插件则在Bundle 'gmarik/vundle'后加上相应的Bundle,如果是github则可以只输入后面的相对路径,如果是其他git则需输入全部url,下面给出一个完整的例子:
```vim
set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle
call vundle#rc()
Bundle 'gmarik/vundle'
"Bundle "MarcWeber/vim-addon-mw-utils"
"Bundle "tomtom/tlib_vim"
"Bundle "honza/snipmate-snippets"
"Bundle "garbas/vim-snipmate"
Bundle "Shougo/neocomplcache"
Bundle "Lokaltog/vim-powerline"
Bundle "drakeguan/vim-vcscommand"
Bundle "scrooloose/nerdtree"
Bundle "pix/vim-taglist"
Bundle "nathanaelkane/vim-indent-guides"
Bundle "clones/vim-cecutil"
Bundle "tpope/vim-fugitive"
"Bundle "c9s/bufexplorer"
Bundle "jnwhiteh/vim-golang"
Bundle "kevinw/pyflakes-vim"
Bundle "mbriggs/mark.vim"
"Bundle "vim-scripts/TabBar"
Bundle "vim-scripts/DrawIt"
Bundle "vim-scripts/calendar.vim--Matsumoto"
Bundle "vim-scripts/Python-mode-klen"
"Bundle "vim-scripts/pydoc.vim"
Bundle "vim-scripts/VOoM"
Bundle "vim-scripts/qiushibaike"
Bundle "vim-scripts/AuthorInfo"
Bundle "vim-scripts/javacomplete"
Bundle "vim-scripts/javaDoc.vim"
Bundle "drmingdrmer/xptemplate.git"
Bundle "vim-scripts/Java-Syntax-and-Folding"

filetype indent plugin on
```
以上是我的Vundle配置,再后面的文章会对其中一些做出介绍:

配置完后保存使用Vim打开任意文档执行
```vim
:BundleInstall
```
如果网络通畅Bundle会逐个安装每个插件.

同时还有:BundleSearch用于搜索插件,非常方便.
