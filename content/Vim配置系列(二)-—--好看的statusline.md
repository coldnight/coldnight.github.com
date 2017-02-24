Title: Vim配置系列(二) —- 好看的statusline
Tags: 配置,系列,漂亮,插件,vim,statusline,Powerline
Category: Vim
Date: 2013-01-05 15:01
Vim是一款文本编辑器,但是这并不影响它有一个好看的外观,大家都知道Vim可以通过配色方案来改变Vim的外观,满足一些"好色之徒",之前大家可能也主意到截图中一个非常漂亮的statusline,这是通过Vim的一个Powerline的插件实现的.之前我们配置了Vundle的插件管理(传送门)我们可以用Vundle安装Powerline,在.vimrc(Windows可能是_vimrc)中添加:
```vim
Bundle "Lokaltog/vim-powerline"
```
然后重新打开vim执行
```vim
:BundleInstall
```
如果你和github畅通的话就会顺利安装插件,然后在.vimrc里添加
```vim
set laststatus=2
let g:Powerline_symbols='unicode'
```
如果gvim打开需要使用/path/to/your/bundle/vim-powerline/fontpatcher/fontpatcher给当前gvim使用的字体打上补丁(依赖需要fontforge和python)

然后重新打开vim你就会发现一个漂亮的statusline

![好看的statusline](/upload/VimPythonComment1.png)
