Title: Awesome+tmux+gnomeDo打造高效Linux桌面环境
Tags: 高效,桌面,Tmux,Linux,gnome do,awesome
Category: Linux
Date: 2012-12-04 13:21
## 引言
近期一直在Linux下工作,使用Ubuntu 11.10,经过一段时间的使用和磨合,终于打造出一套适合自己的高效Linux桌面环境,之前也在博客中零散的写了几篇文章分享,在此做一番总结.

首先先放出桌面截图
![Awesome 桌面截图](/upload/MyDesktop.png)

## Awesome
使用Ubuntu 11.10不习惯默认搭载的Unity,Gnome 3也不尽人如意,也使用xfce/openbox,但使用都不是很好,没有Windows的体验好,然后接触了Awesome,Awesome是一款平铺式窗体管理器,Awesome会去除窗口的标题栏等.会使窗口尽量小的占用桌面空间,而且大部分窗口操作都可以通过键盘来进行操作,免除了各位身为键盘高手的码农们频繁拿鼠标的烦恼.

### 安装
Awesome Ubuntu下安装十分简单:
```bash
sudo apt-get install awesome
```
### 配置
#### 拷贝配置文件
Awesome 的配置文件使用lua脚本,所以如果你会lua配置起来会得心应手,我们先拷贝一个基础配置文件,然后在这个基础上进行更改:
```bash
cp /etc/xdg/awesome/rc.lua ~/.config/awesome #  ~/.confg下如没有awesome则手动创建
```

#### 配置自动启程序
使用awesome之后之前设置的自动启动就会失效,因为Awesome启动是通过配置文件控制的,在配置文件(~/.config/awesome/rc.lua)加上如下内容可以配置自启动程序:
```lua
autorun = true
autorunApps = 
{ 
    "pidgin",
    "fcitx",
    "dbus-launch gnome-do",
    "guake",
    "/opt/qq2012/wineapp/qq/qq.sh",
}

if autorun then
    for app = 1, #autorunApps do
        awful.util.spawn_with_shell(autorunApps[app])
    end
end
```
 在autorunApps中添加要自动启动程序的命令即可在登录时启动相应的程序

#### 使用gnome桌面元素
awesome仅仅是一款窗体管理器,如果不进行相应配置awesome会使用默认的x window元素,非常丑陋我们来配置awesome使用GNOME桌面管理:
在宿主目录创建.xinitrc,添加如下内容:
```bash
#!/bin/sh
gnome-settings-daemon &     # 使用gnome桌面元素
nm-applet --sm-disable &    # 托盘栏添加网卡图标
exec awesome
```
如果不生效,则执行下面命令:
```bash
ln -sf ~/.xinitrc ~/.xprofile
```
### 美化
可以自己编写lua脚本对Awesome进行美化,当然还有很多已经写好的配置,我们叫他为主题
awesome的所有主题可以在 https://github.com/mikar/awesome-themes 上下载.
我们可以使用git将主题整体克隆下来
```bash
git clone git://github.com/mikar/awesome-themes.git ~/.config/awesome/themes
```
然后修改~/.config/awesome/rc.lua的beautiful.init()的参数改为主题的路径:
```lua
beautiful.init("/your/home/path/.config/awesome/themes/bamboo/theme.lua")
```
如果背景图片无法显示,安装feh:
```bash
sudo apt-get install feh
```
### 使用
配置完成后注销使用Awesome会话登录,Awesome使用Win键作为主键,几个常用的为:
```
Win+num 可以在多个桌面切换
Win+Ctrl+r 可以重启Awesome,
Win+k/j可以切换窗口(类Vim操作),
Win+m/n可以最大/小化窗口,
Win+Shift+num 可以将当前窗口发送到其他桌面.
Win+Shift+C可以关闭当前窗口,
Win+Space可以切换布局,Awesome有多种布局,这里不作介绍,大家可以自己稍作尝试.
```
下面给出完整的快捷键:
```
Mod4 + Enter           打开终端                                               
Mod4 + r               执行程序或命令
Mod4 + w               打开Awesome主菜单（鼠标右键关闭）                                                   
Mod4 + Shift + c       关闭当前窗口或应用                                     
Mod4 + Control + r     重启Awesome                                            
Mod4 + Shift + q       退出Awesome

Mod4 + j               切换到下一个窗口                                       
Mod4 + k               切换到前一个窗口                                       
Mod4 + Left            查看前一个tag                                          
Mod4 + Right           查看后一个tag                                          
Mod4 + 1-9             切换到tag 1-9                                          

Mod4 + Control + j     切换到下一个屏幕                                       
Mod4 + Control + k     切换到上一个屏幕                                       
Mod4 + Shift + j       当前窗口和前一个窗口互换位置                           
Mod4 + Shift + k       当前窗口和后一个窗口互换位置                           
Mod4 + h               把主区域的宽度增大5%                                   
Mod4 + l               把主区域的宽度减少5%

Mod4 + m               最大化窗口
Mod4 + n               最小化窗口

Mod4 + Shift + h       增加主区域窗口的数量                                   
Mod4 + Shift + l       减少主区域窗口的数量                                   
Mod4 + Space           切换窗口布局                                           
Mod4 + Shift + space   把当前tag更换为前一种布局                              
Mod4 + Control + space 切换当前窗口是否为浮动                                 

Mod4 + Shift + i       显示当前窗口的Class和instance，这在写脚本的时候尤其有用

Mod4 + Shift + r       重绘当前窗口                                           
Mod4 + t               标记窗口（可标记多个）                                 
Mod4 + Shift + F1~F9   把标记的窗口移动到第1~9个标记上                        

Ctrl + Mod4 + 1~9      把当前桌面和1~9桌面是显示                              
Mod4 + 1~9             恢复                                                   

Mod4 + Esc             快速切换到上一个桌面
```
更加详尽的介绍和配置请参见http://josephpan.net/blog/?p=992

## Tmux
tmux是一款替代screen的产品,除了拥有screen的基本功能外,还有窗口分隔,多人共享等功能.尤其是窗口分隔功能异常突出.同时tmux简化了很多快捷键.还支持vi/emacs风格的快捷键绑定

### 安装
```bash
sudo apt-get install tmux
```

### 配置
tmux 使用~/.tmux.conf作为配置文件,所以我们可以将配置添加到这个文件中,网上很多资料都将前缀键绑定到Ctrl+a,但我喜欢使用Ctrl+a跳到行首,所以我还是使用默认的前缀键Ctrl+b.下面是我的配置文件:
```conf
set -g default-terminal "screen-256color" 
set -g display-time 3000                   
set -g escape-time 0
set -g history-limit 65535                 
set -g base-index 1                        
setw -g utf8 on                           

# split window
unbind '"'
bind - splitw -v # vertical split (prefix -)
unbind %
bind | splitw -h # horizontal split (prefix |)

# select pane
bind k selectp -U # above (prefix k)
bind j selectp -D # below (prefix j)
bind h selectp -L # left (prefix h)
bind l selectp -R # right (prefix l)

# resize pane
bind -r ^k resizep -U 5 # upward (prefix Ctrl+k)
bind -r ^j resizep -D 5 # downward (prefix Ctrl+j)
bind -r ^h resizep -L 5 # to the left (prefix Ctrl+h)
bind -r ^l resizep -R 5 # to the right (prefix Ctrl+l)

# swap pane
bind ^u swapp -U # swap with the previous pane (prefix Ctrl+u)
bind ^d swapp -D # swap with the next pane (prefix Ctrl+d)

# misc
bind e lastp  # select the last pane (prefix e)
bind ^e last  # select the last window (prefix Ctrl+e)
bind q killp  # kill pane (prefix q)
bind ^q killw # kill window (prefix Ctrl+q)

# copy mode
bind Escape copy-mode             # enter copy mode (prefix Escape)
bind ^p pasteb                    # paste buffer (prefix Ctrl+p)
bind -t vi-copy v begin-selection # select (v)
bind -t vi-copy y copy-selection  # copy (y)

# zoom pane <-> window 
# see also: http://tmux.svn.sourceforge.net/viewvc/tmux/trunk/examples/tmux-zoom.sh
bind ^z run "tmux-zoom"

# app
bind ! splitw htop                                     # htop (prefix !)
bind m command-prompt "splitw 'exec man %%'"           # man (prefix m)
bind @ command-prompt "splitw 'exec perldoc -t -f %%'" # perl func (prefix @)
bind * command-prompt "splitw 'exec perldoc -t -v %%'" # perl var (prefix *)
bind % command-prompt "splitw 'exec perldoc -t %%'"    # perl doc (prefix %)
bind / command-prompt "splitw 'exec ri %%'"            # ruby doc (prefix /)

# reload config (prefix r)
bind r source ~/.tmux.conf \; display "Configuration reloaded!"

setw -g mode-keys vi
setw -g automatic-rename off

#-- colorscheme --#
# see also: https://github.com/daethorian/conf-tmux/blob/master/colors/zenburn.conf

# modes
setw -g clock-mode-colour colour223
setw -g mode-attr bold
setw -g mode-fg colour223
setw -g mode-bg colour235

# panes
set -g pane-border-bg default
set -g pane-border-fg colour234
set -g pane-active-border-bg default 
set -g pane-active-border-fg green

# messages
set -g message-attr bold
set -g message-fg colour223
set -g message-bg default

#-- statusbar --#
set -g status-utf8 on
set -g status-interval 1
set -g status-keys vi
set -g status-justify left
set -g display-time 3000
set -g status-bg default
set -g status-fg white
set-window-option -g window-status-current-attr default
set-window-option -g window-status-current-fg red
set-window-option -g window-status-current-bg default
set -g status-left-length 15
set -g status-right-length 55
#set -g status-left "#[fg=white,bg=blue] > #I #W < #[default] |" # 0:bash
set -g status-left "#[fg=white,bg=default] > #S < #[default] |" # session-name
set -g status-right "#[fg=red,bright][ #[fg=cyan]#H #[fg=red]]#[default] #[fg=yellow,bright]- %Y.%m.%d #[fg=green]%H:%M #[default]#[fg=magenta,bright](load: #(cat /proc/loadavg | cut -d \" \" -f 1,2,3))#[default]"
```

### 使用
我使用terminator配合tmux使用,更改terminator的配置(~/.config/terminator/conf)
```conf
[profiles]
  [[default]]
    login_shell = True
    use_custom_command = True   # 允许自定义命令
    custom_command = tmux -2     # 打开终端时执行tmux(-2 是强制终端使用256颜色)   
    background_type = transparent
    scrollbar_position = hidden
    foreground_color = "#ffffff"
    show_titlebar = False
    background_darkness = 0.2
```

### 常用快捷键:
```
Ctrl+b - 创建一个水平分隔 
Ctrl+b | 创建一个垂直分隔
Ctrl+b j/k/h/l 上下左右切换分割窗
Ctrl+b Ctrl+j/k/h/l 向上下左右扩展分割窗大小
Ctrl+b c 创建一个窗口
Ctrl+b , 重命名窗口
Ctrl+b n 切换到下一个窗口
Ctrl+b p 切换到前一个窗口
```

## Gnome-Do
Gnome Do 之前博客里有介绍请看[这里](/2012/9/1/Linux桌面高效工作----使用Gnome-DO/),由于Gonme Do使用Win+Space快捷键已经被Awesome占用,所以为了正常使用Gnome Do 我将Gnome Do的快捷键绑定到`Win+G`.
