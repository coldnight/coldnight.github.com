Title: Linux高效工作----平铺式窗体管理器Awesome
Tags: 高效,管理器,窗体,桌面,平铺式,Linux,awesome
Category: Linux
Date: 2012-10-25 10:23
在Linux桌面环境下开发,总想更高效的工作,我已经装备了Gnome Do和terminator,但是我还是觉得不够快我更加希望能解放右手(当然不是找个妹子戒撸,只是右手的鼠标),而且terminator在跑的东西过多的时候开多个terminator不太好管理,这时候一个词进入了我的眼睛平铺式窗体管理器,与传统窗体管理器不同的是平铺式窗体管理器的窗口不会重叠,窗口会被自动调整成正好铺满全屏的尺寸,也就是说无论开多少窗口都会把屏幕占满,如果想象力贫乏就装一个试试吧:

Awesome是一款运行在Unix和类Unix(Linux/FreeBSD)等系统上的一款平铺式窗体管理器,有占用资源小,易于管理和操作等等有点,这里不罗嗦这些说说安装,Ubuntu安装很简单
```bash
sudo apt-get install awesome
```
安装好后登出会话选择awesome登录,然后你是否茫然无知没办法工作了?先简单介绍下使用方法:

按`Win键+1~9`可以切换桌面,

没有菜单对吧 其实再右上角点一下就会出来一个菜单,打开程序会发现标题栏状态栏什么都木有了大大节省了桌面空间,可问题来了,怎么关闭啊不用担心

按 `Win键+Shift+C`可以关闭当前窗口

打开默认终端 `Win键+Enter`就可以打开终端

可以按住 `Win键+Shift+数字` 可以将当前窗口发送到相应的工作区

同样可以切换工作区的是 `Win+j/Win+k`

`Win+Space`可以切换当前工作区布局

是不是很高效!是不是解放了右手,问题来了,我的`gnome-do`无法用了,`win+space`被占了阿,好吧我把`Gnome-Do`的快捷键调成`Alt+Space`,

但是如何添加开机启动程序呢,很简单Awesome的配置文件是一个lua脚本(我不懂),可以在这个脚本里添加启动程序

创建配置文件:
```bash
cd ~/
mkdir .config/awesome/
cp /etc/xdg/awesome/rc.lua .config/awesome/
```
编辑配置文件在文件末尾添加:
```lua
autorun = true
autorunApps = 
{ 
    "pidgin",
    "thunderbird",
    "gnome-do",
    "guake",
    "/opt/qq2012/wineapp/qq/qq.sh",
}

if autorun then
    for app = 1, #autorunApps do
        awful.util.spawn_with_shell(autorunApps[app])
    end
end
```
很简单吧,应该看得出在哪里添加启动的程序吧,这里启动了`pidgin`,`雷鸟`,`gnome-do`,`guake`和`wine`的`qq2012`

还有很多强大的功能这里就不在介绍配置方法,可以自行google之
