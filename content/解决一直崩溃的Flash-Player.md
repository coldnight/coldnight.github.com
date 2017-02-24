title: 解决一直崩溃的 Adobe Flash Player
date: 2014-03-12
category: Linux
tags: archlinux, flash player, crash, 解决

1月份的时候决定从 Ubuntu 换到 Archlinux, 换完之后 Flash Player 就一直没正常过.
一打开视屏就 crash.  从那之后就一直用手机看视屏, 很别扭, 放着大屏不用一直盯着手机看看.
今天下定决心找找原因. google 了一阵也没有啥结果, 所以我决定卸载现有的, 手动安装一个试试.
```
$ sudo pacman -R flashplugin
```
结果卸载的时候输出段信息:
```
warning: /etc/adobe/mms.cfg saved as /etc/adobe/mms.cfg.pacsave
```
然后我看了下文件内容
```bash
$ cat /etc/adobe/mms.cfg.pacsave
#Hardware video decoding
EnableLinuxHWVideoDecode=1
```
瞬间觉得可能是这个选项引起的, 所以我又装上了 flashplugin
```
$ sudo pacman -S flashplugin
```
同样看到一段信息
```
 >> 
 >> If you have an NVIDIA card that supports libvdpau or Broadcom Crystal HD chips,
 >> uncomment EnableLinuxHWVideoDecode=1 from /etc/adobe/mms.cfg.
 >> If you run into problems, please contact nVidia or Broadcom along with your system config info / driver version.
 >> 
Optional dependencies for flashplugin
    libvdpau: GPU acceleration on Nvidia card [installed]
```
然后我就知道了为什么会出现那个选项打开的情况了, 一开始安装 Archlinux 的时候我误以为我的显卡是 NVIDIA 的,
然后就安装了 NVIDIA 的驱动, 后来仔细一看原来是集成的 Intel 显卡XD, 估计在一开始安装 flashplugin
的时候自动开启了. 然后换了显卡驱动我也不清楚.现在看新的配置文件 EnableLinuxHWVIdeoDecode=1 是注释掉的.

接下来就是验证喽, 打开一个视屏, 果然不 crash 了.

然后开启这个选项, 果然预料中的接着 crash.
