Title: Linux 下 Python 实现按任意键退出
Tags: Python, Linux, 任意键, 退出, 按
Category: Python
Date: 2013-08-20 09:54

初学Python时在总想实现一个按任意键继续/退出的程序(受.bat毒害), 奈何一直写不出来, 最近学习Unix C时发现可以通过`termios.h`库来实现, 尝试一下发现Python也有这个库, 所以终于写出一个这样的程序. 下面是代码:
```python
#!/usr/bin/env python
# -*- coding:utf-8 -*-
import os
import sys
import termios


def press_any_key_exit(msg):
    # 获取标准输入的描述符
    fd = sys.stdin.fileno()

    # 获取标准输入(终端)的设置
    old_ttyinfo = termios.tcgetattr(fd)

    # 配置终端
    new_ttyinfo = old_ttyinfo[:]

    # 使用非规范模式(索引3是c_lflag 也就是本地模式)
    new_ttyinfo[3] &= ~termios.ICANON
    # 关闭回显(输入不会被显示)
    new_ttyinfo[3] &= ~termios.ECHO

    # 输出信息
    sys.stdout.write(msg)
    sys.stdout.flush()
    # 使设置生效
    termios.tcsetattr(fd, termios.TCSANOW, new_ttyinfo)
    # 从终端读取
    os.read(fd, 7)

    # 还原终端设置
    termios.tcsetattr(fd, termios.TCSANOW, old_ttyinfo)


if __name__ == "__main__":
    press_any_key_exit("按任意键继续...")
    press_any_key_exit("按任意键退出...")
```

其他关于`termios`的信息可以参考Linux手册:
```bash
man 3 termios
```

另补充一下*nix终端的三种模式(摘自<Unix\-Linux编程实践教程\>)
### 规范模式
规范模式, 也被成为cooked模式, 是用户常见的模式.驱动程序输入的字符保存在缓冲区, 并且仅在接收到回车键时才将这些缓冲的字符发送到程序.缓冲数据使驱动程序可以实现最基本的编辑功能, 被指派这些功能的特定键在驱动程序里设置, 可以通过命令stty或系统调用tcsetattr来修改

### 非规范模式
当缓冲和编辑功能被关闭时, 连接被成为非规范模式.终端处理器仍旧进行特定的字符处理, 例如处理Ctrl-C及换行符之间的转换, 但是编辑键将没有意义, 因此相应的输入被视为常规的数据输入
程序需要自己实现编辑功能

### raw模式
当所有处理都被关闭后, 驱动程序将输入直接传递给程序, 连接被成为raw模式.
