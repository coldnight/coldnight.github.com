Title: Python 重复安装包报错
Tags: python,包,重复,安装,卸载
Category: Python
Date: 2013-01-25 14:55
最近写程序用到argparse总是会报错
```
/usr/lib/python2.7/dist-packages/pygments/plugin.py:39: UserWarning: Module argparse was already imported from /usr/lib/python2.7/argparse.pyc, but /usr/local/lib/python2.7/dist-packages is being added to sys.path
```

一开始也没怎么在意,不影响什么,但是后来总是出来严重影响心情,google 了没啥明确的报错,后来仔细看报错信息,想起来以前可能庄过argparse这个包测试,但是没有删除
执行`ls /usr/local/lib/python2.7/dist-packages/`查看果然有一个argparse.

执行`sudo pip uninstall argparse`烦人的错误提示消失了.
