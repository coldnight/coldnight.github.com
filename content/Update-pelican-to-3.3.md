title: 将Pelican版本更新到3.3
tags: pelican3.3, 升级
category: Python
date: 2013-12-24
author: cold
summary:
    记录升级Pelican到3.3的过程和一些吐槽,

用Pelican有一段时间了, 由于重装了系统, 所以安装`Pelican`的最新版本,最新版本为 ``3.3``, 顺便也升级模板和插件.

`Pelican`特别喜欢更改配置文件,  而且错误提示非常烂, 完全不知道在说什么, 而且没有安装markdown包的话根本不提示, 仅仅就错误退出, 下面就这次升级越到的问题做个记录.

## Makefile

Pelican升级后会有很多莫名奇妙的问题, 首先之前的`Makefile`不能使用, 所以需要重新生成一份:

    pelican-quickstart .

当然上面操作会更改配置文件, 我用git管理, 所以很方便的`checkout`, 如果你的不是, 先备份下配置文件吧.

## ATOM Feed
然后`make html`的时候失败提示

    File /path/to/project/output/feeds/all.atom.xml is to be overwritten!

看了 [pelican](https://github.com/getpelican/pelican)上的`issue` 原来是更改了`FEED_ATOM`的配置项, 使用了 `FEED_ALL_ATOM`配置项, 将`FEED_ATOM`配置项改为`FEED_ALL_ATOM`配置即可

但是改完之后页面的ATOM FEED链接没有指向正确的地址, 查看模板文件, 发现模板还在引用`FEED_ATOM`, 改成`FEED_ALL_ATOM`即可.

## Markdown
如果莫名奇妙的错误, 没有错误信息, 也没有输出HTML, 那么可能就是`markdown`包没装, `Pelican`的错误提示真心无语.
    
    easy_install -U markdown

### Markdown 代码高亮
Markdown 是通过指定 MD\_EXTENSIONS 选项类配置代码高亮的之前配置这样就可以
```python
MD_EXTENSIONS =  (['codehilite', 'extra', 'fenced_code', 'tables', 'sane_lists'])
```
但是发现现在无法高亮代码, 查看了源码原来要手动指定高亮 css, 不然css会设置成
 codhilite 
 ```python
MD_EXTENSIONS =  (['codehilite(css_class=highlight)', 'extra',
                   'fenced_code', 'tables', 'sane_lists'])
 ```

## 静态文件
`Pelican`去掉了`FILE\_TO\_COPY`项, 所以之前拷贝`robots.txt`之类的文件, 就会失效, 使用`STATIC_PATHS`即可

    STATIC_PATHS = [u"upload", "extra/robots.txt",
                    "extra/404.html",
                ]
如果想指定静态文件位置, 可以使用`STATIC_SAVE_AS`, 会将静态文件存到另外一个目录,
而且对所有`STATIC_PATHS`项生效

    STATIC_SAVE_AS = "static/"

也可以使用 `EXTRA_PATH_METADATA`来为每一项指定路径

    EXTRA_PATH_METADATA = {
        "extra/robots.txt":{"path":"robots.txt"},
        "extra/404.html": {"path":"404.html"},
    }


至此, `Pelican`升级完毕

