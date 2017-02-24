Title: 使用Pelican博客静态生成系统
Tags: 博客,Pelican, 静态, 迁移
Date: 2013-3-14 15:12

# 介绍
最近流行使用静态生成的博客, 看了很多感觉很棒总想尝试一下, 大名鼎鼎的`Jekyll`使用ruby,看了很多文档介绍,依旧不能入门使用,也许是用Ruby编写,对它比较迟钝.看到了一款Python编写的静态生成系统`Pelican`, 试用后除了模板较少外感觉还是不错的,使用`Markdown`和`Rst`编写.

# 安装
可以试用`pip`来安装`Pelican`, 通过`pelican-quickstart`脚本可以快速构建一个站点,详细不在描述.可以看看它的文档

## 模板
模板采用`neat`,这个模板设计简单(看我的页面就知道有多简单了)我非常喜欢, 但是有几个链接`bug`,bug不是很难找,很轻松就能找出来, 这里不列出来, 基于这个模板我做了一点更改.

# 使用
在执行`pelican-quickstart`是开启Makefile, 可以通过make很简单的生成html.
```bash
make html   # 生成html
make clean  # 删除output
```

## 技巧
我是将生成的html通过nginx来打开(Nginx配置就很简单了, 这里就不做介绍), 那么如何确保更新呢?我将整个博客上传到github, 这样随时随地可以`clone`下来进行更改然后`push`上去,在服务器我通过crontab来定时更新html
```bash
crontab -e
```
添加如下内容
```bash
*/5 * * * * cd /path/to/your/blogsource/ && git pull &> /dev/null
```
这样就可以没5分钟更新一次


