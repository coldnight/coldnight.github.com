Title: 使用Pelican打造静态博客
Date: 2013-04-18 09:40
Category: Python
Tags: Python, 静态, 生成, Pelican, 博客

前面有文章介绍本站采用了`Python`编写的`Pelican`静态生成博客系统, 之所以没有使用当前很火的`Jekyll`, 是因为它是`Ruby`编写, 而我又对`Ruby`没有啥兴趣, 所以还是选择了使用了我熟悉的Python编写的这套系统, 我用了一段时间,打算将使用经验分享出来

## 介绍
`Pelican`是一套开源的使用Python编写的博客静态生成, 可以添加文章和和创建页面, 可以使用`MarkDown` `reStructuredText` 和 `AsiiDoc` 的格式来抒写, 同时使用 `Disqus`评论系统, 支持 `RSS`和`Atom`输出, 插件, 主题, 代码高亮等功能, 采用`Jajin2`模板引擎, 可以很容易的更改模板

## 安装
可以从`github`克隆最新的代码安装, 并且建议在`virtualenv`下使用:

### 建立 virtualenv
```bash
virtualenv pelican      # 创建
cd pelican
sh bin/activate            # 激活虚拟环境
```
上面建立了一个Python的虚拟环境(这个命令不是内置可以使用 `easy_install virtualenv` 安装)

### 从github克隆最新代码安装Pelican
```bash
git clone git://github.com/getpelican/pelican.git            # 代码
cd pelican
python setup.py install
```
上面步骤完成后就安装了Pelican

## 开始一个博客
```bash
mkdir /path/to/your/blog
cd /path/to/your/blog
pelican-quickstart
```
在回答一系列问题过后你的博客就建成的, 主要生成下列文件:
```bash
.
|-- content                # 所有文章放于此目录
|-- develop_server.sh      # 用于开启测试服务器
|-- Makefile               # 方便管理博客的Makefile
|-- output                 # 静态生成文件
|-- pelicanconf.py         # 配置文件
|-- publishconf.py         # 配置文件
```

### 写一篇文章
在 `content` 目录新建一个 `test.md`文件, 填入一下内容:
```md
Title: 文章标题
Date: 2013-04-18
Category: 文章类别
Tag: 标签1, 标签2

这里是内容
```
然后执行:
```bash
make html
```
用以生成html

然后执行
```bash
./develop_server.sh start
```
开启一个测试服务器, 这会在本地 8000 端口建立一个测试web服务器, 可以使用浏览器打开:`http://localhost:8000`来访问这个测试服务器, 然后就可以欣赏到你的博客了

### 创建一个页面
这里以创建 `About`页面为例

在`content`目录创建`pages`目录
```bash
mkdir content/pages
```
然后创建`About.md`并填入下面内容
```bash
Title: About Me
Date: 2013-04-18

About me content
```
执行 `make html` 生成html, 然后打开 `http://localhost:8000`查看效果


### 让Pelican支持评论
Pelican 使用`Disqus`评论, 可以申请在[Disqus](https://disqus.com/)上申请一个站点, 然后在`pelicanconf.py`里添加或修改`DISQUS_SITENAME`项:
```python
DISQUS_SITENAME = u"linuxzen"
```
执行
```bash
make html
```
浏览器打开 `http://localhost:8000`查看效果

### 更换主题
Pelican本身也提供了一些主题可供选择, 可以从github克隆下来
```bash
git clone git://github.com/getpelican/pelican-themes.git     # 主题
```
然后在里面找到想要的主题, 然后拷到博客项目当前目录, 这里已`neat`为例
```bash
cp -r /path/to/themes/from/github/neat .
```
然后在 `pelicanconf.py` 配置文件里添加或修改 `THEME`项为 `neat`
```python
THEME = "neat"
```

重新执行 
```bash
make html
```
然后打开 `http://localhost:8000` 查看效果

### 使用插件
Pelican 一开始是将插件内置的, 但是新版本 Pelican将插件隔离了出来, 所以我们要到github上 克隆一份新的插件, 在博客目录执行
```bash
git clone git://github.com/getpelican/pelican-plugins.git    # 插件
```
现在我们博客目录就新添了一个 `pelican-plugins`目录, 我们已配置`sitemap`插件为例,
`sitemap`插件可以生成 `sitemap.xml` 供搜索引擎使用

在`pelicanconf.py`配置文件里加上如下项:
```python
PLUGIN_PATH = u"pelican-plugins"

PLUGINS = ["sitemap"]

## 配置sitemap 插件
SITEMAP = {
    "format": "xml",
    "priorities": {
        "articles": 0.7,
        "indexes": 0.5,
        "pages": 0.3,
    },
    "changefreqs": {
        "articles": "monthly",
        "indexes": "daily",
        "pages": "monthly",
    }
}
```

然后再执行
```bash
make html
```
打开浏览器请求 `http://localhost:8000/sitemap.xml`即可看到生成的 Sitemap 了

### 拷贝静态文件
如果我们定义静态的文件, 该如何将它在每次生成的时候拷贝到 output 目录呢, 我们以`robots.txt` 为例, 在我们的 content/extra 下面我们放了一个定义好的 `robots.txt`文件, 在`pelicanconf.py`更改或添加 `FILES_TO_COPY`项:
```python
FILES_TO_COPY = (
    ("extra/robots.txt", "robots.txt"),
)
```
这样在每次生成html的时候都会把 `content/extra`下的 `robots.txt` 拷贝到 `output`目录下

### 拷贝静态目录
如果是一个静目录怎么办? 我们可以在`pelicanconf.py`里添加或修改 `STATIC_PATHS`项, 比如我们有个`img`目录用来放文章所使用的图片, 我们可以在`pelicanconf.py`添加
```python
STATIC_PATHS = [u"img"]
```

然后执行 
```bash
make html
```
然后 Pelican 就会将 `img`目录拷贝到 `output/static/` 下

## 部署
上面都弄完之后你就可以得到一个功能健全的博客系统, 接下来就是部署到服务器, 上传到服务器并结合nginx或者apache等web服务器部署这里就不在详述

## 参考
如果还有其他问题请参考[官方手册](http://docs.getpelican.com/)
