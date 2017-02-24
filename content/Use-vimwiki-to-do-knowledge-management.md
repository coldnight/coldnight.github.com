title: 使用 Vimwiki + git 做知识管理
date: 2013-12-26
tags: vim, vimwiki, wiki, 知识, 管理, git
category:Vim

一直在找一个合适的知识管理工具, 用过 Evernote, 但是不支持 Markdown, 用了一段时间也放弃了.
最近 python-cn 列表里也在讨论这个问题, 看到有人使用 Vimwiki, 所以就尝试了一下.

安装后,试着写了点东西, 发现很方便做知识管理和记录笔记, 可以生成HTML, 可以定制模板,
这里不讨论如何使用, Vimwiki 的文档介绍的很详细,

我使用[bootstrap](http://bootcss.com)和 jquery 对模板进行了一些定制:

* 添加导航
* 将toc移动到左侧

下面将介绍我是如何做的, 并在最后附上如何部署的

### 指定模板
首先需要更改默认模板
```vim
    let g:vimwiki_list = [{'path': '~/vimwiki',
    \    'path_html': '~/vimwiki_html',
    \    'template_path': '~/vimwiki/template',
    \    'template_default': "default.tpl"}]
```

并将默认的模板作为模板进行修改
```bash
mkdir -p ~/vimwiki/template
cp ~/.vim/bundle/vimwiki/autoload/vimwiki/default.tpl ~/vimwiki/template
```

### 添加静态文件
将jqeury和bootstrap的js和css文件放到 ~/vimwik\_html目录下,

为了统一修改和发布 Wiki, 我将静态文件放在 ~/vimwiki 的 static 目录下, 
并在 ~/vimwiki_html 创建一个软链链接到这个目录, 下面是我 static 目录的一个快照
```bash
$ tree ~/vimwiki/static 
~/vimwiki/static
├── bootstrap
│   ├── css
│   │   ├── bootstrap.min.css
│   │   └── bootstrap-theme.min.css
│   ├── fonts
│   │   ├── glyphicons-halflings-regular.eot
│   │   ├── glyphicons-halflings-regular.svg
│   │   ├── glyphicons-halflings-regular.ttf
│   │   └── glyphicons-halflings-regular.woff
│   └── js
│       └── bootstrap.min.js
├── css
│   └── wiki.css
└── js
    ├── jquery-1.8.3.min.js
    └── wiki.js

6 directories, 10 files
```
其中`wiki.js`和`wiki.css`分别是自定义的Javascript和CSS
然后在~/vimwiki_html中创建链接
```bash
$ ln -s ~/vimwiki/static ~/vimwiki_html/static
```
### 在模板中引用静态文件
结合 `%root_path%`模板变量引入静态文件
```html
<link rel="Stylesheet" type="text/css" href="%root_path%static/css/wiki.css">
<link rel="Stylesheet" type="text/css" href="%root_path%static/bootstrap/css/bootstrap.min.css">
<script type="text/javascript" src="%root_path%static/js/jquery-1.8.3.min.js"></script>
<script type="text/javascript" src="%root_path%static/bootstrap/js/bootstrap.min.js"></script>
<script type="text/javascript" src="%root_path%static/js/wiki.js"></script>
```

### 创建导航
根据 Boostrap 文档中的例子很容易就可以创建一个好看的导航
```html
<nav class="navbar navbar-default navbar-inverse" role="navigation">
 <div class="container">
    <div class="navbar-header">
<button data-target=".bs-navbar-collapse" data-toggle="collapse" type="button" class="navbar-toggle">
        <span class="sr-only"></span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
      </button>
        <a class="navbar-brand" href="/index.html">cold's wiki</a>
    </div>
    <div class="collapse navbar-collapse">
        <ul class="nav navbar-nav">
            <li><a href="%root_path%index.html">首页</a></li>
            <li><a href="%root_path%diary/diary.html">日记</a></li>
            <li><a href="%root_path%TODO.html">TODO</a></li>
        </ul>
    </div>
 </div>
</nav>
```

然后将 content 变量放在 'contaniner' 中
```html
<div class="container content-body">
    %content%
</div>
```

### 替换样式和将 toc 移动到左侧
替换表格样式
```javascript
$("table").addClass("table table-bordered table-striped table-hover");
```

然后将 `toc` 移动到右侧, 并使用 `affix` 效果
```javascript
// 生成左侧toc导航
if ($(".toc").html().trim()){
    var html = $(".content-body").html();
    var toc_html = '<div class="toc">'+$(".toc").html() + "</div>"
    var content = html.replace(toc_html, "");
    var html = '<div class="col-md-3">\n'+toc_html+'\n</div>\n';
    html += '<div class="col-md-9">\n'+content+'\n</div>\n';
    html = html.replace(/blockquote/g, "pre");
    $(".content-body").html(html);
    $(".toc").addClass("bs-sidebar");
    /* $(".toc").attr("role", "complementary"); */
    $(".toc").attr("data-spy", "affix");
    /* $(".toc").attr("data-offset-top", "200") */
    var uls = $(".toc").find("ul");
    for (var i = 0; i < uls.length; i++){
        ul = uls[i];
        console.log($(ul).parent(), $(".toc"));
        console.log($(ul).parent() == $(".toc"));
        console.log($(ul).parent().hasClass("toc"));
        if ($(ul).parent().hasClass("toc"))
            $(ul).addClass("nav bs-sidenav");
        else
            $(ul).addClass("nav");

    }
}
```

完整的[wiki.js](http://wiki.linuxzen.com/static/js/wiki.js)

## 自定义样式
我同时还用 Vimwiki 做TODO管理, 原有的样式感觉挺不错的, 就拷贝了一份, 
还有一点是从 bootcss.com 文档页面扒下来的,
可以在[这里](http://wiki.linuxzen.com/static/css/wiki.css)看到.

## 部署
我将我的vimwiki上传到了 [bitbucket](https://bitbucket.org), 然后添加了一个定义更新的脚本,
下面是脚本内容:
```bash

#!/bin/sh
PWD="/home/yourusername/vimwiki"
cd "$PWD"
git pull | grep "Allready up-to-date." 2>&1 > /dev/null

if [ $? -ne 0 ]
then
    vim +VimwikiIndex +VimwikiAll2HTML +qa
fi
cd -
```
上面脚本命名为 `update.sh` 保存在 `~/vimwiki` 下, 通过 `crontab` 进行定时调用
```bash
$ crontab -l
*/30 * * * * /bin/bash /home/yourusername/vimwiki/update.sh &> /dev/null
```

### 使用 Disqus 评论系统
我在 `template/default.tpl` 添加了代码来使用 `Disqus` 评论系统, 这里不在详述


### 已经上线的Wiki
当然现在我已经部署了自己的wiki, 猛击[这里](http://wiki.linuxzen.com)进入

### 接下来
现在wiki还没有代码高亮, 我不想使用那个js的高亮, 想通过 `pygments` 来做代码高亮,
所以接下想通过 `pygments` 对代码进行高亮


### 参考
* [用jQuery和Bootstrap美化VimWiki输出]( http://www.berlinix.com/vim/vimwiki_with_bootstrap_jquery.php )
