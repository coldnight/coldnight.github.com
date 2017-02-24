#!/usr/bin/env python
# -*- coding: utf-8 -*- #

AUTHOR = u'cold'
SITENAME = u"cold's world"
SITEURL = u'https://www.linuxzen.com'
# SITEURL = u'http://localhost:8000'
SITE_SOURCE = u"https://github.com/coldnight/coldnight.github.com"
SITE_TAGLINE = u"木秀于林"
FEED_DOMAIN = SITEURL
FEED_ATOM = None
FEED_ALL_ATOM = u"feeds/all.atom.xml"

DISPLAY_PAGES_ON_MENU = False

DISPLAY_PAGES_ON_RIGHT = True

DISQUS_SITENAME = u"linuxzen"

TIMEZONE = 'Asia/Shanghai'

DEFAULT_LANG = u'zh'

THEME = "pelican-themes/zurb-F5-basic"

DEFAULT_CATEGORY = u"Python"

ARCHIVES_URL = "archives.html"

GITHUB_URL = u"https://github.com/coldnight/coldnight.github.com"
GITHUB_POSITION = "right"

FLOW_CONTENT = u"""
<script type="text/javascript">var cnzz_protocol = (("https:" == document.location.protocol) ? " https://" : " http://");document.write(unescape("%3Cspan id='cnzz_stat_icon_3767683'%3E%3C/span%3E%3Cscript src='" + cnzz_protocol + "s96.cnzz.com/stat.php%3Fid%3D3767683%26show%3Dpic' type='text/javascript'%3E%3C/script%3E"));</script>
"""
STATIC_PATHS = [u"static/upload",
                "extra/robots.txt",
                "extra/bdsitemap.txt",
                "extra/googledbee14f7be5461f0.html",
                # "extra/404.html",
                ]
# STATIC_SAVE_AS = "static/upload/"
EXTRA_PATH_METADATA = {
    "extra/robots.txt": {"path": "robots.txt"},
    "extra/bdsitemap.txt": {"path": "bdsitemap.txt"},
    "extra/googledbee14f7be5461f0.html": {"path": "googledbee14f7be5461f0.html"},
    "extra/404.html": {"path": "404.html"},
}
# Blogroll
LINKS = (('eleven', 'http://eleveni386.7axu.com'),
         (u'小邪兽_deepin', "http://neteue.com"),
         (u'Frantic1048', "http://frantic1048.com/"),
         (u"晓风'Blog", "http://www.dongxf.com/"),
         (u"邪恶的二进制", "http://evilbinary.org/"),
         (u"城南往事", "http://www.icnws.com"),
         (u"一把汤勺", "http://jeepxiaozi.github.io/"),
         (u"Zey's Blog", "http://www.zeython.com/"),
         )

# Social widget
SOCIAL = (("G+", "https://plus.google.com/u/0/118104100603784013039"),
          ('Github', 'https://github.com/coldnight'),
          ("Linkedin", "http://www.linkedin.com/pub/%E4%BC%9A-%E7%8E%8B/91/571/719"),
          ("Atom Feed", "https://www.linuxzen.com/feeds/all.atom.xml"),
          )

DEFAULT_PAGINATION = 10

MD_EXTENSIONS = (['codehilite(css_class=highlight)', 'extra',
                  'fenced_code', 'tables', 'sane_lists'])

PLUGIN_PATHS = [u"pelican-plugins"]
PLUGINS = ['sitemap']  # , 'gzip_cache']

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

DESCRIPTION = u"博主一个爱好开源技术的人, 对Python比较熟悉,"\
    u"也喜欢用Python捣腾一些东西, 本博主要分享一些开源技术,"\
    u"其中包括但不限于Linux/Python/Vim."

KEYWORDS = u"Python, Linux, vim, 开源, Tornado"
