#!/usr/bin/env python
# -*- coding: utf-8 -*- #

from datetime import date


AUTHOR = u"cold"
SITENAME = u"cold's world"
SITEURL = u"https://www.linuxzen.com"
# SITEURL = u'http://localhost:8080'
SITE_SOURCE = u"https://github.com/coldnight/coldnight.github.com"
SITE_TAGLINE = u"木秀于林"
FEED_DOMAIN = SITEURL
FEED_ATOM = None
FEED_ALL_ATOM = u"feeds/all.atom.xml"

DISPLAY_PAGES_ON_MENU = False

DISPLAY_PAGES_ON_RIGHT = True

DISQUS_SITENAME = u"linuxzen"

TIMEZONE = "Asia/Shanghai"

DEFAULT_LANG = u"zh"

THEME = "zurb-F5-basic"

DEFAULT_CATEGORY = u"Python"

ARCHIVES_URL = "archives.html"

GITHUB_URL = u"https://github.com/coldnight/coldnight.github.com"
GITHUB_POSITION = "right"

COPY_YEAR = date.today().strftime("%Y")

STATIC_PATHS = [
    u"static/upload",
    "extra/favicon.ico",
    "extra/robots.txt",
    "extra/bdsitemap.txt",
    "extra/googledbee14f7be5461f0.html",
    # "extra/404.html",
]

# STATIC_SAVE_AS = "static/upload/"
EXTRA_PATH_METADATA = {
    "extra/favicon.ico": {"path": "favicon.ico"},
    "extra/robots.txt": {"path": "robots.txt"},
    "extra/bdsitemap.txt": {"path": "bdsitemap.txt"},
    "extra/googledbee14f7be5461f0.html": {"path": "googledbee14f7be5461f0.html"},
    "extra/404.html": {"path": "404.html"},
}
# Blogroll
LINKS = (
    ("eleven", "http://eleveni386.7axu.com"),
    (u"小邪兽_deepin", "http://neteue.com"),
    (u"Frantic1048", "http://frantic1048.com/"),
    (u"晓风'Blog", "http://www.dongxf.com/"),
    (u"邪恶的二进制", "http://evilbinary.org/"),
    (u"城南往事", "http://www.icnws.com"),
    (u"一把汤勺", "http://jeepxiaozi.github.io/"),
    (u"Zey's Blog", "http://www.zeython.com/"),
)

# Social widget
SOCIAL = (
    ("GitHub", "https://github.com/coldnight"),
    ("Linkedin", "https://www.linkedin.com/in/gray-king-71957191/"),
    ("Atom Feed", "https://www.linuxzen.com/feeds/all.atom.xml"),
)

DEFAULT_PAGINATION = 10

PIWIK_SSL_URL = "piwik.linuxzen.com"
PIWIK_URL = PIWIK_SSL_URL
PIWIK_SITE_ID = "2"

MARKDOWN = {
    "extension_configs": {
        "markdown.extensions.codehilite": {"css_class": "highlight"},
        "markdown.extensions.extra": {},
        "markdown.extensions.sane_lists": {},
        "markdown.extensions.toc": {},
    },
    "output_format": "html5",
}

PLUGIN_PATHS = [u"pelican-plugins"]
PLUGINS = ["sitemap"]  # , 'gzip_cache']

SITEMAP = {
    "format": "xml",
    "priorities": {"articles": 0.7, "indexes": 0.5, "pages": 0.3},
    "changefreqs": {"articles": "monthly", "indexes": "daily", "pages": "monthly"},
}

DESCRIPTION = (
    u"博主一个爱好开源技术的人，对 Python 比较熟悉，"
    u"也喜欢用 Python 捣腾一些东西，本博主要分享一些开源技术，"
    u"其中包括但不限于 Linux/Python/Vim。"
)

KEYWORDS = u"Python, Linux, vim, 开源, Tornado"
