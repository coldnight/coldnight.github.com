#!/usr/bin/env python
# -*- coding: utf-8 -*- #

from datetime import date


AUTHOR = "cold"
SITENAME = "cold's world"
SITEURL = "https://www.linuxzen.com"
SITE_SOURCE = "https://github.com/coldnight/coldnight.github.com"
FEED_DOMAIN = SITEURL
FEED_ATOM = None
FEED_ALL_ATOM = "feeds/all.atom.xml"

DISPLAY_PAGES_ON_MENU = False

MENUITEMS = [
    ('Zettelkasten', '/notes/')
]

DISPLAY_PAGES_ON_RIGHT = True

DISQUS_SITENAME = "linuxzen"

TIMEZONE = "Asia/Shanghai"

DEFAULT_LANG = "zh"

DEFAULT_CATEGORY = "Python"

ARCHIVES_URL = "archives.html"

GITHUB_URL = "https://github.com/coldnight/coldnight.github.com"
GITHUB_POSITION = "right"

COPY_YEAR = date.today().strftime("%Y")

STATIC_PATHS = [
    "static/upload",
    "static/css",
    "extra/favicon.ico",
    "extra/robots.txt",
    "extra/bdsitemap.txt",
    "extra/googledbee14f7be5461f0.html",
    # "extra/404.html",
]

OUTPUT_RETENTION = ["notes"]

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
    ("小邪兽_deepin", "http://neteue.com"),
    ("Frantic1048", "http://frantic1048.com/"),
    ("晓风'Blog", "http://www.dongxf.com/"),
    ("邪恶的二进制", "http://evilbinary.org/"),
    ("城南往事", "http://www.icnws.com"),
    ("一把汤勺", "http://jeepxiaozi.github.io/"),
    ("Zey's Blog", "http://www.zeython.com/"),
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

PLUGIN_PATHS = ["pelican-plugins"]
PLUGINS = [
    "sitemap",
    # 'gzip_cache',
    "render_math",
]

SITEMAP = {
    "format": "xml",
    "priorities": {"articles": 0.7, "indexes": 0.5, "pages": 0.3},
    "changefreqs": {"articles": "monthly", "indexes": "daily", "pages": "monthly"},
}

SITE_DESCRIPTION = (
    "博主一个爱好开源技术的人，对 Python 比较熟悉，"
    "也喜欢用 Python 捣腾一些东西，本博主要分享一些开源技术，"
    "其中包括但不限于 Linux/Python/Vim。"
)

SITE_KEYWORDS = "Python, Linux, vim, 开源, Tornado"


# ------------- Theme --------------
THEME = "attila"
SITESUBTITLE = '纸上得来终觉浅，绝知此事要躬行'

COLOR_SCHEME_CSS = 'tomorrow.css'


CSS_OVERRIDE = ['static/css/wide.css']

HOME_COVER = "static/upload/cover.jpg"

HEADER_COVERS_BY_TAG = {}
HEADER_COVERS_BY_CATEGORY = {}

AUTHORS_BIO = {
  "cold": {
      "name": "Gray King",
      "cover": "static/upload/cover.jpg",
      "image": "https://s.gravatar.com/avatar/4d0bc04ed0e44ab750ba32b5224101d7?s=200",
      "website": "https://github.com/coldnight",
      "location": "Beijing",
      "bio": "纸上得来终觉浅，绝知此事要躬行",
    }
}

SOCIAL = (
    ("GitHub", "https://github.com/coldnight"),
    ("Twitter", "https://twitter.com/grayking_w"),
    ("Linkedin", "https://www.linkedin.com/in/gray-king-71957191/"),
    ("rss", "/" + FEED_ALL_ATOM),
)

try:
    import os
    import sys

    sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)))
    from local_pelicanconf import *
except ImportError:
    pass
