Title: Python将汉字按拼音排序--一个多音字引发的悲剧
Tags: 汉字排序,汉字,排序,拼音,多音字,python
Category: Python
Date: 2012-10-24 18:29
今天同事告诉我一个项目需要将汉字按拼音排序,之前没做过啊,就google之,down了一份汉字与拼音的对照表,对照表格式如下:
```
拼    pin1
音    yin1
```
将文件保存为`pinyindata`,于是有了如下对应的python代码:
```python
#!/usr/bin/env python
# -*- coding:utf-8 -*-
#
# Author : cold night
# E-mail : wh_linux@126.com
# Date   : 12-10-24 下午3:13
#
from os import path
import locale

class SortedByPy:
    __PYDICT__ = dict()
    PYDATA_PATH = path.join(path.dirname(__file__), 'pinyindata')
    def __init__(self):
        if not self.__PYDICT__:
            py_file = open(self.PYDATA_PATH, 'r')
            for line in py_file.readlines():
                word, pinyin = line.split('\t')
                self.__PYDICT__.update({word:pinyin.strip()})
            py_file.close()

    def get_py(self, word):
        """
        获取汉字拼音
        """
        result = []
        i = word.decode('utf-8')[0].encode('utf-8')
        result.append(self.__PYDICT__.get(i, i))
        result = ''.join(result)
        return result

    def cmp_by_py(self, A, B):
        """
        使用拼音比较两个汉字的先后(使用倒序)
        """

        r1 = self.get_py(A)
        r2 = self.get_py(B)
        return cmp(r1, r2)

    def sort(self, iterable, key=None, reverse=False):
        """
        - `iterable` :
        - `key` : 有则对列表中的字典对应key的值进行排序
        """
        result = []
        for i,v in enumerate(iterable):
            f = v[key][0] if key else v[0]
            if f <= 'z':result.append(iterable.pop(i))
        if key:
            result.sort(cmp = lambda x, y: self.cmp_by_py(x[key], y[key]), reverse= reverse)
        else:
            result.sort(reverse=reverse)
        if key:
            r = sorted(iterable, cmp=lambda x, y: self.cmp_by_py(x[key], y[key]), reverse=reverse)
        else:
            r = sorted(iterable, cmp=lambda x, y: self.cmp_by_py(x, y)l, reverse=reverse)
        result.extend(r)
        return result
```

嗯,写好后测试正常,提交后告诉同事,同事一测发现重庆怎么跑到最后啊,不对啊,有bug啊,我重新将重庆加入到我的测试,真是不行阿,好吧可能是对照表的问题,打开找了下,能找到重啊,难道是decode出问题了,于是打开咱的ipython shell,结果输入重庆,一回车终端崩了,好吧,我再试还是崩,于是我想可能是utf-8的bug或者python的uft-8的bug,于是我将我的想法告知了同事,同事也打开了`ipython试`了下,结果他的可以正常输入,正常decode/encode,这下我想可能是方法的问题吧.

于是看看有无别的办法,看到一个对Linux有效可能对window有效的办法,好嘛正好咱也Linux果断拿过来试试,于是下面的代码诞生了:
```python
#!/usr/bin/env python
# -*- coding:utf-8 -*-
#
# Author : cold night
# E-mail : wh_linux@126.com
# Date   : 12-10-24 下午3:13
#
#from os import path
import locale

class SortedByPy:
    __PYDICT__ = dict()
    #PYDATA_PATH = path.join(path.dirname(__file__), 'pinyindata')
    def __init__(self):
        """
        if not self.__PYDICT__:
            py_file = open(self.PYDATA_PATH, 'r')
            for line in py_file.readlines():
                word, pinyin = line.split('\t')
                self.__PYDICT__.update({word:pinyin.strip()})
            py_file.close()
        """
        pass

    def get_py(self, word):
        """
        获取汉字拼音
        """
        result = []
        i = word.decode('utf-8')[0].encode('utf-8')
        result.append(self.__PYDICT__.get(i, i))
        result = ''.join(result)
        return result

    def cmp_by_py(self, A, B):
        """
        使用拼音比较两个汉字的先后(使用倒序)
        """

        r1 = self.get_py(A)
        r2 = self.get_py(B)
        return cmp(r1, r2)

    def sort(self, iterable, key=None, reverse=False):
        """
        - `iterable` :
        - `key` : 有则对列表中的字典对应key的值进行排序
        """
        result = []
        """
        for i,v in enumerate(iterable):
            f = v[key][0] if key else v[0]
            if f <= 'z':result.append(iterable.pop(i))
        if key:
            result.sort(cmp = lambda x, y: self.cmp_by_py(x[key], y[key]), reverse= reverse)
        else:
            result.sort(reverse=reverse)
        """
        locale.setlocale(locale.LC_ALL, 'zh_CN.UTF-8')
        if key:
            r = sorted(iterable, cmp=locale.strcoll, key = lambda x: x[key], reverse=reverse)
        else:
            r = sorted(iterable, cmp=locale.strcoll, reverse=reverse)
        result.extend(r)
        return result
```
写好了跑起来测试嘛,重庆还是躺在了最后,我想这应该就是utf-8的bug了吧,系统都这样了,于是和同事交流后认为也许是bug吧,回到座位后我苦思冥想阿,绞尽脑汁啊,看看有没有别的办法阿,突然一道闪电阿,我明白了,重`多音字`啊也读`zhong`啊,排在最后是对的啊,没有错误,也没有bug,一个`多音字`引发的悲剧哇.
