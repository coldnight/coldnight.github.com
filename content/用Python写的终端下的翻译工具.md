Title: 用Python写的终端下的翻译工具
Tags: 翻译,终端,python,Linux,google
Category: Python
Date: 2012-04-23 23:23
为什么写这个程序,为什么不给这个程序配备gui?原因很简单,因为我是一个命令行控,Linux习惯了不习惯了鼠标,总觉得点着不如敲命令快,各位在看这篇文章就说明和本人有相同的爱好.这个用python写的翻译工具是通过google来实现的,由于google返回的数据不是很规范(或者说我没有找到规律),现在前三项能正常显示(源词,翻译结果,和汉语拼音).下面的词性和其他释义可能不同,见谅,望大神可以指点下小弟和帮小弟完善,这里赶紧不尽.

好了不费话了,下面放代码:
```python
#!/usr/bin/env python
# -*-coding:utf8 -*-
'''
#=============================================================================
#     FileName: translate.py
#         Desc: To translate with zh to en or en2zh
#       Author: cold
#        Email: wh_linux@126.com
#     HomePage: http://www.linuxzen.com
#      Version: 0.0.1
#   LastChange: 2012-04-23 23:04:08
#      History:
#=============================================================================
'''

import urllib
import urllib2
from sys import argv,exit
import re

# 显示帮助信息
def helpinfo():
print '''
Usage: pytran {zh2en¦en2zh} content
'''
# 格式化输出
def formatresult(result,srclang):
resu = result.split('[[')
if (srclang=='en2zh' or srclang == 'zh2en'):
firstre = resu[1].replace('[','').replace(']','').split('"')
print '源词:',firstre[3]
print '结果:',firstre[1]
if (srclang=='zh2en'):
piny = firstre[7]
else:
piny = firstre[5]
print '拼音:',piny
if(srclang=='zh2en'):
secresu=resu[2].replace('"','').split('[')
else:
secresu = resu[2].replace('"', '').split('[')
print '词性:',secresu[0].replace(',','')
print '其他释义:'
for i in ''.join(secresu[1].split(']')).split(','):
print i

# 获取命令行参数
try:
srclang = argv[1]
except:
helpinfo()
exit(1)
try:
cont = argv[2]
except:
helpinfo()
exit(2)

# 判断翻译目标语言用来确定传送参数
if(srclang == 'zh2en'):
data=urllib.urlencode({'client':'t', 'text':cont,
'hl':'zh-CN','tl':'en',
'multires':'1','prev':'btn',
'ssel':'0','sc':'1'})
elif(srclang == 'en2zh'):
data=urllib.urlencode({'client':'t', 'text':cont,
'hl':'zh-CN', 'sl':'en','tl':'zh-CN',
'multires':'1', 'prev':'btn',
'ssel':'0','sc':'1'})
else:
helpinfo()

# 打开google翻译内容
url = 'http://translate.google.cn/translate_a/t'
req =urllib2.Request(url,data)
req.add_header("User-Agent", "Mozilla/5.0+(compatible;+Googlebot/2.1;++http://www.google.com/bot.html)")
fd = urllib2.urlopen(req)
result =  fd.read()

# 格式化输出
formatresult(result, srclang)
fd.close()
```
为了更方便的使用我们把这个脚本命名位pytranslate,放到/usr/bin下,并赋予执行权限:
```
chmod +x /usr/bin/pytranslate
```
然后我们就可以使用它进行翻译了:
翻译英文到中文:
```
pytranslate en2zh extent
源词: extent
结果: 程度
拼音: Chéngdù
词性: 名词
其他释义:
程度
范围
幅度
规模
度
地步
广度
长度
面
长短
份儿
界
en
翻译中文到英文
pytranslate zh2en 中国
源词: 中国
结果: China
拼音: Zhōngguó
词性: 名词
其他释义:
China
zh-CN
```
好吧相信聪明的你肯定发现如何使用了这里就不罗嗦了.
