Title: Vim 结合Python编写的翻译插件
Tags: 翻译,插件,vim,python
Category: Vim
Date: 2013-01-04 17:58
最近重写了一下之前用Python写的一个终端翻译工具,想着Vim7.3 支持Python于是想将这个功能写为一个插件让Vim也支持翻译功能,现在英汉翻译比较完善, <Leader>t会翻译光标下单词,选中的翻译还不完善,仅仅是个半成品,在此抛砖引玉.将下面代码复制保存为translate.vim 放到~/.vim/plugin目录下即可,代码如下:
```vim
"   Author  :   cold
"   E-mail  :   wh_linux@126.com
"   Date    :   2012/12/20 16:23
"   Desc    :   英汉/汉英翻译插件
"   Useage  :
"                  <Leader> t 翻译当前光标下内容 //XXX 中文不行
"                  <Leader> lt 翻译当前行
"                  <Leader> vt 翻译选中的内容

function GetCursorWord()
    let column = get(getpos('.'), 2, 0) - 1
    let line = getline('.')
    let word = strpart(line, column, 1)
    let start = 1
    while 1
        let tmp = strpart(line, column + start, 1)
        if tmp =~ "[a-zA-Z]"
            let word = word . tmp
        else
            break
        endif
        let start = start + 1
    endwhile
    let start = 1
    while 1
        let tmp = strpart(line, column - start, 1)
        if tmp =~ '[a-zA-Z]'
            let word = tmp . word
        else
            break
        endif
        let start = start + 1
    endwhile
    return word
endfunc

function Translate(m)
    " m 1 -> 翻译当前行
    " m 2 -> 翻译选中
    if mode() == 'n'
        let word = GetCursorWord() " 如果是命令模式则取当前字符下单词
    endif
    "XXX Visual 模式mode()命令返回n
    if a:m == 2
        let word = getreg('*')     " 如果是选择模式获取选择块

    endif
    if a:m == 1
        let word = getline('.')
    endif
python << EOF

import vim
import urllib
import urllib2

class Translate(object):
    """ 使用google进行英->汉, 汉->英的翻译 """
    def __init__(self, text, src='zh-CN', dst = 'en'):
        self.url = 'http://translate.google.cn/translate_a/t'
        self.params = dict(client = "t", text=text,
                           hl = 'zh-CN', tl = dst,
                           multires = '1', prev = 'btn',
                           ssel = '0', sc = '1')
        if src != self.params.get('hl'):
            self.params.update(sl = src)

        return

    def loads(self, content):
        """ 加载翻译结果 """
        while ',,' in content or '[,' in content:
            content = content.replace(',,', ',"",')
            content = content.replace('[,', '["",')
            #content = content.replace(',]', '"",]')
        content = eval(content)
        result = content[0][0]
        desc = content[1]
        pinyin = result[2] if result[2] else result[3]
        others = ''
        for d in desc:
            others += d[0] + '\n'
            for i in d[1]:
                others += "\t{0}\t".format(i)
                for s in d[2]:
                    if s[0] == i:
                        others +=','.join(s[1]) + '\n'
        r = dict(
            result = result[0],
            source = result[1],
            pinyin = pinyin,
        )
        return r

    def translate(self):
        """ 调用google翻译 """
        params = urllib.urlencode(self.params)
        req = urllib2.Request(self.url, params)
        req.add_header("User-Agent",
                       "Mozilla/5.0+(compatible;+Googlebot/2.1;"
                       "++http://www.google.com/bot.html)")

        res = urllib2.urlopen(req)
        result =  res.read()
        return self.loads(result)

def auto_translate(text):
    """ 自动检测当前语言进行翻译 """
    text = text.decode('utf-8')
    if text[0] > u'z':
        src = 'zh-CN'
        dst = 'en'
    else:
        src = 'en'
        dst = 'zh-CN'
    t = Translate(text.encode('utf-8'), src, dst)
    result = t.translate()
    return result

word = vim.eval('word')
result = auto_translate(word.strip())
vim.command('echo "源词: ' + result.get("source") + '"')
vim.command('echo "结果: ' + result.get("result") + '"')
vim.command('echo "拼音: ' + result.get("pinyin") + '"')
if result.get('others'):
    vim.command('echo "其他释义: "')
    vim.command('echo "' + result.get('others') + '"')

EOF

endfunc
nmap <Leader>t :call Translate(0)<cr>
nmap <Leader>lt :call Translate(1)<cr>
"XXX 无法判断当前模式,使用映射替代
map <Leader>ts :call Translate(2)<cr>
```
