Title: About
Date: 2013-03-14

## 关于
```bash
$ whoami
cold

$ archey3
               +                OS: Arch Linux x86_64
               #                Hostname: Dream
              ###               Kernel Release: 3.10.32-1-lts
             #####              Uptime: 22:03
             ######             WM: Awesome
            ; #####;            DE: Xfce
           +##.#####            Packages: 864
          +##########           RAM: 1417 MB / 3923 MB
         #############;         Processor Type: Intel(R) Core(TM)2 Duo CPU     E7500  @ 2.93GHz
        ###############+        $EDITOR: vim
       #######   #######        Root: 21G / 40G (52%) (ext3)
     .######;     ;###;`".      
    .#######;     ;#####.       
    #########.   .########`     
   ######'           '######    
  ;####                 ####;   
  ##'                     '##   
 #'                         `#  

$ history | sed "s#^\s\+[0-9]\+\s\+##g" | grep -oP "(?<=^|\|)\w+"|sort |uniq -c| sort -k1,1nr -k2 | head
   1757 vim
   1275 ls
   1251 git
    854 sudo
    577 py
    423 cd
    282 rm
    171 cc
    170 cp
    151 cat
```

## 开源作品
* [clubot](https://github.com/coldnight/clubot) 一个gtalk/xmpp群bot
* [pual_bot](https://github.com/coldnight/pual_bot) 一个WebQQ机器人
* [tornadohttpclient](https://github.com/coldnight/tornadohttpclient) 一个对`tornado.curl_httpclient`的封装
* [twqq](https://github.com/coldnight/twqq) 基于 Tornado 的高效的异步的 WebQQ 客户端库

## 许可
![知识共享许可协议](http://i.creativecommons.org/l/by-sa/3.0/cn/88x31.png)
如无特别声明本站所有作品均为原创采用[知识共享署名-相同方式共享 3.0 中国大陆许可协议](http://creativecommons.org/licenses/by-sa/3.0/cn/)进行许可。

## 联系
博主现在维护一个 `gtalk/xmpp`群, 使用`gtalk/xmpp`的用户可以添加 `clubot@vim-cn.com`交流

也可以给我发送邮件: `d2hfbGludXhAMTI2LmNvbQ==`
