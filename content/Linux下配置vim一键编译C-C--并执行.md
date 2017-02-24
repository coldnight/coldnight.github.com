Title: Linux下配置vim一键编译C/C++并执行
Tags: 编译,一键,vim,Linux,C/C++
Category: Vim
Date: 2012-04-05 12:07
最近在学习C++,编辑器当然是vim,想在编辑的时候可以一键编译,于是自己写了一个小脚本配合vim来实现.由于刚开始学,所以对C/C++的扩展名不太了解,所以只对.cpp .cc .c进行处理.

首先在/usr/bin/下创建compile脚本:
```
vi /usr/bin/compile
```
添加如下内容:
```bash
#!/bin/bash
# Filename : compile
# Describe : To compile c/c++
# Author   : cold night(www.linuxzen.com)
# Version  : 0.2
# Change   : 增加终端着色 

clear
if [ $# -eq 1 ]
then
    filename=$1
    outname=${filename%\.*}
    typename=${filename#*\.}
    if ( test "$typename" = "cpp" || "$typename" = "cc" )
    then
        echo -n "Compiling..."
        g++ -o "$outname" "$filename" 2> /tmp/errinfo >&2
        if [ $? -eq 0 ]
        then
            echo -e "       \033[32;1mSuccess!!!\033[0m"
            echo "------------------------"
            echo -e "\033[1;44mRunning...\033[0m"
            echo "------------------------"
            ./"$outname"
            echo "------------------------"
        else
            echo -e "       \033[1;31mError!!!\033[0m"
            echo "------------------------"
            echo -e "\033[1;44mError Info:\033[0m"
            echo "------------------------"
            cat /tmp/errinfo
            echo "------------------------"
        fi

    elif [ "$typename" = "c" ]
    then
        echo "Compiling..."
        echo "------------------------"
        gcc -o "$outname" "$filename" 2>/tmp/errinfo >&2
        if [ $? -eq 0 ]
        then
            echo -e "       \033[32;1mSuccess!!!\033[0m"
            echo "------------------------"
            echo -e "\033[1;44mRunning...\033[0m"
            echo "------------------------"
            ./"$outname"
            echo "------------------------"
        else
            echo -e "       \033[1;31mError!!!\033[0m"
            echo "------------------------"
            echo -e "\033[1;44mError Info:\033[0m"
            echo "------------------------"
            cat /tmp/errinfo
            echo "------------------------"
        fi

    fi
else
    echo 'Error: No intput filename'
fi
```
然后给脚本赋予执行权限
```
chmod +x /usr/bin/compile
```
然后编辑vim配置文件:
```
vi ~/.vimrc
```
然后添加下面内容:
```vim
" C++ complier
autocmd FileType cpp map <F8> <Esc>:w!<CR>:!compile %<CR>
autocmd FileType cc map <F8> <Esc>:w!<CR>:!compile %<CR>
autocmd FileType c map <F8> <Esc>:w!<CR>:!compile %<CR>
```
配置完毕我们就可以用vim编辑C/C++源文件的时候按F8就可以进行一键编译执行.
