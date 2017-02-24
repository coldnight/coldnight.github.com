Title: Lisp 文件和文件I/O
Date: 2013-03-28 17:12
Category: Lisp
Tags: Lisp, 学习, 笔记, 文件, I/O


## 读取文件数据
*OPEN*函数获取一个流并从中读取文件的内容.

*READ-CHAR*读取单个字符, *READ-LINE*读取一行文本, 去掉行结束字符作为一个字符串返回, *READ*读取单一个S-表达式并返回一个Lisp对象.

*CLOSE*函数用来关闭文件流
```lisp
(let ((in (open "/some/file/name.txt")))
  (format t "~a~%" (read-line in))
  (close in))
```
可以给*OPEN*指定关键字参数 _:if-does-not-exist_来指定打开文件不存在事的行为: _:error_, 报错(默认), _:create_, 继续进行并创建该文件, *NIL*, 让它返回*NIL*代替一个流
```lisp
(let ((in (open "/some/file/name.txt" :if-dos-not-exist nil)))
  (when in
    (format t "~a~%" (read-line in))
    (close in)))
```

*READ-CHAR*, *READ-LINE*和*READ*都接受一个可选参数, 其默认值为真并指定当函数在文件末尾出被调用是是否应该报错.如果参数为*NIL*, 它们在遇到文件结尾时将返回它们的第三个参数的值, 默认为*NIL*, 因此可以像下面这样打印一个文件的所有行:
```lisp
(let ((in (open "/some/file/name.txt" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
        while line do (format t "~a~%" line))
    (close in)))
```
*READ*是Lisp独有的.每次调用时, 它会读取单一的S-表达式, 跳过空格和注释, 然后返回有S-表达式代表的Lisp对象.

*PRINT*可以以"可读的"形式打印Lisp对象.所以当需要保存一点数据时, *PRINT*和*READ*提供了一个简单的途径, 无须涉及一套数据格式或编写一个解析其, S-表达式也是用于诸如配置文件等事务的良好格式.(但小心*READ*读取后*PRINT*丢失注释)

## 读取字符流
*OPEN*默认返回字符流, 可以向*OPEN*传递一个值为 '(unsigned-byte *)的 _:element-typ_参数就可以读取原始字节

使用*READ-BYTE*来读取上面*OPEN*函数返回的流, 它同样支持可选参数以便指定读取文件末尾时是否应该报错

## 批量读取
最后一个读取函数*READ-SEQUENCE*可以同时工作在字符和二进制流上.同时可以传递一个序列(通常是一个向量)和一个流, 它会尝试用来自流的数据填充该序列.它返回序列中的第一个没有被填充元素的索引, 或者在完全填充的情况下返回改序列的茶馆难度, 也可以传递 _:start_和 _:end_关键字参数来指定一个应当被替代填充的子序列.改序列参数的元素类型必须足以保存带有改流元素类型的元素.

## 文件输出
*OPEN*函数指定 给 _:direction_关键字参数传递 _:output_参数可以获取一个输出流

_:if-exists_关键字参数可以在文件存在时不报错, 传递 _:supersede_可以告诉*OPEN*来替换已有文件, 传递 _:append_将导致*OPEN*打开已有的文件新数据将被追加到文件末尾, _:overwrite_返回一个从文件爱能开始出开始的流从而覆盖已有的数据.传递*NIL*将倒是*OPEN*在文件已存在时返回*NIL*而不是流

一个典型的*OPEN*输出流:
```lisp
(open "/some/file/name.txt" :direction :output :if-exists :supersede)
```

### 写数据函数
*WRITE-CHAR* 会向流中写入一个单一字符

*WRITE-LINE* 写一个字符串并紧跟一个换行

*WRITE-STRING* 写一个字符串而不会添加任何字符

*TERPRI* 无条件打印一个换行符

*FRESH-LINE* 打印一个换行符, 除非改流已经在一行的开始处

*PRINT* 打印一个S-表达式, 前缀一个换行以及一个空格

*PRIN1* 只打印S-表达式

*PPRINT* 美观的打印S-表达式

并非所有的对象都能以一种*READ*可理解的形式打印出来.当试图使用*PRINT* *PRIN1*或*PPRINT*来打印这样一种对象时, 变量*PRINT-READABLY*将会予以控制.当它是*NIL*时, 这些函数将以一种导致*READ*在试图读取时肯定会报错的特殊语法来打印对象;否则它们直接报错而不直接打印该对象

*PRINC*是一种很适合人们使用的方式来答应Lisp对象

*OPEN*打开文件时传递值为 '(unsigned-byte 8)的 _:element-type_实参可以获取一个二进制输出流, 使用*WRITE-BYTE*向流中写入单独的字节

批量输出函数*WRITE-SEQUENCE*可同时接受二进制和字符流, 只要序列中所有元素都是用与改流的适当类型即可

## 关闭文件
*WITH-OPEN-FILE*宏可以确保文件使用完毕后总是被关闭, 其是建立在*UNWIND-PROTECT*之上

所以推荐打开文件%99都用这个宏

## 可移植的文件名 ---- 路径名
为了可移植Common Lisp提供了一种文件名的表示方式:路径名对象.路径名以一种解构话的方式来表示文件名

路径名是一种使用6个组来表示文件名的结构化对象:主机、设备、目录、名称、类型以及版本.这些组件多数都接受原子值, 通常是字符串.只有目录组件有其进一步的机构, 含有一个目录名列表, 其中带有关键字 _:absolute_ 或 _:relative_作为前.只有目录组件有其进一步的机构, 含有一个目录名列表, 其中带有关键字 _:absolute_ 或 _:relative_作为前缀.

可以使用*PATHNAME*函数将字符串转化成路径名

Unix文件系统上, 只有目录、名称和了性组件通常会被用到。在Windows上， 还有一个组件（通常是设备或主机）用来保存驱动器字母

可以使用*PATHNAME-DIRECTORY*、PATHNAME-NAME*和*PATHNAME-TYPE*来检查一个路径名的单独组件
```lisp
CL-USER> (pathname-directory (pathname "/foo/bar/baz.txt"))
(:ABSOLUTE "foo" "bar")
CL-USER> (pathname-name (pathname "/foo/bar/baz.txt"))
"baz"
CL-USER> (pathname-type (pathname "/foo/bar/baz.txt"))
"txt" 
```

*PATHNAME-HOST* *PATHNAME-DEVIECE*和*PATHNAME-VERSION*允许你访问其他三个路径组件名


#p后接一个双引号的字符串是路径名的字面量

*NAMESTRING*接受一个路径名描述符并返回一个名字字符串

*DIRECTORY-NAMESTRING* 和 *FILE-NAMESTRING*返回一个部分名字字符串

```lisp
CL-USER> (namestring #p"/foo/bar/baz.txt")
"/foo/bar/baz.txt"
CL-USER> (directory-namestring #p"/foo/bar/baz.txt")
"/foo/bar/"
CL-USER> (file-namestring #p"/foo/bar/baz.txt")
"baz.txt"
```

## 构造新路径名
*MAKE-PATHNAME*函数来构造任意路径名.每个路径名组件都接受一个关键字参数并返回一个路径名.没被传递的将为*NIL*
```lisp
CL-USER> (make-pathname
          :directory '(:absolute "foo" "bar")
          :name "baz"
          :type "txt")
#P"/foo/bar/baz.txt" 
```

*MERGE-PATHNAMES*接受两个路径名并合并它们, 用跌入个路径名对应的值填充第一个路径名中任何*NIL*组件

## 与文件系统交互
*PROBE-FILE*用于测试某个路径名描述符的文件是否存在, 如果存在将返回该文件的真实名称

*DELETE-FILE*和*RENAME-FILE*用来删除和重命名, *DELETE-FILE*接受一个路径名描述符并删除所命名的文件, 当成功是返回真, 否则它产生另一个*FILE-ERROR*报错

*RENAME-FILE*接受两个路径名描述符, 并进行重命名

*ENSURE-DIRECTORIES-EXIST*来创建目录.它接受一个路径名描述符并确保目录组件中的所有元素存在并且是目录, 如果必要的话它会创建它们. 如果传递的不是目录形式, 则最后一级子目录不会被创建

*FILE-WRITE-DATE*和*FILE-AUTHOR*都接受一个路径名描述符.

*FILE-WRITE-DATE*返回文件上次被写入的事件, 表示形式是从GMT 1990-1-1 00:00起的秒数

*FILE-AUTHOR* 返回文件拥有者

*FILE-LENGTH*由于历史原因接受一个流而不是路径名

*FILE-POSITION* 同样接受一个流, 并返回文件中的当前位置

## 其他文件 I/O类型

*STRING-STREAM*从一个字符串中读取或写入数据

*MAKE-STRING-INPUT-STREAM*和 *MAKE-STRING-OUT-STREAM*用来创建 *STRING-STREAM*, 它们返回一个字符串流可以被 *READ-CHAR* *READ-LINE*或*READ*或*FORMAT* *PRINT* *WRITE-CHAR*以及*WRITE-LINE*这种函数操作

被写入的字符串流可以使用 *GET-OUTPUT-STREAM-STRING*来获取改字符串, 并清空该流

宏*WITH-INPUT-FROM-STRING*和*WITH-OUTPUT-TO-STRING*提供了以个更加遍历的接口

*BROADCAST-STREAM*是一个输出流, *MAKE-BROADCAST-STREAM*用来构造它, *CONCATENATED-STREAM*是一个输入流, *MAKE-CONCATENATED-STREAM*用来构造它, 其接受任何数量的流作为参数


两种可以将流以多种方式拼接在一起的双向流是 *TWO-WAY-STREAM*和*ECHO-STREAM*, 使用*MAKE-TWO-WAY-STREAM*和*MAKE-ECHO-STREAM*用来构造它们, 它们都接受两个参数, 一个输入流一个输出流, 并返回一个适当类型的可同时用于输入和输出的流

不同的是 *ECHO-STREAM*这中的输出流将含有会话的双发的一个副本
