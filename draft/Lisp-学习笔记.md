Title: Lisp 学习笔记
Category: Lisp
Tags: Lisp, 笔记
Date: 2013-03-19

## 数据结构
### list
LIST 函数生成一个由其参数组成的列表
```lisp
CL-USER> (list 1 2 3)
(1 2 3)
```

### plist
属性列表(Property List)可以将列表中的给定位置映射到纪录中的给定字段(相当于hash表, 但是并不是真正的hash表)
```lisp
CL-USER> (list :a 1 :b 2 :c 3)
(:A 1 :B 2 :C 3)
```
plist 和使用创建普通列表的函数只是改变参数来实现

## 变量
### 定义
Lisp使用`DEFVAR`宏来定义变量.如果定义全局变量需用`*`来命名, 比如:
```lisp
(defvar *db* nil) ; 定义*db*全局变量
```
`DEFVAR`宏只能在所定义的变量不存在时才能定义成功, 如果定义的变量名已存在,则不更改变量值

### 赋值
`SETF`函数可以给变量赋值, 也可以给未定义的变量赋值,从而创建这个变量
```lisp
(setf *db* (list 1 2 3 4 5))
(setf *t* 1)
```

### push
使用`PUSH`宏来为为`变量`添加新的项
```lisp
(push (list 1 2 3) *db*)
```

### POP
`pop`宏所执行的操作与`PUSH`正好相反

### 全局变量
#### *QUERY-IO*
`*QUERY-IO*`是一个含有关联到当前终端输入流的全局变量

## 输入
可以用`FORMAT`在`*query-io*`上产生一个提示, 然后使用`FORCE-OUTPUT`确保LISP在`*query-io*`上打印提示信息之前不会换行, 最后使用`READ-LINE`函数来读取`*query-io*`里的单行文本(不好汗结尾的换行)
```lisp
(defun prompt-read (prompt)
    (format *query-io* "~a:" prompt)
    (force-output *query-io*)
    (read-line *query-io*))
```

### 输入数字
当需要从用户读取数字是可以使用`parse-integer`, 它可以从字符串中解析出数字,但默认解析不到时会报错, 但是它也接受一个关键字参数:`junk-allowed`可以让其适当地宽容一些

```lisp
(defun prompt-read (prompt)
    (format *query-io* "~a:" prompt)
    (force-output *query-io*)
    (read-line *query-io*))
(parse-integer (prompt-read "Age") :junk-allowed t)
```
`PARSE-INTEGER`无法找出整数将返回`NIL`而不是0, Lisp的`OR`宏可以解决这个问题
```lisp
(or (parse-integer (prompt-read "Age") :junk-allowed t) 0)
```

### y/n
当需要y/n确认时返回布尔可以使用lisp的`Y-OR-N-P`函数
```lisp
(y-or-n-p "18 years old? [y/n]") ;; 为什么这个第一出现在我脑子里???
```

## 输出
### FORMAT
`FORMAT`函数可以将数据打印到屏幕, `FORMAT`接收两个基本参数, 第一个参数是要打印的设备, 第二个是包含基本文本的格式化串,格式指令以`~`开始 LISP的Hello, world
```lisp
(format t "Hello, world!")
```
#### FORMAT 格式指令
* ~a 美化指令,消耗`format`一个实参,然后将其输出成人类可读格式(比如plist不带前导冒号, 字符串没有引号)
* ~t 表示制表符, 可以使用~10t让`FORMAT`产生足够的空格(不消耗是参)
* ~{ 表示下一个被使用的实参必须是一个列表, `FORMAT`在列表上循环操作, 处理位于~{ 和 ~}之前的指令, 同事每次需要时, 从列表上使用尽可能多的元素
* ~% 不消耗实参产生一个换行
* ~r 可以将一个数据翻译成英文
```lisp
CL-USER> (format t "~a:~10t~a" :author "Wood.D")
AUTHOR:     Wood.D
```

使用格式化的例子
```lisp
CL-USER> (defvar *info* (list :author "Wood.D" :url "www.linuxzen.com" :system "Linux" :editor "Vim"))
CL-USER> (format t "~{~a: ~10t~a~%~}~%" *info*)
AUTHOR:   Wood.D
URL:      www.linuxzen.com
SYSTEM:   Linux
EDITOR:   Vim
```

### WRITE-LINE
```lisp
(write-line "Hello, world!")
```

### PRINT
```lisp
(print "Hello, world!")
```

## 循环
### DOLIST
`DOLIST`宏在列表的所有元素上循环, 依次绑定每个元素到一个变量上:
```lisp
(defvar *db* (list 1 2 3 4))
;; 循环输出所有元素
(dolist (cd *db*)
    (format t cd))
```

### LOOP
LOOP宏可以不断执行一个表达式体,最后通过调用`RETURN`来退出

## 文件操作
### WITH-OPEN-FILE
#### 写入
`WITH-OPEN-FILE`宏会打开一个文件,将文件流绑定到一个变量上, 执行一组表达式,然后再关闭这个文件.它还可以保证即便在表达式体求值出错是也可以正确的关闭文件
```lisp
(with-open-file (out "filename"
                :direction :output
                :if-exists :supersede)
    ...
    )
```
紧跟`WITH-OPEN-FILE`的列表并非函数调用二十`WITH-OPEN-FILE`语法的一部分.`out`在文件操作主体中绑定的文件流变量, 紧跟要打开的文件名,后面就是打开文件的选项, `:direction`和`:output`指定了正在打开一个用于写入的文件, `:if-exists :supersede`说明当存在同文件名的文件时想要覆盖已存在的文件

然后可以调用`print`将数据打印到文件当中
```lisp
(with-open-file (out "filename"
                :direction :output
                :if-exists :supersede)
    (print "Hello, world!" out))
```
同时为了确保正常写入, 可以使用`WITH-STANDARD-IO-SYNTAX`来确保影响`print`行为的特定变量可以被设置成他们的标准值
```lisp
(with-open-file (out "filename"
                :direction :output
                :if-exists :supersede)
    (with-standard-io-syntax
        (print "Hello, world!" out)))
```

#### 读取
`WITH-OPEN-FILE`以读文件打开时,不用指定任何选项,因为默认就是`:input`选项,
并且使用`READ`函数从流中读入, 同时可以使用`WITH-STANDARD-IO-SYNTAX`宏来再依次确保`READ`使用标准的环境
```lisp
(with-open-file (in "filename")
    (with-standard-io-syntax
        (setf *test* (read in))))
```


## 列表操作
### getf
`GETF`可以从属性列表(plist)中取出给定字段名称所对应的值

### REMOVE-IF-NOT
`REMOVE-IF-NOT`接受一个返回`NIL`或`T`的函数和一个列表, `REMOVE-IF-NOT`会将列表的每个元素交给这个函数, 返回`T`则保留这个元素, 否则则丢弃这个元素,并返回一个新的列表

返回列表中的偶数
```lisp
(remove-if-not #'evenp '(1 2 3 4 5 6 9 7 8 10))
```
函数`EVENP`接受一个数字, 当数字为偶数是返回真.`#'`用来获取函数,如果没有`#'`的话Lisp会把`evenp`作为一个变量并查找该变量的值.也可以向`REMOVE-IF-NOT`传递匿名函数, 如下使用匿名函数取出列表中的奇数
```lisp
(remove-ifnot #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))
```
`MOD`对两个数进行取模运算,`LAMBDA`用于定义匿名函数, 它后面紧跟形参列表然后是函数体

### REMOVE-IF
`REMOVE-IF`和`REMOVE-IF-NOT`正好相反, 删除匹配函数的项(传递的第一个函数返回T)

### MAPCAR
`MAPCAR`和`REMOVE-IF-NO`一样都是接收一个函数和一个列表, 不同的是`REMOVE-IF-NOT`会删除函数返回`nil`的元素, `MAPCAR`会返回一个函数返回的新列表
```lisp
(mapcar #'(lambda (x) (+ x 1)) (list 1 2 3 4))
--> (2 3 4 5)
```

### REVERSE
`REVERSE`接受一个列表作为参数,并返回一个逆序的新列表

## 函数
### 定义
可以使用`DEFUN`宏来定义函数,紧跟着的是函数名,然后是形参和函数体
```lisp
(defun hello-world()
  (format t "Hello, world!"))
(hello-world)
```

### 匿名函数
可以使用`lambda`宏来定义匿名函数, 紧跟着是形参然后是函数体

### 函数变量
可以将函数当做变量一样传递参数, 使用`#'`获取函数变量

### 关键字形参
函数普通的形参使用一个简单的形参列表, 随后被绑定到函数调用中对应的实参上
```lisp
(defun foo (a b c) (list a b c))
```
关键字形参如下
```lisp
(defun foo (&key a b c) (list a b c))
```
与普通形参区别是形参列表开始出有一个`&key`.但是对关键字形参调用将是截然不同:
```lisp
(foo :a 1 :b 2 :c 3)  --> (1 2 3)
(foo :c 3 :b 2 :a 1)  --> (1 2 3)
(foo :a 1 :b 2)       --> (1 NIL 3)
(foo)                 --> (NIL NIL NIL)
```
显然实参变量的值被绑定到了相应关键字后面的值.并且如果没有指定某个关键字,那么对应的变量将被设置成NIL.
默认的如果不给关键字形参传递实参将会被设置成NIL,也可以在指定形参时设置一个默认值,
```lisp
(defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))
```
简单的关键字被替换成一个包含:形参名 默认值和一个被成为`supplied-p`形参的列表
```lisp
(foo :a 1 :b 2 :c 3)   --> (1 2 3 T)
(foo :c 3 :b 2 :a 1)   --> (1 2 3 T)
(foo :a 1 :c 3)        --> (1 20 3 T)
(foo)                  --> (NIL 20 30 NIL)
```
大家看到所谓的`supplied-p`就是代表这个形参有没有传递实参,如果传递则为True否则则为假

### 变参
`&rest`改变了解析参数的方式.当参数列表里带有`&rest`时, 一个函数或者宏就可以接受任意数量的实参, 它们将被收集到一个单一的列表中, 并成为`&rest`后面的名字所对应的变量的值
```lisp
CL-USER> (defun where (&rest clauses) clauses)
CL-USER> (where :author "Wood.D" :url "www.linuxzen.com")
(:AUTHOR "Wood.D" :URL "www.linuxzen.com")
```



## 宏
宏与函数的主要语法差异在于你需要用`DEFMACRO`而不是`DEFUN`来定义一个宏
```lisp
CL-USER> (defmacro backwards (expr) (reverse expr))
CL-USER> (backwards ("Hello, world!" t format))
Hello, world!
NIL
```
### HOW WORK
当REPL开始对`backwards`表达式求值时, 它认识到`backwards`是一个`宏`名.依次它保持`("Hello, world!" t format)`不被求值而是将这个列表传给`backwards`代码.这个宏将列表用`REVERSE`反转.然后返回`(format t "Hello, world!")`, `REPL`在对其求值

`也可以在表达式前加反引号(\`)来使表达式不求值`
```lisp
CL-USER> `(1 2 3)
(1 2 3)
```
在一个反引用表达式里,任何以`逗号`开始的子表达式都是被求值的
```lisp
`(1 2 (+ 1 2))  -> (1 2 (+ 1 2))
`(1 2 ,(+ 1 2)) -> (1 2 3)
```

`,@`表达式是`,`的变体可以将接下来的表达式的值嵌入到其外围列表中
```lisp
`(and ,(list 1 2 3))  -> (AND (1 2 3))
`(and ,@(list 1 2 3)) -> (AND 1 2 3)
```


### 展开宏
毫无疑问`宏`其实就是用来生成代码, `MACROEXPAND-1`函数就可以精确的看到一个`宏`调用产生那些代码
```lisp
(macroexpand-1 '(backwards ("hi" t format)))
(FORMAT T "hi")
T
```
