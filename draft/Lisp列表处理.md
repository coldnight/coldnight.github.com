Title: Lisp 学习笔记:列表处理
Date: 2013-03-37 15:57
Category: Lisp
Tags: Lisp, 列表, 学习, 笔记

## 点对单元
点对单元是构成列表的基本元素
*CONS*用来创建点对单元, CONS接受两个实参并返回一个含有连个值的新点对单元.
```lisp
(cons 1 2)  --> (1 . 2)
```
点对单元的两个值分别成为*CAR*和*CDR*, 它们同时也用来访问两个值的函数名.
```lisp
(car (cons 1 2))   --> 1
(cdr (cons 1 2))   --> 2
```
*CAR*和*CDR*也能够支持*SETF*的位置
```lisp
(defparameter *cons* (cons 1 2))
*cons*        --> (1 . 2)
(setf (car *cons*) 10)   --> 10
*cons*       --> (10 . 2)
(setf (cdr *cons*) 20)   --> 20
*cons*       --> (10 . 20)
```

## 列表
列表是通过将点对以链接状态链接在一起构成的.列表的元素被保存在点对的*CAR*中, 而后续的点对的链接则被保存在*CDR*中.链上最后一个单元的*CDR*为*NIL*
```lisp
(cons 1 nil)         --> (1)
(cons 1 (cons 2 nil))  --> (1 2)
(cons 1 (cons 2 (cons 3 nil)))  --> (1 2 3)
```

*LIST*函数可以在背后为你构建一些点对单元并将它们链接在一起
```lisp
(list 1)       --> (1)
(list 1 2)     --> (1 2)
(list 1 2 3)   --> (1 2 3)
```
对列表而言*FIRST*和*REST*分别是*CAR*和*CDR*的同义词
```lisp
(defparameter *list* (list 1 2 3 4))
(first *list*)       --> 1
(rest *list*)        --> (2 3 4)
(first (rest *list*)) --> (2)
```
因为点对单元可以保存任何类型的值, 所以也可以保存列表, 并且单一列表可以保存不同类型的对象
```lisp
(list "foo" (list 1 2) 10)  --> ("foo" (1 2) 10)
```
因此列表可以用来表示任意深度与复杂堵的数

## 函数式编程和列表
函数式编程的本质在于, 程序完全由没有副作用的函数组成.列表是可变的.但列表可以当作函数式数据类型来对待,只要将其值视为是由它们包含的元素所决定即可

## 破坏性操作
修改已有的对象的操作被成为是 _破坏性的_.在函数式编程中, 改变一个对象的状态相当于"破坏"了它.

有两种相当不同的破坏性操作: _副作用性_ 操作和 _回收性_ 操作

副作用性操作是那些专门利用其副作用的操作.所有对*SETF*的使用都是破坏性的, 还包括*VECTOR-PUSH*或*VECTOR-POP*这类底层使用*SETF*来修改已有对象状态的函数 ,它们没打算被用于函数式风格编写的代码中, 因此不该用函数式术语来描述它们.

如果将非函数式的副作用性操作和那些返回结构共享结果的函数混合使用, 就需要小心修改共享结构
```
(defparameter *list-1* (list 1 2))
(defparameter *list-2* (list 3 4))
(defparameter *list-3* (append *list-1* *list-2*))

(setf (first *list-2*) 0) --> 0
*list-2*                  --> (0 4)
*list-3*                  --> (1 2 0 4)
```
在共享结构中, *list-2*中的第一个点对单元也是*list-3*中的第三个点对单元, 所以更改了*list-2*也改变了*list-3*, 对*list-2*的*FIRST*进行*SETF*改变了该点对单元中的*CAR*部分的值


另一种破坏操作即回收操性操作, 其本来就是用于函数式代码中的. 它们的副作用仅是一种优化手段.它们在构造结果时会重用来自它们实参的特定点对单元.回收性函数将点对单元作为原材料来重用, 如果必要它将修改其*car*和*CDR*来构造想要的结果.只有调用回收性函数之后不需要原先列表的情况下, 回收函数才可以被安全地使用.

*REVERSE*的对应的回收性函数是*NREVERSE*, _N_的含义是 _non-consing_, 意思它不需要分配任何新的点对单元

*NCONC*是*APPEND*的回收性版本;而*DELETE* *DELETE-IF* *DELETE-IF-NOT*和*DELETE-DUPLICATES*则是序列函数的*REMOVE*家族的回收性版本

*NCONC*每一个非空列表的最后一个点对电源的*CDR*设置成指向下一个非空列表的第一个点对单元, 然后返回第一个列表

## 列表处理函数
Common Lisp 提供了*SECOND*到*TENTH*类似*FIRST*的函数哟哪里改获取相应的元素

*NTHCDR*接受一个索引和一个列表, 并返回调用*CDR* _n_ 次的结果
```lisp
(nthcdr 3 (1 2 3 4 5))   --> (4 5)
```

28个符号*CAR/CDR*函数则是另一个不时会用到的函数家族.每个函数都是通过将由最多四个A和D组成的序列放在C和R之前来命名, 其中每个A代表对*CAR*的调用而每个D则代表对*CDR*的调用
```lisp
(caar list)  === (car (car list))
(cddr list)  === (cdr (cdr list))
(cadadr list) === (car (cdr (car (cdr list))))
```
但要注意, 这其中许多函数仅当应用与含有其他列表的列表时才有意义.
```lisp
(caar (list 1 2 3))                         --> error
(caar (list (list 1 2) 3))                  --> 1
(cadr (list (list 1 2) (list 3 4)))         --> (3 4)
(caadr (list (list 1 2) (list 3 4)))        --> 3
```

*FIRST-THENTH* *CAR* *CADR* 也可以用作*SETF*的位置


## 映射
*MAPCAR*最接近*MAP*函数, 它总是返回一个列表, 所以它并不要求*MAP*所要求的结果类型形参, *MAPCAR*和*MAP*一样接受一个函数和一个(含一个)以上的列表实参, 函数被应用在列表实参的相继元素上, 每次函数的应用会从列表中接受一个元素.函数调用的结果都被收集在一个新列表中
```lisp
(mapcar #'(lambda (x) (* 2 x)) (list 1 2 3))   --> (2 4 6)
(mapcar #'+ (list 1 2 3) (list 10 20 30))    --> (11 22 33)
```

*MAPLIST*也和*MAPCAR*较为相似, 它们之间的区别在于*MAPLIST*传递给函数的不是列表元素而是实际的点对单元
