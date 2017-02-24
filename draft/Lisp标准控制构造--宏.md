Title: Lisp学习笔记---宏: 标准控制构造
Tags: Lisp, 宏, 标准控制构造, 学习, 笔记
Category: Lisp
Date: 2013-03-20 16:43

## 概述
### IF
所有`宏`都定义了自己的语法, 它们能够解决那些被传递的`S-表达式`如何能转换成Lisp形式.核心语言有了`宏`, 就能构造出新的语法, 诸如`WHEN` `DOLIST`和`LOOP`这样的控制构造以及`DEFUN`和`DEFPARAMETER`这样的定义形式.

## WHEN和UNLESS
最基本的条件执行形式由`IF`特殊操作符提供, 其基本形式是: 如果`x`成立, 那么执行`y`, 否则执行`z`
```
(if condition then-form [else-form])
```
### PROGN
但是 _then-form_ 和 _else-form_ 都被限制必须是单一的Lisp形式, 但是特殊操作符 *PROGN* 可以按顺序执行任意数量的表达式并返回最后一个表达式的值
```lisp
(if (spam-p current-message)
  (progn
    (file-in-spam-folder current-message)
    (update-spam-database current-message)))
```

### WHEN
上面的`IF`语句令人厌烦, 因为 *IF* 为什么执行多条表达式要加上 *PROGN*, 之前我们了解到`宏`就是用来生成代码, 而 *WHEN* 标准宏就是用来生成上面的`IF`表达式的代码, 可以使用*WHEN* 执行多条表达式当第一个表达式通过的时候, 上面的代码可以写成如下:
```lisp
(when (spam-p current-message)
  (file-in-spam-folder current-message)
  (update-spam-database current-message))
```

*WHEN* 宏是通过如下方式定义:
```lisp
(defmacro when (condition &rest body)
  `(if ,condition (progn ,@body))) ; `使表达式不进行求值, ,@在`的情况下求值并将值放到外围列表中
```

### UNLESS

*UNLESS* 宏去想法的条件, 只有当条件为假时才求值其形式
```lisp
(defmacro unless (condition &rest body)
  `(if (not ,condition) (progn ,@body))) ; ,在`的情况下使表达式强制求值
```

## COND
*IF*特殊操作符在遇到多重分支条件语句时再一次变得丑陋不堪
```lisp
(if a
  (do-x)
  (if b
    (do-y)
    (do-z)))
```

如果需要在 _then-form_ 中包括多个表达式, 就需要用到*PROGN*, 但是代码会更加丑陋.*COND*宏用于表达多重分支条件:
```lisp
(cond
  (test-1 form*)
        .
        .
  (test-N form*))
```
可以像下面替换前面写的*IF*表达式
```lisp
(cond (a (do-x))
      (b (do-y))
      (t (do-z)))
```

## AND OR和NOT
*NOT*是一个函数而不是一个宏, 它接受单一参数并对其值去翻,*AND*和*OR*则是宏, 它们对任意数量的子表达式逻辑与和逻辑或操作.支持`短路`特性(*AND*如果取到一个`NIL`的值则立即停止返回`NIL`, 如果所有子表达式都返回非*NIL*则返回最后一个表达式的值.*OR*只要有子表达式返回非*NIL*那么它将返回当前子表达式的值)

## 循环
Lisp的25个特殊操作符中没有一个能够直接支持结构话循环, 所有Lisp循环控制结构都是构建在一对提供原生goto机制的特殊操作符之上的宏

*DO*提供了一种基本的结构化循环构造, 而*GOLIST*和*DOTIMES*则提供了两种易用确不那么通用的构造, *LOOP*宏提供了一种成熟的微型语言

### DOLIST
*DOLIST*在一个列表的元素上循环操作, 使用一个一次持有列表中所有后继元素的变量来执行循环体
```lisp
(dolist (var list-form)
  body-form*)
```
当循环开始时, _list-form_ 被求值一次以参省一个列表. 然后循环体在列表的每一项上求值一次, 同时用变量 _var_ 保存当前项的值
```lisp
CL-USER> (dolist (x '(1 2 3)) (print x))
1
2
3
NIL
```
*RETURN* 可以结束之前终端一个*DOLIST*循环

### DOTIMES
*DOTIMES* 是用于循环技术的高级循环构造
```lisp
(dotimes (var count-form)
  body-form*)
```
*RETURN*同样可以提前结束*DOTIMES*循环

### DO
*DO* 允许绑定任意数量的变量, 并且变量在每次循环中的改变方式也是完全可控的也可以测试条件来决定何时终止循环, 并提供一个表达式, 在循环结束时进行求值来为*DO*表达式整体生成一个返回值
```lisp
(do (variable-definition*)
    (end-test-form result-form*)
  statement*)
```
每一个 _variable-definition_ 引入了一个将存在与循环体作用域之内的变量.单一变量定义完整形式是一个含有三个元素的列表
```
(var init-form step-form)
```
_init-form_ 将在循环开始时被求值并将结果值绑定到变量 _var_ 上, 在循环的每一个后续迭代开始之前, _step-form_ 将被求值并把新值分配给 _var_, 并且是可选的, 如果不给那么变量在循环过程中将保持不变, 如果 _init-form_ 没有给出那么变量将被绑定到 *NIL*, 另外和*LET*一样可以将一个含有名字的列表简化成一个简单的变量名来使用

每一次迭代开始时以及所有循环变量都被指定新值后, _end-test-form_ 会被求值.只要其值为*NIL*,  迭代过程就会*继续*, 依次求值 _statement_

当 _end-test-form_ 求值为真时, _result-form_ (结果形式)将被求值, 最后一个结果形式的值将被作为*DO*表达式的值返回

```lisp
(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
  ((= 10 n) cur))
```

### 强大的LOOP
*LOOP*宏可以提供一种更容易表达的方式, 它有两大类: _简化的_ 和 _扩展的_

简化的极其简单, 就是一个不绑定任何变量的无限循环
```lisp
(loop
  body-form*)
```
整个循环不停的迭代, 只要之用*RETURN*来进行终止

扩展的*LOOP*则是完全不同的庞然大物.

下面是一个地道的*DO*循环, 它将1到10的数字放到一个列表中
```lisp
(do ((nums nil) (i 1 (1+ i)))
    ((> i 10) (nreverse nums))
  (push i nums)) -> (1 2 3 4 5 6 7 8 9 10)
```
用*LOOP*版本如下:
```lisp
(loop for i from 1 to 10 collecting i) --> (1 2 3 4 5 6 7 8 9 10)
```

用*LOOP*对前10个平方数求和
```lisp
(loop for x from 1 to 10 summing (expt x 2))
```

用*LOOP* 计算第11个Fibonacci Sequence
```lisp
(loop for i below 10
      and a = 0 then b
      and b = 1 then (+ b a)
      finally (return a))
```

*LOOP*有:across, and, below, collecting, counting, finally, for, from, summing, then和to等循环关键字, 它们存在表明当前正在使用扩展的*LOOP*
