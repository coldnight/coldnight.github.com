Title: Lisp 学习笔记---宏
Tags:Lisp, 宏
Category: Lisp
Date: 2013-03-21 09:52

## 宏
前面介绍中其实*宏*的功能就是生成Lisp代码, *宏*就是编译器用来生成代码并随后编译的程序

## 宏展开期和运行期
只有当所有的*宏*都被完全展开并且产生的代码被编译后, 程序才可以实际运行.

*宏* 运行的时期被成为*宏*展开期, 运行期指的是正常的代码(包括*宏*生成的代码) 实际运行阶段

运行期和展开期的代码运行的环境完全不同.*宏*展开期无法访问那些仅存在与运行期的数据.比如*WHEN*宏唯一可用的数据就是源代码.

## DEFMACRO
*宏*使用*DEFMACRO*来定义:
```lisp
(defmacro name (parameter*)
  "Optional documentation string."
  body-form*)
```

和函数一样, 宏由名字、形参列表、可选文档字符串以及Lisp表达式体所构成.但宏并不是直接做事， 而是生成做事的代码。

### 编写宏的步骤
1. 编写示例的宏调用以及它应当展开的代码, 反之依然;
2. 编写从示例调用的参数中生成手写展开是代码；
3. 确保宏抽象不产生"泄漏"

## 示例宏: do-primes
do-primes 迭代在相继的素数上, 先编写两个工具函数:一个检测数字是否为素数, 另一个用来返回大于或等于其实参的下一个素数
```lisp
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))
```
假设需使用下面的宏来循环:
```lisp
(do-primes (p 0 19)
  (format t "~d " p))
```
那么这个宏应该生成下面的代码:
```lisp
(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
  (format t "~d " p))
```

## 宏形参
传递给宏的实参是代表宏调用源代码的Lisp对象

任何宏的第一步工作都是提取出实参所代表的宏调用源代码的Lisp对象用于计算展开式的部分.可以像如下方式定义do-primes宏

```lisp
(defmacro do-primes (var-and-range &rest body)
  (let ((var (first var-and-range))
        (start (second var-and-range))
        (end (third var-and-range))
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
    ,@body))))
```
宏形参列表是所谓的解构形参列表, 解构就是分拆一个结构体. 在上面的例子中形参名将被替换成嵌套的形参列表.嵌套的形参列表中的形参将从绑定到该形参列表的表达式元素中获取其值.列入可以将 _var-and-range_ 替换成一个列表 _(var start end)_ 

宏形参列表的另一个特性是可以使用*&body*作为*&rest*的同义词, *&body*被用来保存一个构成改宏主体的形式列表

根据上面特性 do-primes 可以按照下面定义:
```lisp
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime 1+ ,var)))
        ((> ,var ,end))
     ,@body))
```

## 生成展开式
可以使用*MACROEXPAND-1*函数来查看一个宏生成的lisp代码
```lisp
(macroexpand-1 '(do-primes (n 1 100) (format t "~d " n)))
```

## 堵住漏洞
试想如下代码:
```lisp
(do-primes (n 0 (random 100))
  (format t "~d " n))
```
查看其展开式
```lisp
CL-USER> (macroexpand-1 '(do-primes (p 0 (random 100)) (format t "~d " n)))
(DO ((N (NEXT-PRIME 0)  (NEXT-PRIME (1+ N))))
    ((> N (RANDOM 100)))
  (FORMAT T "~d " N))
T
```
上面给代码*RANDOM*将在每次循环终止测试时被求值, 也就是说循环可能被随机终止,为了修复这个漏洞, 我们可以在展开开始对 _end_ 进行求值
```lisp
(defmacro do-primes ((var start end) &body body)
  `(do ((ending-value ,end)
        (,var (next-prime ,start) (next-prime (+1 ,var))))
       ((> ,var ending-value))
    ,@body))
```
但书上说上面的形式违反了什么狗屁的*最少惊动原则*, 求值顺序与宏调用中的顺序相反, 所以应该改成下面:

```lisp
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (+1 ,var)))
        (ending-value ,end))
       ((> ,var ending-value))
    ,@body))
```
但是新的漏洞又出来了, 考虑如下面两个调用
```lisp
(do-primes (ending-value 0 10)
  (print ending-value))

(let ((ending-value 0))
  (do-primes (p 0 10)
    (incf ending-value p))
  ending-value)
```
使用*MACROEXPAND-1*展开第一个
```lisp
CL-USER> (macroexpand-1 '(do-primes (ending-value 0 10) (print ending-value)))
(DO
 ((ENDING-VALUE (NEXT-PRIME 0) (NEXT-PRIME (1 ENDING-VALUE)))
  (ENDING-VALUE 10))
 ((> ENDING-VALUE ENDING-VALUE)) (PRINT ENDING-VALUE))
T
```
上面的代码某些lisp可能因为 _ending-value_ 作为变量名在统一*DO*循环中被用了两次而拒绝上面的代码. 如果没有拒绝代码也将无限循环下去
第二个展开车给你下面的代码:
```lisp
(let ((ending-value 0))
  (do ((p (next-prime 0) (next-prime (1+ p)))
       (ending-value 10))
      ((>p ending-value))
    (incf ending-value p))
  ending-value)
```
上面代码循环开有*LET*所创建的 _ending-value_ 绑定被*DO*内部的同名变量所掩盖, 外层的 _ending-value_ 将不改变, *DO*循环内的 _ending-value_ 将递增,从而造成无限循环

### 解决
*包*从某种意义上能解决, 但有一个更好的解决方案:
函数*GENSYM*在每次被调用时返回唯一符号.所以 do-primes 可以改成如下:
```lisp
(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))
```

上面的形式将被展开成如下:
```lisp
CL-USER> (macroexpand-1 '(do-primes (ending-value 0 10) (print ending-value)))
(DO
 ((ENDING-VALUE (NEXT-PRIME 0) (NEXT-PRIME (1+ ENDING-VALUE))) (#:G11009 10))
 ((> ENDING-VALUE #:G11009)) (PRINT ENDING-VALUE))
T
```

综上所述, 要定义好一个没有以上基类漏洞的宏, 只须遵循下面的规则即可:
* 除非特殊理由, 否则需要将展开式中的任何子表达式放在一个位置上, 使其求值顺序与宏调用的子形式相同
* 除非特殊理由, 否则需要确保子形式仅被求值依次, 放在是在展开式中创建变量来只有求值参数形式所得到的值, 然后在展开式中所有需要用到该值的地方使用这个变量
* 在宏展开期使用 *GENSYM* 来创建展开式中所用到的变量名

## 用宏生成宏
宏不仅在编译函数的时候使用, 也可以为编写一个宏, 比如上面的 do-primes需要使用*LET*形式开始然后引入了一些变量来保存宏展开过程中用到的生成符号.我们可以编写宏来将其抽象掉:
```lisp
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))
```
然后可以使用上面的宏来重新定义 do-primes
```lisp
(defmacro do-primes ((var start end) &body body)
  (with-gensysms (ending-value-name)
   `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
         (,ending-value-name ,end))
        ((> ,var ,ending-value-name))
      ,@body)))
```
