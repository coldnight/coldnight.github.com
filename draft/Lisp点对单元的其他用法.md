Title: 点对单元的其他用法
Date: 2013-03-28 15:33
Category: Lisp
Tags: Lisp, 学习, 笔记, 点对单元, 用法

## 树
数结构是同时跟随*CAR*和*CDR*的引用, 只要它们指向其他点对单元, 树中的值就是该树结构中所有点对单元引用的非点对单元的值

## 集合
集合也可以用点对单元实现.*ADJOIN*函数用来构造集合, 它接受一个项和一个代表集合的列表并返回另一个代表集合的列表, 其中含有该项和原有集合中的所有项.它会扫描该列表, 如果没找到该项, 那么它会创建一个保存该项的新点对单元, 并让其指向原先的列表并返回它.否则, 它返回原先的列表

*ADJOIN*也接受 _:key_和 _:test_关键字参数, 它们用于检测该项是否存在与原先类表中.

*ADJON*不会影响原先列表, 如果打算修改将*ADJOIN*返回的值赋值到该列表所来自的位置上. *PUSHNEW*修改宏可以字段自动做到这点
```lisp
(defparameter *set* ())       -->  *SET*
(adjoin 1 *set*)              --> (1)
*set*                         --> NIL ;; 因为不会修改源列表
(setf *set* (adjion 1 *set*)) --> (1)
*set*                         --> (1)
(pushnew 2 *set*)             --> (2 1)
(pushnew 2 *set*)             --> (2 1)
```

## 查询表: alist 和 plist
点对单元还可以用来构建表将键映射到值上.有两类居于点对的查询表经常会用到:关联表也称为 _alist_以及属性表也称为 _plist_

alist和plist不能用于大型表(使用哈希表替代)

### alist
alist是一种数据结构, 它能将一些键映射到值上, 同时也支持反向查询, 并且给定一个值时, 它还能找出对应的键.alist也支持添加键/值映射来掩盖已有的映射, 并且当这个映射以后被移除时原先的映射还可以再次暴露出来

alist的主查询函数是*ASSOC*, 其接受一个键和一个alist并返回第一个*CAR*匹配改建的点对单元
```lisp
(assoc 'a '((a . 1) (b . 2) (c . 3)))   --> (A . 1)
```
默认指定的键使用*EQL*与alist中的键比较, 如果想用字符串作为键, 可以这样写:
```lisp
(assoc "a" '(("a" . 1) ("b" . 2) ("c" . 3)) :test #'string=) --> ("a" . 1)
```
*ASSOC*搜索列表时会从列表的前面开始扫描, 因此alist中的一个键值对可以遮盖列表中的所有后面带有相同键的其他键值
```lisp
(assoc 'a '((a . 10) (a . 1) (b . 2) (c . 3)))   --> (A . 10)
```
*ACONS*可以向一个alist的前面添加键值对
```lisp
(acons 'new-key 'new-value alist)
```
*PAIRLIS*可以从两个分开的键和值列表中构造出一个alist, 返回的alist可能与原先列表相同或相反的键值对
```lisp
(pairlis '(a b c) '(1 2 3))  --> ((C . 3) (B . 2) (A . 1))
```

### plist
从结构上讲 plist只是一个正常的列表, 其中带有交替出现的键和值作为列表中的值

plist不像alist那样灵活.plist仅支持一种基本查询操作, 即函数*GETF*, 其接受一个plist和一个键, 返回所关联的值或*NIL*, *GETF*也接受可选的第三个参数, 它将在键没有被找到的时候代替*NIL*作为返回值

*GETF*总是使用*EQ*来测试所提供的键是否匹配plist中的键.所以不能使用数据和字符作为plist中的键

可以将*SETF*与*GETF*一起使用来设置给定键的关联值值, *SETF*对待*GETF*时可以将*GETF*的第一个参数视为位置, 这样可以使用*GETF*的*SETF*来向一个已有的plist中添加新的键值对

*REMF*宏可以从plist中移除一个键/值对

*GET-PROPERTIES*能高效的从单一的plist中抽取多个值.它接受一个plist和一个需要被搜索的键的列表

## DESTRUCTURING-BIND
*DESTRUCTURING-BIND*宏提供一种解构任意列表的方式, 这类似与宏形参列表分拆它们的参数列表的方式:
```lisp
(destructuring-bind (parameters*) list
    body-form*)
```
该参数列表可以包含宏参数列表中支持的任何参数类型, 比如*&optional*和*&rest*与*&key*参数下面是一些例子
```lisp
(destructuring-bind (x y z) (list 1 2 3)
  (list :x x :y y :z z))   --> (:X 1 :Y 2 :Z 3)

(destructuring-bind (x y z) (list 1 (list 2 20) 3)
  (list :x x :y y :z z))   --> (:X 1 :Y (2 20) :Z 3)

(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2 20) 3)
  (list :x x :y1 y1 :y2 y2 :z z))  --> (:X 1 :Y1 2 :Y2 20 :Z 3)

(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2) list 3)
  (list :x x :y1 y1 :y2 y2 :z z))  --> (:X 1 :Y1 2 :Y2 NIL :Z 3)

(destructuring-bind (&key x y z) (list :x 1 :y 2 :z 3)
  (list :x x :y y :z z))   --> (:X 1 :Y 2 :Z 3)

(destructuring-bind (&key x y z) (list :z 1 :y 2 :x 3)
  (list :x x :y y :z z))   --> (:X 3 :Y 2 :Z 1)
```

还有一种支持宏的参数就是*&whole*.如果它被指定, 它必须是参数列表中的第一个参数, 并且它会绑定在整个列表形参上,在一个*&whole*参数之后, 其他参数可以像通常那样出现并且将像没有*&whole*参数存在那样抽取列表中指定部分
```lisp
(destructuring-bind (&whole whole &key x y z) (list :z 1 :y 2 :x 3)
  (list :x x :y y :z z :whole whole))
--> (:X 3 :Y 2 :Z 1 :WHOLE (:Z 1 :Y 2 :X 3)
