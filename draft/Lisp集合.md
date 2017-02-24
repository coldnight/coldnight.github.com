Title: Lisp 学习笔记 ---- 集合
Date: 2013-03-25 11:29
Tags: Lisp, 集合, hash, array, list
Category: Lisp

## 向量
向量是Common Lisp基本的整数索引集合, 它们分为两大类: 定长向量和变长向量

### 定长向量
定长向量是一个块数据头以及一段保存向量元素的连续内存区域

可以使用*VECTOR*来生成含有特定值的定长向量, 改函数接受任意数量的参数并返回一个新分配的含有那些参数的定长向量.
```lisp
(vector)  --> #()
(vector 1) --> #(1)
(vector 1 2) --> #(1 2)
```
*#(...)* 是向量的字面表达式, 该语法可以使用*PRINT*打印并用*READ*读取.也可以使用*#(...)*在代码中添加字面向量, 但修改字面对象的后果并不明确, 因此应当总是使用*VECTOR*或更为通用的函数*MAKE-ARRAY*来创建打算修改的向量

*MAKE-ARRAY*比*VECTOR*更加通用, 因为它可以用来创建任何维度的数组以及定长和变量向量. *MAKE-ARRAY*的一个必要参数是一个含有数组维度的列表, 创建有初始化的向量可以传递 _:initial-element_参数:
```lisp
(make-array 5 :initial-element nil) --> #(NIL NIL NIL NIL NIL)
```


### 变长向量
变长向量抽象了实际存储, 允许向量随着元素的增加和移除和增大和减小

*MAKE-ARRAY*也可以传递一个 _:fill-pointer_ 实参用来创建变长向量:
```lisp
(make-array 5 :fill-pointer 0)   --> #()
```
因为上面填充指针是0, 所以看起来是个空的

函数*VECTOR-PUSH*在可变向量的填充指针当前值上添加一个元素并将填充指针递增依次, 返回新元素被添加未知的所以.

函数*VECTOR-POP*返回最近推入的项, 并在递减填充指针, 从而删除此项
```lisp
(defparameter *x* (make-array 5 :fill-pointer 0))
(vector-push 'a *x*)  -> 0
*x*  -> #(A)
(vector-push 'b *x*)   -> 1
*x* -> #(A B)
(vector-pop *x*)  --> B
*x* -> #(A)
(vector-pop *x*)  --> A
*x* -> #()
```

但是上面的向量并不是完全变长的.向量`*x*`只能保存最多5个元素.可以向*MAKE-ARRAY* 传递 _:adjustable_关键字参数来创建变长向量
```lisp
(make-array 5 :fill-pointer 0 :adjustable t)  --> #()
```
*VECTOR-PUSH-EXTEND*可以像*VECTOR-PUSH*那样工作, 但是*VECTOR-PUSH-EXTEND*可以向一个已满的向量中推入元素, 它能自动扩展数组

## 向量子类行
*MAKE-ARRAY*通过添加一个关键字参数 _:element-type_ 来创建指定子类型的向量, 比如可以传递*CHARACTER*作为 _:element-type_ 来创建字符串
```lisp
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character) --> ""
```

也可以传递*BIT*符号作为 _:element-type_ 的类型来创建位向量

## 作为序列的向量
*LENGTH*可以返回一个序列的茶馆难度;*ELT*(element) 允许通过一个整数索引访问个别元素, 并在超出索引边界时报错
```lisp
(defparameter *x* (vector 1 2 3))

(length *x*) --> 3
(elt *x* 0)  --> 1
(elt *x* 1)  --> 2
(elt *x* 3)  --> error
```
*ELT*也是一个支持*SETF*的位置, 因此可以像下面这样设置一个特定元素的值
```lisp
(setf (elt *x* 0) 10)
*x*  --> #(10 2 3)
```

## 序列迭代函数
*COUNT* 返回续例中某项出现的次数
```lisp
(count 1 #(1 2 1 1 2 1))   --> 4
```
*REMOVE* 返回被移除某项后的序列
```lisp
(remove 1 #(1 2 1 1 2 1))  --> #(2 2)
(remove 1 '(1 2 1 1 2 1))  --> (2 2)
(remove #\a "foobarbaz")   --> "foobrbz"
```
*SUBSTITUTE* 某项被新项替换后的序列
```lisp
(substitute 10 1 #(1 2 1 1 2 1))  --> #(10 2 10 10 2 10)
(substitute #\x #\b "foobarbaz")  --> "fooxarxaz"
```
*FIND* 返回序列中的某项, 没有则返回NIL
```lisp
(find 1 #(1 2 1 1 2 1))  --> 1
(find 10 #(1 2 1 1 2 1))  --> NIL
```
*POSITION* 返回序列中某项的索引, 某有则返回NIL
```lisp
(position 1 #(1 2 1 1 2 1))  --> 0
```

以上函数都接收下面的关键字形参来修改函数的默认行为
_:test_ 关键字来传递一个接受两个参数并返回一个布尔值的函数, 它将使用改函数代替默认的等价性测试*EQL*来比较序列中的每个元素

_:key_关键字可以传递函数参数, 并将每一个元素作为实参传递给函数, 并将替代元素和函数返回值进行比对
```lisp
;; 统计序列中等于"foo"的元素个数
(count "foo" #("foo" "bar" "baz") :test #'string=)   --> 1
;; 查找序列中元素第一个值为'c的元素
(find 'c #((a 10) (b 20) (c 30) (d 40)) :key #'first) --> (C 30)
```
_:start_ 和 _:end_ 参数提供边界指示, 将函数效果限制在序列实参特定的子序列上
_:end_ 为*NIL*或省略它则一直到序列结束

_:from-end_ 参数将使函数在序列结尾处开始检查每一个元素
```lisp
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first)   -> (A 10)
;; 从结尾出查找
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first :from-end t) --> (A 30)
```

_:count_ 用于指定有多少元素被移除和替换.和 _:from-end_参数一起使用将影响*REMOVE*和*SUBSTITUTE*的行为
```lisp
(remove #\a "foobarbaz" :count 1)              --> "foobrbaz"
(remove #\a "foobarbaz" :count 1 :from-end t)  --> "foobarbz"
```

## 高阶函数变体
对于上面的函数 Common Lisp都提供了两种 _高阶函数变体_:它们接受一个将在每个序列元素上调用的函数, 用此来代替各项参数.

一组变体被命名为基本函数相同的名字并带有一个追加的 *-IF*, 这些函数用于计数、查找、移除已经替换序列中那些函数参数返回真的元素。

另一类是以*-IF-NOT*后缀的命名并计数、查找、移除已经替换序列中那些函数参数返回真的元素。
```lisp
(count-if #'evenp #(1 2 3 4 5))       --> 2
(count-if-not #'evenp #(1 2 3 4 5))   --> 3
(position-if #'digit-char-p "abcd0001") --> 4
(remove-if-not #'(lambda (x) (char= (elt x 0) #\f))
  #("foo" "bar" "baz" "foom"))   --> #("foo" "foom")
```

除了 _:test_这些*-IF*和*-IF-NOT*变体都接受和它们原始版本相同的关键字参数

*REMOVE*函数家族支持第四个变体*REMOVE-DUPLICATES* 它将为序列去重
```lisp
(remove-duplicates #(1 2 1 2 3 1 2 3 4))  --> #(1 2 3 4)
```

## 整个序列上的操作
*COPY-SEQ*和*REVERSE*都接受单一的序列参数并返回一个相同类型的新序列

*CONCATENATE* 创建一个简爱嗯任意序列链接在一起的新序列,它必须限制指定产生目标序列的类型
```lisp
(concatenate 'vector #(1 2 3) '(4 5 6))   --> #(1 2 3 4 5 6)
(concatenate 'list #(1 2 3) '(4 5 6))     --> (1 2 3 4 5 6)
(concatenate 'string "abc" '(#\d #\e #\f)) --> "abcdef"
```

## 排序与合并
*SORT*和*STABLE-SORT*提供了两种序列排序方式.它们都就诶受一个序列和一个由两个参数组成谓词, 返回改序列排序后的版本
```lisp
(sort (vector "foo" "bar" "baz") #'string<) --> #("bar" "baz" "foo")
```
*STABLE-SORT*可以保证不会重拍任何被该谓词视为等价的元素, *SORT*值保证结果是已排序的并可能重拍等价元素

这两个函数都是 _破坏性_函数, 也就是会对源序列进行修改

这两个函数也接受关键字参数 _:key_, 它和其他序列函数的 _:key_参数一样, 应该是一个被用来从序列元素中抽取出传给排序谓词值的函数

*MERGE* 接受两个序列和一个函数, 并返回按照该函数合并这两个序列所产生的序列, 它同样接受一个 _:key_ 参数.它的第一个参数必须用来指定生成序列类型的描述符
```lisp
(merge 'vector #(1 3 5) #(2 4 6) #'<)   --> #(1 2 3 4 5 6)
(merge 'list #(1 3 5) #(2 4 6) #'<)     --> (1 2 3 4 5 6)
```

## 子序列操作
*SUBSEQ* 返回序列中从一个特定索引开始并延续要一个特定终止索引或结尾出的子序列
```lisp
(subseq "foobarbaz" 3)    --> "barbaz"
(subseq "foobarbaz" 3 6)  --> "bar"
```
*SUBSEQ*也支持*SETF*, 但不会扩大或缩小一个序列.
```lisp
(defparameter *x* (copy-seq "foobarbaz"))
(setf (subseq *x* 3 6) "xxx")   ; 子序列和新值具有相同长度
*x*  --> "fooxxxbaz"
(setf (subseq *x* 3 6) "abcd")  ; 新值太长, 其他字符被呼略
*x*  --> "fooabcbaz"
(setf (subseq *x* 3 6) "xx")    ;新值太短, 只替换现有的新值
*x*  --> "fooxxrbaz"
```

*FILL*函数将一个序列的多个元素设置到单个值上
```lisp
(fill '(1 2 3) 4)  --> (4 4 4)
```
_:start_ 和 _:end_关键字参数可以将效果限制在给定的子序列上

*SEARCH*函数可以像*POSITION*那样工作, 不过第一个参数是序列而不是一个单独的项
```lisp
(position #\b "foobarbaz") --> 3
(search "bar" "foobarbaz") --> 3
```
*MISMATCH*可以找出两个带有相同前缀的序列首次分叉的位置
```lisp
(mismatch "foobarbaz" "foom") --> 3
```
*MISMATCH*也接受 _:key_和 _:test_还有 _:from-end_, _:start1_ 、_:end1_ 、 _:start2_和 _:end2_参数则用来指定两个序列中的子序列

## 序列谓词
*EVERY*、*SOME*、*NOTANY*和*NOTEVERY*, 它们在序列上迭代测试一个布尔谓词

*EVERY*在谓词失败时返回假

*SOME* 返回有谓词所返回的第一个非NIL值

*NOTANY* 在谓词满足时返回假

*NOTEVERY* 在谓词失败时返回真

## 序列映射函数
*MAP* 和序列谓词函数一样, 接受一个 _n-_ 参数函数和n个序列.并返回一个新序列, 它同样需要被告知其所创建序列的类型
```lisp
(map 'vector #'* #(1 2 3 4 5) #(10 9 8 7 6)) --> #(10 18 24 28 30)
```

*MAP-INFO*和*MAP*相似, 但它将结果防止在一个作为第一个参数传递的序列中, 如果序列茶馆难度不同, *MAP-INFO*将只影响与最短序列数俩个你相当的元素

*REDUCE*函数映射在单个序列傻瓜你, 先将将一个需要两个参数的函数应用到序列的最初两个元素上, 再将函数返回值和序列后续元素用于该函

```lisp
(reduce #'+ #(1 2 3 4 5 6 7 8 9 10)) --> 55
```
*REDUCE* 也接受完整关键字参数(:key :from-end :start :end), 以及*REDUCE*专用的 _:init-value_ 用于指定一个值, 放在序列的第一个元素上参与运算(如果指定了 _:from-end_, 那么值将放置在序列最后)

## 哈希表
*MAKE-HASH-TABLE* 用来创建一个哈希表, 不带参数可以创建一个哈希表, 其认定两个键等价使用*EQL*作为比对. 如果想用字符串作为键, 应该使用*EQUAL*作为比对键,需要在创建函数时给它的 _:test_关键字函数传递 *EQUAL*作为参数

*GETHASH* 提供了对哈希表元素的访问, 同时支持*SETF*, 
```lisp
(defparameter *h* (make-hash-table))
(gethash 'foo *h*)  --> NIL
(setf (gethash 'foo *h*) 'quux)
(gethash 'foo *h*)   -->QUUX
```
*GETHASH* 实际上返回了两个值: 主值是保存给定键下的值或NIL, 从值是一个布尔值, 用来指示改键在哈希表中是否存在

*MULTIPLE-VALUE-BIND*宏来利用*GETHASH*的而外返回

*REMHASH*接受同样的参数用来移除指定项

*CLRHASH*用来完全清楚哈希表中的所有键值对

## 哈希表迭代
*MAPHASH*和*MAP*函数相似, 它接受一个接受两个参数的函数和一个哈希表, 并在哈希表的每一个键值对上调用依次该函数
```lisp
(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) *h*)
```
