Title: Lisp学习笔记: 广义函数
Category: Lisp
Date: 2013-03-29 16:40
Tags: Lisp, 学习, 笔记, 广义函数


广义函数就是通过使用不同的参数类型来实现可以同名函数的机制, 类似C++的方法重载

## 定义
*DEFGENERIC*用于定义广义函数, 该定义不产生任何实际代码, 并且它可以接受任何对象作为参数.

广义函数的实际实现是由方法提供, 每一个方法提供广义函数用于特定参数类的实现, 方法通过特化那些由广义函数所定义的比较参数, 来表达它们可以处理的参数类型

## DEFGENERIC
*DEFGENERIC*用于定义广义函数, 与*DEFUN*相似. *DEFGENERIC*的形参列表指定了那些定义在该广义函数上的所有方法都必须接受的参数. *DEFGENERIC*应当总是带有的选项是 _:documentation_, 下面是一个广义函数:
```lisp
(defgeneric withdraw (account amount)
  (:documentation "Withdraw"))
```

## DEFMETHOD
*DEFMETHOAD*用来定义实现广义函数的方法, 方法的形参列表必须与它的广义函数保持一致, 下面是一个实现了withdraw广义函数的方法
```lisp
(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
    (error "Account overdrawn."))
  (decf (balance account) amont))
```

*CALL-NEXT-METHOD*是广义函数机制的一部分,用于租客可应用的方法.
