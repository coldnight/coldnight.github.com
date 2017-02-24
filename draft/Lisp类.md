Title: Lisp学习笔记: 面向对象
Category: Lisp
Tags: Lisp, 学习, 笔记, 面向对象, 类
Date: 2013-03-29 16:54

## DEFCLASS
*DEFCLASS*宏用来创建用户定义类.类关联行为是通过定义广义函数和特化在该类上的方法觉得的, *DEFCLASS*只负责将类定义为一种数据类型

类作为数据类型的三个方面:

* 名字
* 与其他类的关系
* 构成该类实例的那些槽的名字

*DEFCLASS*的基本形式
```lisp
(defclass name (direct-superclass-name*)
  (slot-specifier*))
```

*MAKE-INSTANCE*可以将类实例化, 它使用类名作为参数

借类如果不指定将继承*STANDARD-OBJECT*类

# 槽
*DEFCLASS*形式大部分是由槽描述符的列表组成的.每个槽描述符定义的槽都属于改类的每个实例. 实例的每个槽都可以保存值的位置, 该位置可以通过函数*SLOT-VALUE*来访问

*SLOT-VALUE*接受一个对象和一个槽的名字作为参数并返回该对象中改命名的槽的值.它支持 *SETF*位置

一个类从基类中继承槽描述符

## 对象初始化
Common Lisp提供了三种方式来控制槽的初始值:
* 通过:initarg 选项, 你可以指定随后作为 *MAKE-INSTANCE*的关键字形参名字并使用该参数的值保存在槽中
* :initform可以让你指定一个Lisp表达式在没有:initarg参数传递给*MAKE-INSTANCE*时为改槽计算一个值
* 在广义函数*INITIALIZA-INSTANCE*上定义一个方法, 它将被*MAKE-INSTANCE*调用

下面是一个初始化例子
```lisp
(defclass bank-account ()
  ((customer-name
    :initarg :customer-name)
  (balance
    :initarg :balance
    :initform 0)))
```

如果实例时没有给 :customer-name关键字形参, 那么 customer-name 槽将会是未绑定

可以像下面定义, 要求必须给定 :customer-name参数
```lisp
(defvar *account-numbers* 0)

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name."))
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))))
```

### INITIALIZE-INSTANCE
*STANDARD-OBJECT*上特化的 *INITIALIZA-INSTANCE*主方法负责槽的初始化工作

最常见的的是定义一个特化在类上的 _:after_方法

下面是一个例子:
```lisp
(defvar *account-numbers* 0)

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name."))
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))
   account-type))
;; 设置 account-type槽
(defmethod initialize-instance :after ((account bank-account) &key)
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account-type)
          (cond
            ((>= balance 100000) :gold)
            ((>= balance 50000) :silver)
            (t :bronze)))))
```

为了保持该方法的形参列表与广义函数一致, 形参列表中的*&key*是必不可少的

## 访问函数
通过*SLOT-VALUE*很容易定义一个函数来读取槽的值
```lisp
(defun balance (account)
  (slot-value account 'balance))
```
更好的方法是使用广义函数, 这样为子类提供了不同的方法或使用附加方法来扩展其定义
```lisp
(defgeneric balance (account))

(defmethod balance ((account bank-account))
  (slot-value account 'balance))
```

更简洁的方式是定以*SETF*函数, *SETF*函数是一种扩展*SETF*的方式, 其定义了一个新的位置类型使其知道如何设置它.*SETF*函数的名字是一个亮元素列表, 其中第一个元素是符号setf而跌入个元素是一个符号, 通常是一个用来访问改*SETF*函数将要设置的位置的函数名.*SETF*函数可以接受任何数量的参数, 但第一个参数总是赋值的位置上的值.
```lisp
(defun (setf customer-name) (name account)
  (setf (slot-value account 'customer-name) name))
```
可以使用下面调用
```
(setf (customer-name my-account) "Sally Sue")
```
上面表达式将被编译成一个对你刚刚定义的*SETF*函数的调用, 其中 "Sally Sue"作为第一个参数而my-account 的值作为第二个参数

可以定义广义的*SETF*函数:
```lisp
(defgeneric (setf customer-name) (value account))

(defmethod (setf customer-name) (value (account bank-account))
  (setf (slot-value account 'customer-name) value))
```

也可以为 customer-name定义一个读取函数
```lisp
(defgeneric customer-name (account))

(defmethod customer-name ((account bank-account))
  (slot-value account 'customer-name))
```
根据上面代码可以写出如下表达式
```lisp
(setf (customer-name *account*) "Sally Sue")

(customer-name *account*)
```

收工编写与Lisp风格不太温和, *DEFCLASS*提供了三个槽选项, 自动的为特定槽创建读取和写入函数.

* _:reader_ 选项指定广义函数的名字, 该函数只接受一个对象参数.当*DEFCLASS*被求值时, 如果广义函数不存在则创建它, 然后添加一个方法, 该方法基于新类特化一个参数并返回该槽的值.该名字可以是任意的, 但通常将其命名成与槽本身相同的名字
```lisp
(balance
 :initarg :balance
 :initform 0
 :reader balance)
```

* _:writer_ 选项用来创建设置一个槽的值的广义函数和方法.该函数和方法按照*SETF*函数的要求创建, 接受新值作为其第一个参数并把它作为结果返回.
```lisp
(customer-name
 :initarg :customer-name
 :initform (error "Must supply a customer name.")
 :reader customer-name
 :writer (setf customer-name))
```
* _:accessor_ 选项用来同时创建读取函数和对应的*SETF*函数.
```lisp
(customer-name
 :initarg :customer-name
 :initform (error "Must supply a customer name.")
 :accessor customer-name)
```

根据上面重新定义 _bank-account_类
```lisp
(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name.")
    :accessor customer-name
    :documentation "Customer's name")
   (balance
    :initarg :balance
    :initform 0
    :reader balance
    :documentation "Current account balance")
   (account-number
    :initform (incf *account-numbers*)
    :reader account-number
    :documentation "Account number, unique within a bank.")
   (account-type
    :reader account-type
    :documentation "Type of account, one of :gold, :slver, or :bronze.")))
```

## WITH-SLOTS 和 WITH-ACCESSORS
两个标准宏 *WITH-SLOTS* 和 *WITH-ACCESSORS*可以减轻调用槽的混乱.

*WITH-SLOTS*提供了对槽的直接访问, 就像*SLOT-VALUE*那样, 形式如下:
```lisp
(with-slots (slot*) instance-form
  body-form*)
```
每一个 _slot_元素可以是一个槽的名字, 它也用作一个变量名;以个两元素列表, 第一个元素是一个用作变量的名字, 第二个元素则对应槽的名字. _instance-form_被求值依次来产生要访问其槽的对象
```lisp
(defmethod access-low-balance-penalty ((account bank-account))
  (with-slots (balance) account
    (when (< balance *minium-balance*)
      (decf balance (* balance .01)))))
```
或者使用两元素列表:
```lisp
(defmethod access-low-balance-penalty ((account bank-account))
  (with-slots ((bal balance)) account
    (when (< balance *minium-balance*)
      (decf balance (* balance .01)))))
```
*WITH-ACCESSORS* 提供了一个访问方法的简单方法

如果你已经用了一个 _:accessor_而不只是 _:reader_定义了balance, 那么可以使用*WITH-ACCESSORS*, 它和 *WITH-SLOTS*形式相同, 除了槽列表的每一项都必须包含一个变量名和一个访问函数名字的两元素列表, 在*WITH-ACCESSORS*的主体中, 对一个变量的引用等价的对应相应的访问函数调用.
```lisp
(defmethod access-low-balance-penalty ((account bank-account))
  (with-accessors ((balance balance)) account
    (when (< balance *minimum-balance*)
      (decf balance (* balance .01)))))
```
上面代码中第一个balance是变量的名字, 第二个是访问函数的名字, 它们不必相同.例如和编写方法来合并两个帐号
```lisp
(defmethod merge-accounts ((account1 bank-account) (account2 bank-account))
  (with-accessors ((balance1 balance)) account 1
    (with-accessors ((balance2 balance)) account 2
      (incf balance1 balance 2)
      (setf balance2 0))))
```

## 分配在类上的槽
最后一个槽选项是 _:allocation_. 它的值可以是 _:instance_或 _:class_,默认是 _:instance_. 当一个槽带有 _:class_分配选项时, 该槽只有单一值存储在类中, 并且被所有实例所共享,并同样使用 *SLOT-VALUE*对 _:class_槽进行访问,

_:class_槽也同样通过该类的一个实例来访问该槽的值, 尽管它实际并没有保存在实例中. _:initform_ 和 _:initarg_选项质上具有相同效果, 只是 _:initform_将在类定义时而不是每次创建实例时求值, 另一方面, 传递 _:initarg_给 *MAKE-INSTANCE*会设置该值, 从而影响该类所有实例
