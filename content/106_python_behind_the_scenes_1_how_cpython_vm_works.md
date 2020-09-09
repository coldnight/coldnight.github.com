Title: 【译】Python 幕后 #1: CPython 虚拟机如何工作
Date: 2020-09-08
Category: Python
Tags: Python, CPython, VM, 机制
Slug: python-behind-the-scenes_1_how_cpython_vm_works

原文链接：[Python behind the scenes #1: how the CPython VM works](https://tenthousandmeters.com/blog/python-behind-the-scenes-1-how-the-cpython-vm-works/)。

### 介绍（Introduction） {#介绍-introduction}

你是否曾经好奇过当你运行 Python 代码时 `python` 做了些什么？

```shell
$ python script.py
```

这篇文章将开启一个系列来尝试解答这个问题。我们将深入 Python 最流行的实现 CPython 的内部。
通过深入 CPython 的内部我们将更深一层的去理解这门编程语言本身。这也是我们这个系列的最主要的目标。
如果你熟悉 Python 并且可以阅读 C 代码，但是对 CPython 源码本身没有太多的经验，
那么你可能非常适合本系列，并且对本系列感兴趣。


### 什么是 CPython 并且为什么有人想学习它（What CPython is and why anyone would want to study it） {#什么是-cpython-并且为什么有人想学习它-what-cpython-is-and-why-anyone-would-want-to-study-it}

我们首先来说明一些众所周知的事情。CPython 是用 C 编写的 Python 解析器。他是 Python 语言的众多实现
的一种，其他还有诸如 PyPy、Jython、IronPython 等。CPython 的独特之处在于它是 Python 的起源、维护时间最长也是最受欢迎的。

CPython 实现了 Python，但是 Python 是什么？最简单的一个答案可能是：Python 是一门编程语言。
当正确问相同的问题，那么答案将会更加明确：什么定义了 Python？Python 不像 C 语言有正式的规范，
但是与之相近的是 [Python 语言参考（Python Language Reference）](https://docs.python.org/zh-cn/3.9/reference/index.html)，它以如下内容开始：

> 我希望尽可能地保证内容精确无误，但还是选择使用自然词句进行描述，正式的规格定义仅用于句法和词法解析。这样应该能使文档对于普通人来说更易理解，但也可能导致一些歧义。因此，如果你是来自火星并且想凭借这份文档把 Python 重新实现一遍，也许有时需要自行猜测，实际上最终大概会得到一个十分不同的语言。而在另一方面，如果你正在使用 Python 并且想了解有关该语言特定领域的精确规则，你应该能够在这里找到它们。如果你希望查看对该语言更正式的定义，也许你可以花些时间自己写上一份 --- 或者发明一台克隆机器 :-)

所以 Python 不仅仅通过语言参考定义，说 Python 是通过语言参考定义的实现或者说是 CPython 都是错误的，
因为其中的一些实现细节并不是语言的一部分。一个基于引用计数的垃圾回收器就是例子。由于没有一个来说法是正确的，
我们可以说 Python 的一部分是由 Python 语言参考（Python Language Reference）定义，
一部分是它的主要实现 CPython 定义。

这样的结论似乎很古怪，但是我认为这对我们弄清我们要学习的主题至关重要。我们可能依然困惑我们为什么需要学习它。
除了好奇心，我认为还有如下理由：

-   拥有完整的视角可以更深入的理解这门语言。如果了解一些 Python 的细节那么就更容易掌握一些 Python 特性。
-   在实践中实现细节很重要。当想要了解语言适用性及其局限性、评估性能或检测效率低下时，了解对象如何存储，
    垃圾回收器如何工作，以及如何协调多个线程将是非常重要的。
-   CPython 提供了 Python/C API 来允许我们用 C 扩展 Python 或者在 C 中嵌入 Python。
    程序员需要很好的理解 CPython 如何工作才能高效的使用这些 API。


### 了解 CPython 如果工作需要做些什么（What it takes to understand how CPython works） {#了解-cpython-如果工作需要做些什么-what-it-takes-to-understand-how-cpython-works}

CPython 被设计成易于维护。一个新人完全可以阅读源代码并理解代码做了些什么。但是，这可能需要一些时间。
通过这个系列我希望能帮助你缩短这个时间。


### 这个系列如何推进（How this series is laid out） {#这个系列如何推进-how-this-series-is-laid-out}

我选择采取自上而下的方法。在这个部分我们将探索 CPython 虚拟机的核心概念。接下来，我们将看到
CPython 如何编译一个程序到 VM 可以执行的内容。再然后，我们将熟悉源代码，并通过执行一个程序
来学习解释器的主要部分。最后，我们可以挑选语言不同的方面来一个接一个的去看看它们是如何实现的。
这是我的一个大概的想法，并不是一个严格的计划。

****Note****: 本文参考 CPython 3.9。一些实现细节将必然会随着 CPython 的演进而改变。
我将会尝试关注一些重要的改变并添加更新备注。


### 鸟瞰（The big picture） {#鸟瞰-the-big-picture}

执行一个 Python 程序大概经过三个阶段：

1.  初始化（Initialization）
2.  编译（Compilation）
3.  解释（Interpretation）

在初始化阶段，CPython 初始化运行 Python 所需要的数据结构。同时也准备一些诸如
内建类型、配置和加载内建模块，初始化导入系统（import system）和一些其他的事情。
这是一个非常重要的阶段，但是由于其功能性质这个阶段也是常被 CPython 的探索者忽略的一个阶段。

接下来是编译阶段。CPython 在某种意义上是一个解释器而不是编译器，因为它不输出机器码。
但是解释器通常会在执行之前把源代码翻译成一种中间语言（intermediate representation）。
CPython 也是如此。这个翻译阶段和一个典型的编译器做同样的事情：解析源代码然后构建 AST（Abstract Syntax Tree）、
通过 AST 生成字节码、甚至执行一些字节码优化的操作。

在进入下一阶段之前，我们需要理解什么是字节码（bytecode）。字节码是一系列的指令。
每一个指令由两个字节组成：一个为 opcode，一个为参数（argument）。看如下例子：

```python
def g(x):
    return x + 3
```

CPython 将函数定义 `g` 翻译成一个字节序列：[124, 0, 100, 1, 23, 0, 83, 0]。
如果我们运行标准库 `dis`&nbsp;[^fn:1]去反汇编它，我们将会得到如下内容：

```python
2           0 LOAD_FAST            0 (x)
            2 LOAD_CONST           1 (3)
            4 BINARY_ADD
            6 RETURN_VALUE
```

字节 `124` 表示 opcode `LOAD_FAST` 并且有一个参数 `0` 。字节 `100` 表示 opcode `LOAD_CONST` 并且有一个参数 `1` 。
`BINARY_ADD` 和 `RETURN_VALUE` 指令不需要参数所以总是被编码成 `(23, 0)` 和 `(83, 0)` 。

CPython 的核心就是一个运行字节码的虚拟机。通过查看上面例子你可能已经猜到它是如何工作的拉。
CPython 虚拟机一个基于栈的。也就意味这它执行指令并通过栈存储和获取数据。
`LOAD_FAST` 指令将局部变量压入栈，
`LOAD_CONST` 压入一个常量，
`BINARY_ADD` 从栈中弹出两个对象，然后进行相加并将结果放回栈。
最好 `RETURN_VALUE` 从栈弹出任意值然后将结果返回给调用者。

当有指令需要运行时字节码运行在一个巨大的执行循环中， `yield` 一个值或者发生错误将导致它停止。

这样一个简短概述引发了很多问题：

-   参数对 opcode `LOAD_FAST` 和 `LOAD_CONST` 的意义是什么？他们是索引吗？他们如何索引？
-   VM 会在栈上放置值或者对象的引用吗？
-   CPython 如何知道 `x` 是一个局部变量。
-   如果参数太大无法放到一个字节内怎么办？
-   连接两个字符串和两个数字相加是同一个指令吗？如果是，VM 如何处理这些操作之间的差异？

要回答这些和其他一些有趣的问题我们需要先看一下 CPython VM 的一些核心概念。


### 代码对象、函数对象、帧（Code objects, function objects, frames） {#代码对象-函数对象-帧-code-objects-function-objects-frames}


#### 代码对象 {#代码对象}

我们已经看过一个简单的函数的字节码是什么样子的。但是一个典型的 Python 程序要复杂的多。
VM 如何执行一个包含程序定义和函数调用的模块（module）？

考虑如下程序：

```python
def f(x):
    return x + 1

print(f(1))
```

它的字节码什么样子？让我们分析这个程序做了什么来解答这个问题。它定义了一个函数 `f` ，
通过一个参数 `1` 调用函数 `f` 然后打印结果。无论函数 `f` 做了什么都不会抱憾在模块字节码中。
我们可以通过运行一个反汇编来证明我们自己：

```nil
1           0 LOAD_CONST               0 (<code object f at 0x10bffd1e0, file "example.py", line 1>)
            2 LOAD_CONST               1 ('f')
            4 MAKE_FUNCTION            0
            6 STORE_NAME               0 (f)

4           8 LOAD_NAME                1 (print)
           10 LOAD_NAME                0 (f)
           12 LOAD_CONST               2 (1)
           14 CALL_FUNCTION            1
           16 CALL_FUNCTION            1
           18 POP_TOP
           20 LOAD_CONST               3 (None)
           22 RETURN_VALUE
```

第一行通过从一些叫做代码对象（code object）的东西创建一个函数并绑定名字为 `f` 来定义函数 `f` 。
我们没有看到函数 `f` 用来返回一个自增的参数的字节码。

被作为单一执行单元的代码片段如一个模块或者一个函数体被称为代码块。
CPython 存储关于代码块的信息的结构体就是代码对象（code object）。
它包含字节码和一些其他的比如代码块使用的变量名列表。运行一个模块或者调用一个函数意味着开始
执行相应的代码对象。


#### 函数对象（function object） {#函数对象-function-object}

但是，函数不仅仅是代码对象。它必须包含一些额外的信息比如名字、文档字符串（docstring）、
默认参数和定义在闭包作用域中的变量的值。这些信息连同代码对象存储在一个函数对象里。
`MAKE_FUNCTION` 指令用于创建函数对象。CPython 中定义函数对象的源码前置了如下注释：

> Function objects and code objects should not be confused with each other:
>
> Function objects are created by the execution of the 'def' statement. They reference a code object in their <span class="underline"><span class="underline">code</span></span> attribute, which is a purely syntactic object, i.e. nothing more than a compiled version of some source code lines. There is one code object per source code "fragment", but each code object can be referenced by zero or many function objects depending only on how many times the 'def' statement in the source was executed so far.

<!--quoteend-->

> 函数对象和代码对象不应互相混淆：
>
> 函数对象通过执行 'def' 语句创建。它们通过他们的 <span class="underline"><span class="underline">code</span></span> 属性引用一个代码对象，这个代码对象
> 是一些源代码编译后纯语法对象。每一个代码“片段（fragment）”都对应一个代码对象，
> 但是每一个代码对象都可以被零个或多个函数对象引用，取决于源码中的 'def' 语句目前为止被执行了多少次。

多个函数对象如何饮用一个代码对象？这里有个例子：

```python
def make_add_x(x):
    def add_x(y):
        return x + y
    return add_x

add_4 = make_add_x(4)
add_5 = make_add_x(5)
```

`make_add_x` 函数的字节码包含了一个 `MAKE_FUNCTION` 指令。函数 `add_4` 和 `add_5` 是通过同一个
代码对象作为参数调用这个指令产生的结果，但是其参数 `x` 的值不相同。
每一个函数拥有自己的变量单元的机制允许我们创建如 `add_4` 和 `add_5` 的闭包函数。

我们继续下一个主题之前推荐你看一下定义函数对象的代码：

```c
struct PyCodeObject {
    PyObject_HEAD
    int co_argcount;            /* #arguments, except *args */
    int co_posonlyargcount;     /* #positional only arguments */
    int co_kwonlyargcount;      /* #keyword only arguments */
    int co_nlocals;             /* #local variables */
    int co_stacksize;           /* #entries needed for evaluation stack */
    int co_flags;               /* CO_..., see below */
    int co_firstlineno;         /* first source line number */
    PyObject *co_code;          /* instruction opcodes */
    PyObject *co_consts;        /* list (constants used) */
    PyObject *co_names;         /* list of strings (names used) */
    PyObject *co_varnames;      /* tuple of strings (local variable names) */
    PyObject *co_freevars;      /* tuple of strings (free variable names) */
    PyObject *co_cellvars;      /* tuple of strings (cell variable names) */

    Py_ssize_t *co_cell2arg;    /* Maps cell vars which are arguments. */
    PyObject *co_filename;      /* unicode (where it was loaded from) */
    PyObject *co_name;          /* unicode (name, for reference) */
        /* ... more members ... */
};
```

```c
typedef struct {
    PyObject_HEAD
    PyObject *func_code;        /* A code object, the __code__ attribute */
    PyObject *func_globals;     /* A dictionary (other mappings won't do) */
    PyObject *func_defaults;    /* NULL or a tuple */
    PyObject *func_kwdefaults;  /* NULL or a dict */
    PyObject *func_closure;     /* NULL or a tuple of cell objects */
    PyObject *func_doc;         /* The __doc__ attribute, can be anything */
    PyObject *func_name;        /* The __name__ attribute, a string object */
    PyObject *func_dict;        /* The __dict__ attribute, a dict or NULL */
    PyObject *func_weakreflist; /* List of weak references */
    PyObject *func_module;      /* The __module__ attribute, can be anything */
    PyObject *func_annotations; /* Annotations, a dict or NULL */
    PyObject *func_qualname;    /* The qualified name */
    vectorcallfunc vectorcall;
} PyFunctionObject;
```


#### 帧对象（frame object） {#帧对象-frame-object}

当执行一个代码对象时，VM 需要一直跟踪变量的值并不断的更新值栈（value stack）。
同时还需要记住在什么地方停止运行当前代码对象然后去运行其他的代码对象，并且在哪里返回。
CPython 在一个帧对象里存储这些信息，或者简单的说成帧。一个帧提供了一个哪个代码对象可以被执行的状态。
由于我们已经开始习惯源代码，所以这里我贴出帧对象的定义：

```c
struct _frame {
    PyObject_VAR_HEAD
    struct _frame *f_back;      /* previous frame, or NULL */
    PyCodeObject *f_code;       /* code segment */
    PyObject *f_builtins;       /* builtin symbol table (PyDictObject) */
    PyObject *f_globals;        /* global symbol table (PyDictObject) */
    PyObject *f_locals;         /* local symbol table (any mapping) */
    PyObject **f_valuestack;    /* points after the last local */

    PyObject **f_stacktop;          /* Next free slot in f_valuestack.  ... */
    PyObject *f_trace;          /* Trace function */
    char f_trace_lines;         /* Emit per-line trace events? */
    char f_trace_opcodes;       /* Emit per-opcode trace events? */

    /* Borrowed reference to a generator, or NULL */
    PyObject *f_gen;

    int f_lasti;                /* Last instruction if called */
    /* ... */
    int f_lineno;               /* Current line number */
    int f_iblock;               /* index in f_blockstack */
    char f_executing;           /* whether the frame is still executing */
    PyTryBlock f_blockstack[CO_MAXBLOCKS]; /* for try and loop blocks */
    PyObject *f_localsplus[1];  /* locals+stack, dynamically sized */
};
```

第一个帧被创建用来执行模块的代码对象。任何时候需要执行另外一个代码对象 CPython 都会创建创建新的
帧去执行该代码对象。每一个帧都有一个引用指向前一个帧。从而，帧形成了一个栈被称为调用栈，当前帧位于
顶部。当一个函数被调用，一个新的帧被压到栈上。当从当前执行帧返回时，CPython 通过记录的最后处理的指令
来继续执行前一个帧。某种意义上 CPython 除了执行帧其他什么也没做。但是接下来我们马上看到这个总结
善意的隐藏了某些细节。


### 线程、解释器、运行时（Threads, interpreters, runtime） {#线程-解释器-运行时-threads-interpreters-runtime}

我们已经讨论过三个重要的主题：

-   代码对象
-   函数对象，和
-   帧对象

CPython 还有三个重要的主题：

-   线程状态（thread stage）
-   解释器状态（interpreter state），和
-   运行时状态（runtime state）


#### 线程状态 {#线程状态}

线程状态是一个包含线程特定数据栈数据结构，其中包含调用栈、异常状态和调试设置。
不应将其和系统线程（OS thread）混淆，尽管它们联系紧密。考虑当时使用标准库 `threading`
在一个单独的县城运行一个函数发生了什么：

```python
from threading import Thread

def f():
    """Perform an I/O-bound task"""
    pass

t = Thread(target=f)
t.start()
t.join()
```

`t.start` 实际上创建通过系统调用（类 Unix 系统中通过 `pthread_create` ，Windows 通过
 `_beginthreadex` ）了一个新的系统线程。新建的线程调用在 `_thread` 模块中的函数负责调
用相应的目标函数。这个函数不仅仅接收目标函数和目标函数的参数，同时一个新的线程状态
被用在了新建的系统线程上。系统线程通过它自己的线程状态进入执行循环，并一直持有。

这里我们可能记得阻止多线程同时陷入执行循环的著名的 GIL（Global Interpreter Lock）。
主要原因是为了在不引入更多细粒度的锁的情况下保护 CPython 状态免受损坏。
[Python C/API 参考](https://docs.python.org/zh-cn/3.9/c-api/init.html#thread-state-and-the-global-interpreter-lock)清晰的解释了 GIL：

> The Python interpreter is not fully thread-safe. In order to support multi-threaded Python programs, there’s a global lock, called the global interpreter lock or GIL, that must be held by the current thread before it can safely access Python objects. Without the lock, even the simplest operations could cause problems in a multi-threaded program: for example, when two threads simultaneously increment the reference count of the same object, the reference count could end up being incremented only once instead of twice.

<!--quoteend-->

> Python 解释器不是完全的线程安全。为了支持 Python 多线程程序，引入一个称为全局解释器锁或者 GIL 的全局锁，
> 当前线程必须持有该锁才能安全的访问 Python 对象。如果没有持有该锁，就连最简单的操作都会在多线程程序中引发
> 问题：比如，当两个线程同时增加同一个对象的引用计数，引用计数最终可能只被增加了一次。

要管理多线程，就需要一个比线程状态更高层级的数据结构。


#### 解释器状态和运行时状态（interpreter and runtime states） {#解释器状态和运行时状态-interpreter-and-runtime-states}

实际上，这是两个状态：解释器状态和运行时状态。两者的需求区分看起来似乎不明显。但是，任何程序
的执行都需要各个状态的最少一个实例，并且有合理的原因。

解释器状态是一组线程以及该组相关的数据。线程共享诸如加载的模块（sys.modules）、内建对象（builtins.\_\_dict\_\_）
和导入系统（importlib）。

运行时状态是一个全局变量。保存着进程相关的数据。包含 CPython 状态（是否初始化）和 GIL。

通常情况下，一个进程的所有线程都属于同一个解释器。但是，有一些罕见的情况比如有人想创建一个子解释器来隔离一组线程。
比如 [mod\_wsgi](https://modwsgi.readthedocs.io/en/develop/user-guides/processes-and-threading.html#python-sub-interpreters) 使用不同的解释器来运行 WSGI 程序。最明显的隔离效果是各组线程拥有它们自己版本的模块，包括 `__main__` ，
也就是隔离全局命名空间（global namespace）。

CPython 没有提供像 `threading` 模块那样简单的方式创建新的解释器。这个特性仅仅通过 Python/C API 提供支持，
但是[未来有可能改善](https://www.python.org/dev/peps/pep-0554/)。


### 架构摘要（Architecture summary） {#架构摘要-architecture-summary}

让我们来快速总结 CPython 的架构来看看这一切是如何组织在一块的。解释器可以被看作分层结构（layer structure）。
以下总了了这些层级：

1.  运行时（Runtime）：进程的全局 CPython 状态；包含 GIL 和内存分配机制。
2.  解释器（Interpreter）：一组线程和它们共享的数据，如导入的模块。
3.  线程（Thread）：特定于单个系统县城的数据；包含调用栈。
4.  帧（Frame）：调用栈的元素；提供执行一个代码对象的状态。
5.  执行循环（Evalution loop）：执行一个代码对象（描述代码块做了什么，包含字节码、变量名字）

我们已经看到不同的层级通过相应的数据结构来表示。在某些情况下它们很难等效。比如，内存分配机制
通过使用全局变量来实现。这不是运行时状态的一部分，但是绝对是 CPython 运行时层级的一部分。


### 总结（Conclusion） {#总结-conclusion}

在这一部分我们已经大体描述了 `python` 在执行一个 Python 程序时做了什么。我们已经看到它工作在三个状态：

1.  初始化 Python 运行时
2.  编译源代码到一个模块代码对象；然后
3.  执行代码对象的字节码。

解释器中负责执行字节码的部分称为虚拟机（VM，virtual machine）。CPython VM 包含一些特别重要的概念：
代码对象（code object）、帧对象（frame object）、线程状态（thread state）、解释器状态（interpreter state）和
运行时（runtime）。这些数据结构构成了 CPython 架构的核心。

我们还有很多内容没有涉及到。我们避免陷入到源代码中。初始化和编译阶段完全超出了我们的范围。
相反，我们从虚拟机的概述开始。通过这种方式，我认为，我们可以更好的看到每个阶段所负责的内容。
现在我们知道 CPython 将源代码编译成了什么 -- 代码对象（code object）。
接下来我们将看到它是如何做到的。

如果你有任何问题、评论或者建议，随时通过 victor@tenthousandmeters.com 联系原作者。

****Update 4 September 2020****: I've made [a list of resources](https://tenthousandmeters.com/materials/python-behind-the-scenes-a-list-of-resources/) that I've used to learn about CPython internals

[^fn:1]: 译注： `python -c "import dis;dis.dis('''def g(x): return x + 3''')"`
