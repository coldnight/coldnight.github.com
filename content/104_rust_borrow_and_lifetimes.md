Title: 【译】Rust 借用和生命周期
Date: 2020-03-25
Category: Rust
Tags: Rust, 借用, 生命周期
Slug: rust-borrow-and-lifetimes


> 译者注：这是我学习 Rust 生命周期对我最有帮助的文章之一，故翻译了一下。


原文链接：[Rust Borrow and Lifetimes](http://arthurtw.github.io/2014/11/30/rust-borrow-lifetimes.html)。

Rust 是一门处于往 1.0 活跃开发的新语言（译注：1.0 早已发布，目前最新稳定版本 [1.42](https://github.com/rust-lang/rust/releases/tag/1.42.0)）。
我必须再写一篇关于我为什么觉得 Rust 很棒的新博客，但是今天我将关注在它的借用（borrow）
和生命周期（lifetimes）系统，这也是常常让包括我在内的 Rust 新手陷入困境的地方。这篇文章假设
你基本了解 Rust，如果还没推荐你先阅读[指南](http://doc.rust-lang.org/guide.html)和[指针指南](http://doc.rust-lang.org/guide-pointers.html)。


## 资源所有权和借用 {#资源所有权和借用}

Rust 通过一个难缠的（sophisticated）借用系统在不用 GC 的情况下达到内存安全。对于任何资源
（栈内存、堆内存、文件句柄等），他们都对应一个唯一的所有者（owner）在需要的情况下处理资源回收。
你可以通过 `&` 或者 `&mut` 创建一个新的绑定指向该资源，这被称之为借用或可变借用。编译器确保
所有的所有者（owners）和借用者（borrowers）行为正确。


## 拷贝和转移（Copy and move） {#拷贝和转移-copy-and-move}

在我们开始进入借用系统之前，我们需要知道 Rust 如何处理拷贝和转移。这个 [StackOverflow 答案](https://stackoverflow.com/questions/24253344/is-it-possible-to-make-a-type-only-movable-and-not-copyable/24253573#24253573)非常值得一读。
基本上，在赋值和函数调用上：

1.  如果值是可拷贝的（copyable）（仅涉及原始（primitive）类型，不涉及如内存或文件句柄的资源），编译器默认进行拷贝。
2.  其他情况，编译器转移（moves）所有权（ownership）并使原来的绑定无效。

简而言之，POD（Plan Old Data） => 拷贝，Non-POD（线性类型（linear types））=> 转移。

以下是一些额外的注释供你参考：

-   Rust 拷贝像 C。每一个按值（by-value）使用一个值都是字节拷贝（通过 `memcpy` 浅拷贝），而不是语义上的拷贝或克隆。
-   如果想要让一个 POD 结构体变成不可拷贝的，你可以使用一个 [NoCopy](http://doc.rust-lang.org/std/kinds/marker/struct.NoCopy.html) 标记，或者实现 [Drop](http://doc.rust-lang.org/std/ops/trait.Drop.html) 特性（trait）。

转移之后，所有权就转移到了下一个所有者那。


## 资源回收 {#资源回收}

Rust 会在任何资源的所有权消失后立刻释放该资源，就这些，当：

1.  所有者超出作用域，或
2.  正在持有的所有者改变绑定（原始绑定变成 void）。


## 所有者和借用者的权限（privileges）和限制 {#所有者和借用者的权限-privileges-和限制}

这一节基于 [Rust Guide](http://doc.rust-lang.org/guide.html) 在权限（privileges）一部分提到拷贝和转移。

所有者有一些权限。它可以：

1.  控制资源回收。
2.  借出资源，不可变的（可多次借用）或可变的（只能独占），和
3.  交出所有权（通过转移）

同时所有者也存在一些限制：

1.  不可变借用期间，所有者不能

    a. 改变资源，或者

    b. 以可变的方式借出资源。

2.  可变借用期间所有者不能

    a. 访问该资源，或者

    b. 再次借出该资源。

借用者同时也有一些权限。除了访问或者更改借用的资源外，借用者也可以进一步借出（share the borrow）：

1.  不可变借用者可以借出（拷贝）不可变借用（译注：再次以不可变借用借出）
2.  可变借用者可以交出（转移）可变借用。（可变引用默认使用转移。）


## 代码示例 {#代码示例}

关于借用我们已经聊的够多了，让我们一起来看一些代码吧（你可以通过 <https://play.rust-lang.org> 运行这些 Rust 代码。）
在下面所有的例子中，我们将使用不可拷贝的 `struct Foo` ，因为它包含了一个装箱（boxed）（堆分配）值。
使用不可拷贝资源可以限制相关操作，让我们更好的学习。

对于每一个代码示例，我们还提供了一个“作用域图表”（scope chart）来展示所有者和借用者的作用域。
图表第一行的大括号和代码中的大括号一一对应。


### 所有者在可变借用期间不能访问资源 {#所有者在可变借用期间不能访问资源}

如果我们将代码中的 `println!` 解除注释，代码将不能编译：

```rust
struct Foo {
    f: Box<int>,
}

fn main() {
    let mut a = Foo { f: box 0 };
    // mutable borrow
    let x = &mut a;
    // error: cannot borrow `a.f` as immutable because `a` is also borrowed as mutable
    // println!("{}", a.f);
}
```

```nil
           { a x * }
   owner a   |_____|
borrower x     |___| x = &mut a
access a.f       |   error
```

这违反了所有者限制 #2(a)。如果我们将 `let x = &mut a;` 在一个嵌套的代码块里：借用
在 `println!` 之前结束，这段代码将能正常工作：

```rust
fn main() {
    let mut a = Foo { f: box 0 };
    {
        // mutable borrow
        let x = &mut a;
        // mutable borrow ends here
    }
    println!("{}", a.f);
}
```

```nil
           { a { x } * }
   owner a   |_________|
borrower x       |_|     x = &mut a
access a.f           |   OK
```


### 借用者可以转移可变借用到一个新的借用者 {#借用者可以转移可变借用到一个新的借用者}

这段代码展示借用者的权限 #2: 可变借用 `x` 可以将所有权转移可变借用到一个新的借用者 `y` 。

```rust
fn main() {
    let mut a = Foo { f: box 0 };
    // mutable borrow
    let x = &mut a;
    // move the mutable borrow to new borrower y
    let y = x;
    // error: use of moved value: `x.f`
    // println!("{}", x.f);
}
```

```nil
           { a x y * }
   owner a   |_______|
borrower x     |_|     x = &mut a
borrower y       |___| y = x
access x.f         |   error
```

转移之后，原始的借用者 `x` 不再能访问借用的资源。


## 借用作用域（Borrow scope） {#借用作用域-borrow-scope}

如果我们开始传递引用（ `&` 和 =&mut=）事情就开始变得有趣，同时也是 Rust 新手们开始困惑的地方。


### 生命周期（Lifetime） {#生命周期-lifetime}

在整个借用过程中，知道借用者的借用什么时候开始和结束非常重要。在[生命周期指南](http://doc.rust-lang.org/guide-lifetimes.html)中是这样定义生命周期的：

> A lifetime is a static approximation of the span of execution during which the pointer is valid: it always corresponds to some expression or block within the program.

<!--quoteend-->

> 生命周期是指针有效范围的静态近似值：它始终对应程序中的某些表达式或代码块。

然而，我更喜欢使用 **借用作用域（borrow scope）** 这个术语去描述借用生效的作用域。请注意它不同于上面生命周期的定义。
（我第一次见到这个术语是在一个 Rust [RFC 讨论](https://github.com/rust-lang/rfcs/pull/431) 中，尽管我的定义可能会有所不同。）我会在稍后给出我为什么避免使用生命周期的原因。
现在我们先把生命周期放在一边。


### & = borrow {#and-borrow}

一些关于借用的事情：

首先，只需要记住 `&` = 借用， `&mut` = 可变借用。任何地方你看到一个 `&` ，那就是一个借用。

其次，当一个 `&` 出现在任何结构体中（在它的字段中）或者函数/闭包（返回值或者捕获的引用），结构体/函数/闭包就是一个借用者，
并且应用所有的借用规则。

再次，对于每一个借用，都存在一个所有者和一个或多个借用者。


### 扩展借用作用域 {#扩展借用作用域}

一些关于借用作用域的事情：

首先，一个借用作用域：

-   是一个借用生效的范围，并且
-   不一定是借用者的词法作用域，因为借用者可以扩展借用作用域（参见下面）。

其次，借用者在赋值或者函数调用中可以通过拷贝（不可变借用）或者转移（可变借用）扩展借用作用域。
接收者（receiver）（可以是新的绑定、结构体、函数或者闭包）变成新的借用者。

再次，借用作用域是所有借用者作用域的并集，并且被借用的资源必须在整个借用作用域里有效。


### 借用公式 {#借用公式}

根据最后一点，我们得到一个借用公式：

> 资源作用域 >= 借用作用域 = 所有借用者作用域的并集。


### 代码示例 {#代码示例}

让我们看一些扩展作用域的代码示例。结构体 `struct Foo` 和前面的一样：

```rust
fn main() {
    let mut a = Foo { f: box 0 };
    let y: &Foo;
    if false {
        // borrow
        let x = &a;
        // share the borrow with new borrower y, hence extend the borrow scope
        y = x;
    }
    // error: cannot assign to `a.f` because it is borrowed
    // a.f = box 1;
}
```

```nil
             { a { x y } * }
  resource a   |___________|
  borrower x       |___|     x = &a
  borrower y         |_____| y = x
borrow scope       |=======|
  mutate a.f             |   error
```

即使借用发生在 `if` 代码块之内并且借用者 `x` 在 `if` 代码块之后超出作用域，它已经通过赋值 `y=x;` 扩展了借用作用域，
所以存在两个借用者： `x` 和 `y` 。根据借用公式：借用作用域是借用者 `x` 和借用者 `y` 作用域的并集：
范围开始第一次借用于 `let x = &a;` 直到 `main` 代码块的结尾。（注意绑定 `y` 在 `y=x;` 之前不是借用者。）

你可能注意到了由于条件永远是 false `if` 代码块永远不会执行，但是编译器始终拒绝资源所有者 `a` 去访问
它的资源。这是因为所有的借用检查发生在编译期，这样程序运行时就不需要做任何事情。


## 借用多个资源 {#借用多个资源}

目前为止我们只关注借用单个资源。借用者可以借用多个资源吗？当然！比如一个函数可以接受两个引用然后
基于一些情况返回其中一个，e.g. 其中字段值比较大的那一个。

```rust
fn max(x: &Foo, y: &Foo) -> &Foo
```

`max` 函数返回一个 `&` 指针，因此它是一个借用者。返回的结果可以是输入参数的任意一个，所以它借用了
两鞥额资源。


### 命名借用作用域（Named borrow scope） {#命名借用作用域-named-borrow-scope}

当存在多个 `&` 指针作为输入，我们需要使用 **命名生命周期（named lifetimes）** 指定它们之间的关系，
参见 [Lifetimes Guide](http://doc.rust-lang.org/guide-lifetimes.html#named-lifetimes)。但现在，让我们叫它们 **命名借用作用域（named borrow scopes）** 。

上面的代码没有使用 **命名生命周期** 指定它们之间的关系是不会通过编译器的，i.e. 哪些借用者 **分组（grouped）**
到哪个借用作用域。下面的实现是合法的：

```rust
fn max<'a>(x: &'a Foo, y: &'a Foo) -> &'a Foo {
    if x.f > y.f { x } else { y }
}
```

```nil
(All resources and borrowers are grouped in borrow scope 'a.)
                  max( {   } )
    resource *x <-------------->
    resource *y <-------------->
borrow scope 'a <==============>
     borrower x        |___|
     borrower y        |___|
   return value          |___|   pass to the caller
```

在这个函数中，我们有一个借用作用域 `'a` 和三个借用者：两个输入参数和函数返回结果。
前面提到的借用公式依然生效，但是现在每个被借用的资源必须满足公式。参见下面的例子：


### 代码示例 {#代码示例}

在接下来的代码中，我们来使用上面的 `max` 函数在 `a` 和 `b` 之间选择一个更大 `Foo` ：

```rust
fn main() {
    let a = Foo { f: box 1 };
    let y: &Foo;
    if false {
        let b = Foo { f: box 0 };
        let x = max(&a, &b);
        // error: `b` does not live long enough
        // y = x;
    }
}
```

```nil
              { a { b x (  ) y } }
   resource a   |________________| pass
   resource b       |__________|   fail
 borrow scope         |==========|
temp borrower            |_|       &a
temp borrower            |_|       &b
   borrower x         |________|   x = max(&a, &b)
   borrower y                |___| y = x
```

直到 `let x = max(&a, &b)` 都一些正常，因为 `&a` 和 `&b` 都是尽在表达式中有效的临时引用，
并且第三个借用 `x` 借用了两个资源（不管最终是 `a` 或 `b` ，对于借用检查器而言它都借用了）直到 `if`
块结束，所以借用作用域是从 `let x = max(&a, &b);` 到 `if` 块结尾。两个资源 `a` 和 `b` 在整个借用作用域
都有效,因此满足借用公式。

现在如果我们解除最后一个赋值 `y = x;` 的注释， `y` 变成第四个借用者，然后借用作用域被扩展到 `main`
块的结尾，导致资源 `b` 不能满足公式。


## 结构体作为借用者 {#结构体作为借用者}

除了函数和闭包之外，一个结构体也可以通过其字段存储多个引用来借用多个资源。我们通过下面的一些例子
来看看借用公式如何生效的。我们来使用 `Link` 结构体来保存一个引用（不可变借用）：

```rust
struct Link<'a> {
  link: &'a Foo,
}
```


### 结构体借用多个资源 {#结构体借用多个资源}

即使只有一个字段，结构体 `Link` 也可以借用多个资源：

```rust
fn main() {
    let a = Foo { f: box 0 };
    let mut x = Link { link: &a };
    if false {
        let b = Foo { f: box 1 };
        // error: `b` does not live long enough
        // x.link = &b;
    }
}
```

```nil
             { a x { b * } }
  resource a   |___________| pass
  resource b         |___|   fail
borrow scope     |=========|
  borrower x     |_________| x.link = &a
  borrower x           |___| x.link = &b
```

在上面例子中，借用者 `x` 从所有者 `a` 借用资源，借用作用域到 `main` 块的结尾。So far so good。
如果我们解除最后一个赋值 `x.link = &b;` 的注释， `x` 也尝试从所有者 `b` 借用资源，这会让资源 `b`
不能满足借用公式。


### 没有返回值的函数扩展借用作用域 {#没有返回值的函数扩展借用作用域}

一个没有返回值的函数同样也可以通过输出参数能扩展借用作用域。例如，这个函数 `store_foo` 接受一个
`Link` 的可变引用，然后存储一个引用（不可变借用）到 `Foo` 里：

```rust
fn store_foo<'a'>(x: &mut Link<'a>, y: &'a Foo) {
  x.link = y;
}
```

在接下来的代码中，被 `a` 所有的资源是被借用资源； `Link` 结构体被借用者 `x` 可变的引用着（i.e. `*x` 是借用者）；
借用作用域直到 `main` 块的结尾。

```rust
fn main() {
    let a = Foo { f: box 0 };
    let x = &mut Link { link: &a };
    if false {
        let b = Foo { f: box 1 };
        // store_foo(x, &b);
    }
}
```

```nil
             { a x { b * } }
  resource a   |___________| pass
  resource b         |___|   fail
borrow scope     |=========|
 borrower *x     |_________| x.link = &a
 borrower *x           |___| x.link = &b
```

如果我们解除最后一个函数调用 `store_foo(x, &b);` ,这个函数将会尝试将 `&b` 存储到 `x.link` ，
将资源 `b` 作为另外一个被借用的资源，由于 `b` 的作用域没有覆盖整个借用作用域，导致不满足借用公式。


### 多个借用作用域 {#多个借用作用域}

一个函数中可以存在多个借用作用域。例如：

```rust
fn superstore_foo<'a, 'b>(x: &mut Link<'a>, y: &'a Foo,
                          x2: &mut Link<'b>, y2: &'b Foo) {
    x.link = y;
    x2.link = y2;
}
```

这个的函数（可能不是特别有用）中，涉及两个不同的借用作用域。每个借用作用域都有它们自己的作用域公式要满足。


## 为什么生命周期会造成困惑 {#为什么生命周期会造成困惑}

最后，我想解释一下为什么我认为 Rust 借用系统使用 **生命周期** 术语会造成困惑（同时避免在这片博文中使用它）。

当我们讨论借用时会涉及到不同类型的“生命周期”：

A. 资源所有者的生命周期（或者 被所有/被借用 资源
B. 被借用的生命周期，i.e. 从开始借用到最后返还
C. 每一个独立的借用者或被借用的指针的生命周期

当有人说“生命周期”，它可以指上面的任何一个。如果涉及多个资源和借用者就会变的更加困惑。
比如，在函数或者结构体生命中一个“命名的生命周期”指哪个？是 A、B 或者 C？

在我们的前一个 `max` 函数中：

```rust
fn max<'a>(x: &'a Foo, y: &'a Foo) -> &'a Foo {
    if x.f > y.f { x } else { y }
}
```

生命周期 `'a` 的意义是什么？它不应该是 A，因为涉及两个资源并且他们有不同的生命周期。也不可能是 C，
因为有三个借用者： `x` 、 `y` 和函数的返回值，并且他们也都有不同的生命周期。它是 B 吗？可能。
但是整个借用作用域并不是一个具体的对象，它怎么能有一个“生命周期”呢？称它为生命周期就会造成困惑。

另一种说法是它意味着对被借用资源的最小生命周期要求。一定程度上是有道理的，
但是我们怎么称呼最小生命周期要求“生命周期”？

所有权/借用概念自身已经够复杂了。我会说：对术语“生命周期”的困惑对学习这个概念造成了更多的莫名其妙。

P.S. 使用上面定义的 A、B 和 C，借用公式变成：

> A >= B = C\_1,UC\_2U...UC\_n


## 学习 Rust 是值得的！ {#学习-rust-是值得的}

尽管借用和所有权可能让你花一些时间来掌握（to grok），但是是一个非常有趣的学习。Rust 尝试不用 GC
来实现内存安全，并且目前来看做的非常好。一些人说他们通过学习 Haskell 改变了他们编程的方式。
我认为Rust 同样也值得你学习。

希望这篇博文能提供一些帮助。
