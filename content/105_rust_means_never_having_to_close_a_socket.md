Title: 【译】Rust 意味着无需手动关闭 Socket 连接
Date: 2020-06-20
Category: Rust
Tags: Rust, 借用, 生命周期
Slug: rust-means-never-having-to-close-a-socket

> 译者注：这是我学习 Rust 生命周期对我最有帮助的文章之一，故翻译了一下。


原文链接：[Rust Means Never Having to Close a Socket](https://blog.skylight.io/rust-means-never-having-to-close-a-socket/)

Rust 最酷的特性之一就是它可以自动地帮助你管理资源，同时在仍能保持安全（没有段错误）和高性能。

这是因为 Rust 是一门与众不同地编程语言，要理解我说的可能有点困难，让我来更近一步说明：

-   Rust 就像带垃圾回收的编程语言，你无需手动释放内存
-   Rust 不同于其他带垃圾回收的编程语言，你无需[^fn:1]手动关闭或者释放像文件、套接字和锁这样的资源
-   Rust 达到以上这些特性不附带任何运行时开销（垃圾回收或者引用计数），并且不牺牲安全性。

如果你曾经造成过一个套接字或者文件泄漏，或者使用过一些抽象方法造成了这些资源的泄漏，那么你就会知道这有多重要。

你可能已经期望通过“使用后释放”来避免内存问题，而与此同时你并没有考虑到没有明确地关闭套接字可能出现类似的错误。我在这里告诉你，还有更好地办法。

如果你使用的是带垃圾回收的编程语言，则应密切关注本文提到的资源管理方面的内容。如果你使用的是像 C/C++ 这样底层编程语言，你可能会对安全方面更加感兴趣。

> Rust 的许多特性都是从其他语言借鉴而来。Rust 之所以变得有趣是因为它把所有的这些特性放在了一起，并且在编程语言层面实现了更严格地保证。
> 实际上，这种编程语言层面的保证让这些特性更加实用。


## 所有权系统（The Ownership System） {#所有权系统-the-ownership-system}

让这种保证工作的方式是通过 Rust 的「所有权（ownership）」系统。不管任何时候你创建一个新的对象，都被创建它的「作用域（scope）」所拥有。

让我们通过一个例子来进一步说明：我们定义一个函数，函数拷贝输入文件到临时文件去处理它，然后拷贝输入文件到输出文件。

```rust
fn process(from: &Path, to: &Path) -> IoResult<()> {
    // creates a new tempdir with the specified suffix
    let tempdir = try!(TempDir::new("skylight"));

    // open the input file
    let mut from_file = try!(File::open(from));

    // create a temporary file inside the tempdir
    let mut tempfile =
        try!(File::create(&tempdir.path().join("tmp1")));

    // copy the input file into the tempfile
    try!(io::util::copy(&mut from_file, &mut tempfile));

    // use an external program to process the tmpfile in place

    // after processing, copy the tempfile into the output file
    let mut out = try!(File::create(to));

    io::util::copy(&mut tempfile, &mut out)
}
```

在这个例子中，函数 `process` 的作用域再第一行创建了 `TempDir` 是其初始拥有者。在这个例子中， `process` 函数从未放弃所有权，所以当函数完成调用，
它就会自动被丢弃（dropped），也就是会删除 `Tempfile` 。

这就是一个关于自动资源管理的例子。 `TempDir` 对象不仅仅是一片内存，它还代表被管理的资源。一旦程序不在使用该资源，那么它的清理逻辑将会被调用。

> 另外：在这 C++ 中被称为 「RAII」（Resource Acquistion Is Initialization）：资源获取即初始化，它是编程中最容易混淆但是有用的命名。

对我来说很有趣地是，能最大效率地减轻程序员手动管理内存的技术往往也最难成功和有效地减轻程序员手动地管理资源。
在高级语言中，我们从不需要释放内存，但是我们通常需要关闭套接字、文件和释放锁。

在实际中，在带有垃圾回收机制的编程语言中泄漏这些资源的情况令人震惊，所以我真的很享受这样一个事实，
在 Rust 中忘记关闭套接字不是一个大问题，就像在 Rust 忘记释放内存一样。并且在 Rust 中，
你可以免受防御涉及资源的“释放后使用”错误，就像你免受防御涉及内存的“释放后使用”错误一样。

这听起来很神奇，所以你可能会有一些问题关于它实际上是如何工作的。

首先，这个系统基于事实上一个对象在同一时间只能有一个所有者。我该如何确保我没有错误地在多个地方引用 `TempDir` ？
答案是所有权系统不是建议性的。在 Rust 中，对象被创建其的作用域所拥有。它可以将所有权转移到其他作用域，或者在完成执行后保留所有权。
当一个作用域完成时，Rust 将销毁它所拥有地所有对象。

因为一个对象同时只能归一个作用域所有，你可以通过查看就知道执行结束时有哪些对象将被销毁。

```rust
struct Person {
    first: String,
    last: String
}

fn hello() {
    let yehuda = Person {
        first: "Yehuda".to_string(),
        last: "Katz".to_string()
    };

    // `yehuda` is transferred to `name_size`, so it cannot be
    // used anymore in this function, and it will not be destroyed
    // when this function returns. It is up to `name_size`,
    // or possibly a future owner, to destroy it.
    let size = name_size(yehuda);

    let tom = Person {
        first: "Tom".to_string(),
        last: "Dale".to_string()
    };

    // `tom` wasn't transferred, so it will be
    // destroyed when this function returns.
}

fn name_size(person: Person) -> uint {
    let Person { first, last } = person;
    first.len() + last.len()

    // this function owns Person, so the Person is destroyed when `name_size` returns
}
```

仅仅通过逐一查看这两个函数，你可以看到 `yehuda` 被转移到了 `name_size` 函数，但是 `tom` 则没有。
通过查看 `name_size` 函数，你可以看到它一直拥有它的 `person` 参数直到它返回。仅仅通过查看这两个函数，
你就可以直接确定哪个对象（如果有）将会在它们执行完毕被销毁。

但是如何解释临时文件的例子？如果你查看 `process` 函数的第三行代码，你可以看到 `TempDir` 上的方法 `tempdir.path()` 被调用。
难道这不是意味着我创建了第二个引用，并且理论上有两个所有者？或者意味着我们将所有权转移到了 `path` 方法，也就是当该方法返回时会立即销毁这个目录？
显然这两个答案都行不通。


## 借用和借出（Borrowing and Lending） {#借用和借出-borrowing-and-lending}

要理解这里发生了什么，我们需要看一下 `path` 方法的方法签名。

```rust
fn path(&self) -> &Path
```

可以通过如下方式念出这个方法签名：

> path 方法「借用（borrows）」self 并返回「借用的（borrowed）」Path。

一个函数借用一个对象并不会取的对象的所有权，并且在返回时不会销毁该对象。它只能在函数调用期间使用借用的对象，它不能，比如，创建线程并在线程中使用借用的对象。
换句话说，借用的对象必须不能在超出借出它的函数的作用域外存活。

这意味着 Rust 编译器会检查所有函数调用并且在编译期得知代码是否尝试获取所有权。一旦一个对象的所有权被转移，那么原所有者会被拒绝访问该对象。

```rust
struct Person {
    first: String,
    last: String,
    age: uint
}

fn hello() {
    let person = Person {
        first: "Yehuda".to_string(),
        last: "Katz".to_string(),
        age: 32
    };

    let thirties = is_thirties(person);
    println!("{}, thirties: {}", person, thirties);
}

// This function tries to take ownership of `Person`; it does not
// ask to borrow it by taking &Person
fn is_thirties(person: Person) {
    person.age >= 30 && person.age < 40
}
```

如果我尝试编译这段代码，我会得到下面的编译错误（略有删节）：

```nil
move.rs:16:34: 16:40 error: use of moved value: `person`
move.rs:16     println!("{}, thirties: {}", person, thirties);
                                            ^~\~~~~

move.rs:15:32: 15:38 note: `person` moved here
move.rs:15     let thirties = is_thirties(person);
                                          ^~\~~~~
```

错误的意思是 `hello` 函数的作用域是 `Person` 的初始所有者，但是当调用 `is_thirties` 时，它把所有权转移到了 `is_thirties` 函数的作用域。
作为新的所有者，当 `is_thirties` 返回，它就会释放 `Person` 占据的内存。

作为替代你会想使用「借用和借出」写这个程序：

```rust
fn hello() {
    let person = Person {
        first: "Yehuda".to_string(),
        last: "Katz".to_string(),
        age: 32
    };

    // lend the person -- don't transfer ownership
    let thirties = is_thirties(&person);

    // now this scope still owns the person
    println!("{}, thirties: {}", person, thirties);
}

fn is_thirties(person: &Person) {
    person.age >= 30 && person.age < 40
}
```

****从根本上讲，这意味着经过验证地所有权是你函数接口的一部分。**** Rust 开发者有时将其称为“借用检查器（borrow checker）”，但是却恰当好处。

实际上，这些大部分时间可以正常工作的原因是，函数获得它们值的方式是通过“借用（borrowing）”。它们获得值、通过这些值处理一些逻辑然后返回。
长时间保持该值（比如通过使用线程）既不常见，又是时候该考虑一下正在发生的什么。

当我们开始编写一个新的函数时应该借用所需参数，而不是尝试获取其所有权。经过一段时间的 Rust 编程之后这将不会增加认知成本，只是默认这样做。
如果编译器抱怨（随着你掌握这些规则将它们变成习惯（second nature）这样的情况将越来越少），这意味着你正在做含有潜在危险地事情，那就需要你思考一下了。


## 从一个借用对象中返回一个借用字段（Returning a Borrowed Field from a Borrowed Object） {#从一个借用对象中返回一个借用字段-returning-a-borrowed-field-from-a-borrowed-object}

在前面我们检查了如下方法签名：

```rust
fn path(&self) -> &Path
```

这个签名可能让你困惑。我之前说过当一个函数借用了一个对象，它必须只能在函数调用期间使用这个值，并且在此之后就不能使用。
难道返回对象的其中一部分没有违反这个规则？

这个之所以没问题是因为 `path` 的调用者明显有权使用 `Tempfile` 并通过参数将之借给 `path` 。
在这个案例中，Rust 编译器将会保证返回的 `Path` 没有在超出拥有 `Tempfile` 的作用域之外存活。

实际上，这意味着你可以返回从上游借来的内容，然后 Rust 将处理好跟踪该内容的原容器。

让我们通过一个例子来举例说明：

```rust
fn hello() -> &str {
    let person = Person {
        first: "Yehuda".to_string(),
        last: "Katz".to_string(),
        age: 32
    };

    first_name(&person)
}

fn first_name(person: &Person) -> &str {
    // as_slice borrows a slice "view" out of a string
    person.first.as_slice()
}
```

如果你仔细观察，你可以立即看到问题所在。函数 `hello` 试图返回一个借用的 `&str` ，但是拥有包含被返回的字节的原 `Person` 的所有权在 `hello` 中。
一旦 `hello` 返回，那么 `Person` 将不复存在，导致借用的内容（字符串切片）指向了无效地地址。

如果试图编译这段代码，你将得到如下报错：

```nil
move.rs:8:15: 8:19 error: missing lifetime specifier [E0106]
move.rs:8 fn hello() -> &str {
                        ^~\~~
```

这个有点混乱地错误信息表示我们正尝试返回借用的字节，但是函数的调用者没有借给我们借用字节的来源 `Person` 。
Rust 正在向我们征询如果返回值不属于调用者作用域那么应该归属于那个「生命周期（lifetime）」。

> 通常情况下，Rust 会将返回值的作用域绑定到借用参数的作用域。这在里，我们没有借用的参数，所以 Rust 要求我们进行显式地定义。

实际情况下，这表示一个函数可以轻松地通过借用的方式返回借用参数中的内容。否则，你需要给该返回值找到一个调用者可以访问的存储位置，
或者克隆（clone）该值让调用者拥有一份自己的拷贝。


## 易用性（Ergonomics） {#易用性-ergonomics}

咋一看，所有权这些机制让人感觉很复杂，并且看起来可能会对使用 Rust 的易用性产生重大影响。可以肯定的是，一开始确实会有这种感觉。

但是有几个因素会让 Rust 的所有权变得比远看上去更加易用。

首先，大量的实际代码适用于借用/借出模式。随着我写了越来越多的 Rust 代码，我逐渐意识到用 Ruby 编写的程序遵循类似的模式：
函数创建一些对象并将它们传递给子函数执行某些任务，然后子函数返回新值。

当然，这是递归的，因此仅当差异（在于函数调用期间使用参数，以及延长参数的使用周期）在 Rust 中被显式区分时，它才能变得显而易见。
只有通过函数签名进行区分和全面覆盖，并检查错误，我们才能获得 Rust 提供的保证。

> 相反，C++ 只对部分情况进行了明显地区分，并且没有错误检查。带有垃圾回收机制的语言通常会隐藏“转移（transferred）”和“借出（lent）”参数。

如我上面所说，这意味着 Rust 程序员快速学习将借用作为默认行为去编写函数来减轻许多系统认知负担。

其次，随着使用一段时间 Rust 之后，大部分人意识到借用检查器错误和警告他们的都是真实地、严重地和比较难以发觉的错误。
一段时间以后，借用检查器自然地将您推入编程模式并且减少出现此类难以发觉地错误的影响。

再次，我个人发现，对所有权的清晰了解可以大大提高我对程序进行推理地能力，同时避免意外引入后面会花费大量地时间跟踪排查的内存泄漏问题。

最后，自动资源管理具有真正地易用性优势，既可以防止资源泄漏（当我懒惰时），又可以防止额外地样板代码和缩进（当我谨慎时）。

除 C++ 之外，很少程序员经历过自动资源管理为标准的编程环境，打开大脑的“blub”部分并认为它没有那么大的作用是非常非常容易的。
Rust 改变了这个领域中的传统权衡，我建议您将脑中告诉自己「我不需要它在《我选择的编程语言》，它有多重要？ ？」的小声音暂时先放回去。


## 引用计数（和垃圾回收） {#引用计数-和垃圾回收}

你可能已经注意到 Rust 已经有引用计数指针（并且计划未来实现 GC）。

它是如何在所有权系统中工作的呢？

以我的经验，一旦习惯了所有权范式，你会很少想要使用 `Rc` 指针。例如，整个 Cargo 代码库中没有使用引用计数指针的实例，
只使用了一次原子计数指针（用于在并行构建的代码的线程之间共享锁）。

我认为这是由于所有权非常明确，并且切实地改善了本地推理。如果你检查任意使用正常 Rust 引用的函数，
则可以轻易地知道一旦函数返回哪些内存（和资源）仍将存活，而哪些不会。例如，如果你使用闭包，
则可以立即判断它是否存活于当前函数作用域外，如果它确实存活于当前函数作用域外，你还可以知道闭包拥有哪些对象。

我也认为所有权和借出概念可以很好地映射到实际地编程模式。有一些事情你不可以做，但是大部分情况下，略微调整代码结构就可以通过编译。
作为交换，内存和资源泄漏都很少发生，并且代码清晰度得到了提高。

如果不是这种情况，我怀疑即使是经验丰富地 Rust 开发人员也会更频繁地使用 `Rc` 。

综上所述，在某些情况下，引用计数甚至垃圾回收也可以正常的在所有权系统下工作。 Rust 的 “智能指针” 系统允许 `Rc` 指针在相同地所有权和借用系统内透明地运行，
并且当引用计数减小到 0 时运行析构函数（伴随明显地本地推理和运行时性能上的成本）。


## 其他语言的机制（Facilities in Other Languages） {#其他语言的机制-facilities-in-other-languages}

带有垃圾回收机制的语言通常会提供一些机器辅助程序员手动管理资源。在大部分现代编程语言中，你不用显式的调用 `close` ，
但是你需要调整语言结构将资源与词法作用域联系在一起，然后在完成后进行释放。

让我们观察一些例子，然后我会讨论这些方法的缺点。

在 Ruby 中，你可以使用一个块标识你将在指定的作用域里使用资源。一旦块返回，资源将会被清理。

```ruby
File.open("/etc/passwd") do |file|
  # use the file
end
```

在 Python 中，一个特殊的语言关键字 `with` 用来创建一个协议进行资源获取，然后在代码块结束后释放资源：

```python
with open("/etc/passwd") as file:
  # use the file
```

Ruby 和 Python 都使用了通过调整语言结构和创建新协议的方法抽象了特定于资源关闭的机制。用户永远不知道关闭什么样子，
但是他们必须使用特殊的抽象来确保资源关闭被调用。

在 Go 中， `defer` 关键字允许程序员在原始创建逻辑之后提供清理逻辑来管理资源：

```go
file, error := os.Open("/etc/passwd")
if err != nil {
    return;
}
defer file.Close()

// use the file
```

这种方式比 `try/catch/finally` 有好一些，因为它保持清理逻辑紧跟资源获取逻辑，但是没有抽象关闭逻辑。

所有这些方法都有一系列的问题。再次，我建议你远离大脑中的很可能会告诉您这些问题“在实践中不会变得很重要”的“blub”中心。

-   向已经使用中的结构后面添加资源释放逻辑是不可能的，因为他们的使用者（clients）将会使用正常的对象创建 API。
    这会导致在更高层对象中抽象资源更加困难，因为资源管理需要暴漏到公共 API。
-   基于块的方法（Ruby 和 Python，不包含 Go）引入向右漂移。每次你想要使用一个资源，你都被强制创建一个新的作用域范围。
    这在 Ruby（有很好的块）和 Python（使用语言层面的结构）中相当烦人，在 JavaScript 中还有一个严重地问题，即引入新的作用域会阻止你返回或者摆脱当前循环。
-   这些方法（包含 Go 的 `defer` ）需要你在给定的词法作用域内使用资源。当你想要将资源传递给多个函数时将会引发尴尬（或不可能的）编程风格。
    实际上，它迫使语言使用一个不地道的基于作用域的所有权系统模型进行对象管理。
    -   一旦你开始使用资源调用其他函数，则可能意外地创建“释放后使用” 的 bug，如函数绑定在资源上（如闭包中），并在调用者关闭资源后尝试使用它。

Rust 中的自动资源管理可缓解所有这些问题：

-   资源管理对象可以定义一个析构器（destructor）抽象释放逻辑。通过正常创建一个对象就可以让析构器在正确的时间调用。
    对象可以在被使用之后添加析构函数而无需修改客户端代码。
    -   注意 Rust 中的析构器不同于带 GC 的语言中的析构器。它们总是在对象不在被使用后执行，并且一定会被执行，除了运行析构器本身不附带任何运行时开销。

-   由于资源管理和自动内存管理通过相同的方式工作，这将消除恼人的缩进并且不需要包围额外的代码。
-   在 Rust 中，你可以像传递其他类型的对象一样传递资源。如果你将所有权转移到其他作用域，资源将会在新作用域完成时被关闭。
    除此之外，借用系统将还会像保证内存一样保证资源不存在“释放后使用”。

简而言之，使用同一系统进行内存和资源管理确实有好处。

我不会说 Rust 所有权系统像垃圾回收一样不用耗费任何心力。 但是，Rust 已经做了很多非常聪明的事情来弥补，正如我们所看到的在某些情况下的易用性甚至超过了带垃圾回收机制的语言。

作为交换，你将获得一种非常快速的语言，并且可以绝对安全地直接控制内存。

因此，它开启了一个高级语言用户都可以编写低级代码的时代，这确实让我感到兴奋。同时在社区也可以找到很多人互相学习。

[^fn:1]: 当我说“无需”，我的意思是绝大部分都不需要。在带有垃圾回收的语言中，有时你最终还是会直接管理内存， 同样的在 Rust 中你最终还是会直接管理资源。重要的是在两种情况下，主要的编程模型是编程语言替你管理资源。
