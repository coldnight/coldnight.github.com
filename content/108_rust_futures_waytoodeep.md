Title: 【译】深入理解 Rust future
Date: 2021-07-29
Category: Rust
Tags: Rust,future,tokio
Slug: understanding-rust-futures-by-going-way-too-deep

原文链接：[Understanding Rust futures by going way too deep](https://fasterthanli.me/articles/understanding-rust-futures-by-going-way-too-deep)。

译者注：原文大量的引入了有趣的对话，迫于排版问题这里不进行翻译，必要的对话通过引用块来解释。


## 深入理解 Rust future {#深入理解-rust-future}

用 Rust future！就是这么简单！直到我们发现并非如此。所以我们先探索简单的部分，然后继续探索困难部分而不是等它慢慢靠近我们。


## 起步 {#起步}

> Choo choo here comes the easy part 🚂💨

我们创建一个新的项目：

```nil
$ cargo new waytoodeep
     Created binary (application) `waytoodeep` package
```

我们需要安装 `cargo-edit` 如果之前没有安装过的话，接下来就可以直接 `cargo add` ：

```nil
$ cargo install cargo-edit
    Updating crates.io index
  Downloaded cargo-edit v0.7.0
  Downloaded 1 crate (57.6 KB) in 0.47s
     Ignored package `cargo-edit v0.7.0` is already installed, use --force to override
```

> 因为 `cargo-edit` 很方便，所以你可能已经安装过它。部分读者会感到困惑是因为像
> `cargo new`, `cargo build`, `cargo test`, `cargo run` 等子命令都内置在 cargo 中，
> 但是 `cargo add` 没有。
>
> 实际上，有一大堆像这样的包，如 [cargo-hack](https://lib.rs/crates/cargo-hack),[cargo-udeps](https://lib.rs/crates/cargo-udeps),[cargo-expand](https://lib.rs/crates/cargo-expand)...[等等](https://lib.rs/keywords/cargo)。

然后我们需要选择一个「异步运行时」（async runtime），因为这些 future 对象不会轮询（poll）自己。。。
我们毫无理由的选择 tokio，唯一的原因是：过去几个月我一直在用它。

```nil
$ cargo add tokio@1.9.0 --features full
    Updating 'https://github.com/rust-lang/crates.io-index' index
      Adding tokio v1.9.0 to dependencies with features: ["full"]
```

然后我们修改 `main` 函数使用 tokio 默认执行器（executor）（ `cargo new` 为我们生成了一个 `main` 函数，但是这里并不能满足我们的需求）：

```rust
// in `src/main.rs`

#[tokio::main]
async fn main() {
    println!("Hello from a (so far completely unnecessary) async runtime");
}
```

```nil
$ cargo run                                                                                                                                                                                          3s 209ms
   Compiling waytoodeep v0.1.0 (/Users/wh/codes/rust/waytoodeep)
    Finished dev [unoptimized + debuginfo] target(s) in 3.47s
     Running `target/debug/waytoodeep`
Hello from a (so far completely unnecessary) async runtime
```

酷！

接下来让我们添加其他一些我喜欢在我的项目中使用的好东西。

首先，对于错误处理 - 我们编写程序就需要处理一堆不同库里不同的错误类型，如果能通过一个类型统一它们就会非常整洁。

[eyre](https://lib.rs/crates/eyre) 可以赋予我们这些（就像 `anyhow` ）！

并且因为我喜欢漂亮的颜色我将使用 [color-eyre](https://lib.rs/crates/color-eyre)。

```nil
$ cargo add color-eyre@0.5.11
    Updating 'https://github.com/rust-lang/crates.io-index' index
      Adding color-eyre v0.5.11 to dependencies
```

现在我们需要安装 `color-eyre` 作为默认的崩溃（panic）处理器，我悄悄修改了一些环境变量来默认输出调用堆栈（backtracks）。

```rust
use color_eyre::Report;

#[tokio::main]
async fn main() -> Result<(), Report> {
    setup()?;

    println!("Hello from a (so far completely unnecessary) async runtime");

    Ok(())
}

fn setup() -> Result<(), Report> {
    if std::env::var("RUST_LIB_BACKTRACE").is_err() {
        std::env::set_var("RUST_LIB_BACKTRACE", "1")
    }
    color_eyre::install()?;

    Ok(())
}
```

```nil
$ cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.02s
     Running `target/debug/waytoodeep`
Hello from a (so far completely unnecessary) async runtime
```

很好！现在如果我们某处出现了一个错误，我们将看到完整的堆栈跟踪，就像下面这样：
![](https://fasterthanli.me/content/articles/understanding-rust-futures-by-going-way-too-deep/assets/color-eyre.78931d5fc80841f6.webp)

最后，因为我喜欢结构化日志，让我们添加 [tracing](https://lib.rs/crates/tracing) 然后通过漂亮的颜色打印它们，让我们添加 [tracing-subscriber](https://lib.rs/crates/tracing-subscriber).

```nil
$ cargo add tracing@0.1.26 tracing-subscriber@0.2.19
    Updating 'https://github.com/rust-lang/crates.io-index' index
      Adding tracing v0.1.26 to dependencies
      Adding tracing-subscriber v0.2.19 to dependencies
```

我们已经有一个 `setup` 函数，所以直接在那里安装 `tracing-subscriber`.. 然后我们将 `println!` 改成 `info!` ！
然后，为了演示如何设置让我们再次修改一些环境变量：对所有包（crates）默认 `info` 日志级别。

```rust
use color_eyre::Report;
use tracing::info;
use tracing_subscriber::EnvFilter;

#[tokio::main]
async fn main() -> Result<(), Report> {
    setup()?;

    info!("Hello from a comfy nest we've made for ourselves");

    Ok(())
}

fn setup() -> Result<(), Report> {
    if std::env::var("RUST_LIB_BACKTRACE").is_err() {
        std::env::set_var("RUST_LIB_BACKTRACE", "1")
    }
    color_eyre::install()?;

    if std::env::var("RUST_LOG").is_err() {
        std::env::set_var("RUST_LOG", "info")
    }
    tracing_subscriber::fmt::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    Ok(())
}
```

```nil
$ cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.02s
     Running `target/debug/waytoodeep`
Jul 25 17:03:46.993  INFO waytoodeep: Hello from a comfy nest we've made for ourselves
```

好了，我们准备好做一些有用的事情了。


### 做一些有用的事情 {#做一些有用的事情}

当决定在咖啡间隙阅读哪一篇文章的时候，人们通常同时打开几个网站，然后读最先加载出来的那一篇。

事实如此。你可以引用我的话，谁会去验证呢？毕竟这听起来需要很多工作。

所以让我们来编写一个程序做相同的事情。

让我们引入 [reqwest](https://lib.rs/crates/reqwest) -- 尽管我不喜欢它的 API，但它会很好的完成接下来的工作。

同时，因为 [screw OpenSSL](https://www.openssl.org/news/vulnerabilities.html) 我们将标记 reqwest 使用 [rustls](https://lib.rs/crates/rustls)：

```nil
$ cargo add reqwest@0.11.4 --no-default-features --features rustls-tls
    Updating 'https://github.com/rust-lang/crates.io-index' index
      Adding reqwest v0.11.4 to dependencies with features: ["rustls-tls"]
```

我们准备好发送一个请求了！

```rust
use color_eyre::Report;
use tracing::info;
use tracing_subscriber::EnvFilter;
use reqwest::Client;

#[tokio::main]
async fn main() -> Result<(), Report> {
    setup()?;

    info!("Hello from a comfy nest we've made for ourselves");

    let client = Client::new();
    let url = "https://fasterthanli.me";
    // this will turn non-200 HTTP status codes into rust errors,
    // so the first `?` propagates "we had a connection problem" and
    // the second `?` propagates "we had a chat with the server and they
    // were not pleased"
    let res = client.get(url).send().await?.error_for_status()?;
    info!(%url, content_type = ?res.headers().get("content-type"), "Got a response!");


    Ok(())
}

fn setup() -> Result<(), Report> {
    if std::env::var("RUST_LIB_BACKTRACE").is_err() {
        std::env::set_var("RUST_LIB_BACKTRACE", "1")
    }
    color_eyre::install()?;

    if std::env::var("RUST_LOG").is_err() {
        std::env::set_var("RUST_LOG", "info")
    }
    tracing_subscriber::fmt::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    Ok(())
}
```

出发了！

```nil
cargo run
   Compiling waytoodeep v0.1.0 (/Users/wh/codes/rust/waytoodeep)
    Finished dev [unoptimized + debuginfo] target(s) in 7.16s
     Running `target/debug/waytoodeep`
Jul 26 16:50:57.778  INFO waytoodeep: Hello from a comfy nest we've made for ourselves
Jul 26 16:50:59.090  INFO waytoodeep: Got a response! url=https://fasterthanli.me content_type=Some("text/html; charset=utf-8")
```

这就是我所说的「结构化日志」。嗯，其中的一部分。让我们看下这行代码：

```rust
info!(%url, content_type = ?res.headers().get("content-type"), "Got a response!");
```

我们输出来一个消息： `Got a response!` ，一个名为 `url` 的标签：值为变量 `url` 的 [Display](https://doc.rust-lang.org/stable/std/fmt/trait.Display.html) 格式，
一个名为 `content_type` 的标签：值为表达式的 [Debug](https://doc.rust-lang.org/stable/std/fmt/trait.Debug.html) 格式。

就是这么简单！ `name = %value` 输出 `Display` ， `name = ?value` 输出 `Debug` 。

当然，还有非常棒的跨度（spans），重点是你可以将它们发送到 APM（Appliation Performance Monitoring），比如 Datadog 或者 Honeycomb 等，但是这不是一篇关于跟踪的文章。

为了举例说明，如果我们安装一个 JSON 的 tracing subscriber，我们将获得如下内容：

```nil
$ cargo run
   Compiling waytoodeep v0.1.0 (/home/amos/ftl/waytoodeep)
    Finished dev [unoptimized + debuginfo] target(s) in 3.09s
     Running `target/debug/waytoodeep`
{"timestamp":"Jul 25 17:17:21.531","level":"INFO","fields":{"message":"Hello from a comfy nest we've made for ourselves"},"target":"waytoodeep"}
{"timestamp":"Jul 25 17:17:21.709","level":"INFO","fields":{"message":"Got a response!","url":"https://fasterthanli.me","content_type":"Some(\"text/html; charset=utf-8\")"},"target":"waytoodeep"}
```

这应该足以激起你的兴趣。


### 同时获取两个地址 {#同时获取两个地址}

现在让我们获取两个地址：

```rust
pub const URL_1: &str = "https://fasterthanli.me/articles/whats-in-the-box";
pub const URL_2: &str = "https://fasterthanli.me/series/advent-of-code-2020/part-13";
```

。。。这是一个公平的比较。 这两篇文章都托管在我自己的网站上，绝对不是为了推广，而是为了使获取时间具有可比性，并且任一都有可能先加载完成（并且会随着时间的推移随机变化）。

我们将创建一个函数来获取内容：

```rust
async fn fetch_thing(client: &Client, url: &str) -> Result<(), Report> {
    let res = client.get(url).send().await?.error_for_status()?;
    info!(%url, content_type = ?res.headers().get("content-type"), "Got a response!");
    Ok(())
}
```

并使用它：

```rust
#[tokio::main]
async fn main() -> Result<(), Report> {
    setup()?;

    info!("Hello from a comfy nest we've made for ourselves");

    let client = Client::new();
    fetch_thing(&client, URL_1);
    fetch_thing(&client, URL_2);

    Ok(())
}
```

然后运行它:

```rust
$ cargo run
   Compiling waytoodeep v0.1.0 (/home/amos/ftl/waytoodeep)
warning: unused implementer of `Future` that must be used
  --> src/main.rs:15:5
   |
15 |     fetch_thing(&client, URL_1);
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   |
   = note: `#[warn(unused_must_use)]` on by default
   = note: futures do nothing unless you `.await` or poll them

warning: unused implementer of `Future` that must be used
  --> src/main.rs:16:5
   |
16 |     fetch_thing(&client, URL_2);
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   |
   = note: futures do nothing unless you `.await` or poll them

warning: 2 warnings emitted

    Finished dev [unoptimized + debuginfo] target(s) in 3.01s
     Running `target/debug/waytoodeep`
Jul 25 17:26:31.571  INFO waytoodeep: Hello from a comfy nest we've made for ourselves
```

奇怪的是，没有任何事情发生。

> 黄色的波浪线和恼人的 Rust 警告已经给出了提示。

让我们来修复它：

```rust
fetch_thing(&client, URL_1).await?;
fetch_thing(&client, URL_2).await?;
```

```nil
$ cargo run
   Compiling waytoodeep v0.1.0 (/home/amos/ftl/waytoodeep)
    Finished dev [unoptimized + debuginfo] target(s) in 3.17s
     Running `target/debug/waytoodeep`
Jul 25 17:27:29.768  INFO waytoodeep: Hello from a comfy nest we've made for ourselves
Jul 25 17:27:29.891  INFO waytoodeep: Got a response! url=https://fasterthanli.me/articles/whats-in-the-box content_type=Some("text/html; charset=utf-8")
Jul 25 17:27:29.974  INFO waytoodeep: Got a response! url=https://fasterthanli.me/series/advent-of-code-2020/part-13 content_type=Some("text/html; charset=utf-8")
```

所以，第零课：future 对象不做任何事情直到它们被轮询（polled）。

这是因为 future 对象几乎就是状态。让我们来创建一个：

```rust
// in `src/main.rs`

mod dumb;
```

```rust
// in `src/dumb.rs`

use std::{
    future::Future,
    pin::Pin,
    task::{Context, Poll},
};

use tracing::info;

pub struct DumbFuture {}

impl Future for DumbFuture {
    type Output = ();

    fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
        info!("Hello from a dumb future!");
        Poll::Ready(())
    }
}
```

```rust
// back in `src/main.rs`

#[tokio::main]
async fn main() -> Result<(), Report> {
    setup()?;

    let fut = dumb::DumbFuture {};

    Ok(())
}
```

以上！我们几乎就完成了，除了我们没有进行 `.await` 。

运行它除了打印警告不会有任何效果：

```nil
$ cargo run
   Compiling waytoodeep v0.1.0 (/home/amos/ftl/waytoodeep)
warning: unused variable: `fut`
  --> src/main.rs:14:9
   |
14 |     let fut = dumb::DumbFuture {};
   |         ^^^ help: if this is intentional, prefix it with an underscore: `_fut`
   |
   = note: `#[warn(unused_variables)]` on by default

warning: 1 warning emitted

    Finished dev [unoptimized + debuginfo] target(s) in 2.11s
     Running `target/debug/waytoodeep`
```

因为怎么可能？我们字面上仅仅构建了一个结构体。一个零大小的结构体。

如果我们调用它的 `.await` 。。 然后当我们要求运行时运行它的事件循环直到 future 对象被轮询（polled）并且最终返回 `Poll::Ready` （我们的代码立即返回）：

```rust
#[tokio::main]
async fn main() -> Result<(), Report> {
    setup()?;

    info!("Building that dumb future...");
    let fut = dumb::DumbFuture {};
    info!("Awaiting that dumb future...");
    fut.await;
    info!("Done awaiting that dumb future");

    Ok(())
}

```

```nil
$ cargo run
   Compiling waytoodeep v0.1.0 (/home/amos/ftl/waytoodeep)
    Finished dev [unoptimized + debuginfo] target(s) in 2.34s
     Running `target/debug/waytoodeep`
Jul 25 17:37:09.261  INFO waytoodeep: Building that dumb future...
Jul 25 17:37:09.261  INFO waytoodeep: Awaiting that dumb future...
Jul 25 17:37:09.261  INFO waytoodeep::dumb: Hello from a dumb future!
Jul 25 17:37:09.262  INFO waytoodeep: Done awaiting that dumb future
```

这里与 ECMAScript 的 `promise` 有一些略微的区别：即使它们压根没有被 await 其中包含的工作依然会被执行。

但是 Rust 的 future 对象仅仅是无聊的状态机，如果你故意制造麻烦就可以理解这个机制：

```rust
// in `src/dumb.rs`

impl Future for DumbFuture {
    type Output = ();

    fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
        panic!("Oh heck no");
    }
}
```

```nil
$ RUST_BACKTRACE=1 cargo run
   Compiling waytoodeep v0.1.0 (/home/amos/ftl/waytoodeep)
    Finished dev [unoptimized + debuginfo] target(s) in 2.28s
     Running `target/debug/waytoodeep`
Jul 25 17:41:18.956  INFO waytoodeep: Building that dumb future...
Jul 25 17:41:18.956  INFO waytoodeep: Awaiting that dumb future...
The application panicked (crashed).
Message:  Oh heck no
Location: src/dumb.rs:14

  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ BACKTRACE ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                                ⋮ 6 frames hidden ⋮
   7: <waytoodeep::dumb::DumbFuture as core::future::future::Future>::poll::h4a44780628f4c5f0
      at /home/amos/ftl/waytoodeep/src/dumb.rs:14
   8: waytoodeep::main::{{closure}}::h36de5a1f1f2a5c5b
      at /home/amos/ftl/waytoodeep/src/main.rs:17
   9: <core::future::from_generator::GenFuture<T> as core::future::future::Future>::poll::h20a96e082c7a581e
      at /home/amos/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/future/mod.rs:80
  10: tokio::park::thread::CachedParkThread::block_on::{{closure}}::hdf98cb3c7fdf3de4
      at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-1.9.0/src/park/thread.rs:263
  11: tokio::coop::with_budget::{{closure}}::h6a86a24a246e220f
      at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-1.9.0/src/coop.rs:106
  12: std::thread::local::LocalKey<T>::try_with::h2ce0ac27c85965b6
      at /home/amos/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/std/src/thread/local.rs:376
  13: std::thread::local::LocalKey<T>::with::hc449f38c9f65fb53
      at /home/amos/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/std/src/thread/local.rs:352
  14: tokio::coop::with_budget::h5db157bd1e95e0e8
      at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-1.9.0/src/coop.rs:99
  15: tokio::coop::budget::h7b57383f1255ac24
      at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-1.9.0/src/coop.rs:76
  16: tokio::park::thread::CachedParkThread::block_on::hece399485213b91c
      at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-1.9.0/src/park/thread.rs:263
  17: tokio::runtime::enter::Enter::block_on::h89e9882e539e82d3
      at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-1.9.0/src/runtime/enter.rs:151
  18: tokio::runtime::thread_pool::ThreadPool::block_on::h1a0186470c00ba70
      at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-1.9.0/src/runtime/thread_pool/mod.rs:71
  19: tokio::runtime::Runtime::block_on::h7c21d6989b86d606
      at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-1.9.0/src/runtime/mod.rs:452
  20: waytoodeep::main::hb4dd5ffd46a5c032
      at /home/amos/ftl/waytoodeep/src/main.rs:20
  21: core::ops::function::FnOnce::call_once::hc1fcc87431f77d25
      at /home/amos/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/ops/function.rs:227
                                ⋮ 11 frames hidden ⋮

Run with COLORBT_SHOW_HIDDEN=1 environment variable to disable frame filtering.
Run with RUST_BACKTRACE=full to include source snippets.
```

上面堆栈跟踪如果加上颜色效果会更好，所以我希望你在本地做了相同的尝试，即使如此我们依然可以看到我们真正的 main 函数在 20 帧，然后往上，我们可以看到 `Runtime::block_on`  、一个线程池的东西、一些挂起（parked）的线程、thread-local（其他 TLS）、一个 ****生成的**** future（帧 9 和 8，也就是我们的 `async fn main` 的最终结果），最后是我们的 `DumbFuture` poll 方法（帧 7）。

帧 6 到 1 就是 [panic](https://doc.rust-lang.org/stable/std/panic/index.html) 机制，再次完全超出本文讨论的范围。

但是请站起来，亲爱的观众，用你的手臂绕过这个装置，以确保没有障眼法，没有隐藏的线，没有。。。

。。。我要说的是对于异步堆栈跟踪没有“特殊处理”（special handling）。当然，这里我们崩溃了，但是仅仅是 Rust，操作系统甚至不知道我几乎避免了一场灾难。

但是我们可以制造更大的混乱，如果我们愿意使用 `unsafe` ：

```rust
impl Future for DumbFuture {
    type Output = ();

    fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
        unsafe {
            *(0xF00D as *mut u64) = 0x0;
        }
        unreachable!(); // pinky promise
    }
}
```

然后就不会有一些列的崩溃处理来拯救我们：

```nil
$ RUST_BACKTRACE=1 cargo run
   Compiling waytoodeep v0.1.0 (/home/amos/ftl/waytoodeep)
    Finished dev [unoptimized + debuginfo] target(s) in 2.18s
     Running `target/debug/waytoodeep`
Jul 25 17:46:53.926  INFO waytoodeep: Building that dumb future...
Jul 25 17:46:53.926  INFO waytoodeep: Awaiting that dumb future...
zsh: segmentation fault (core dumped)  RUST_BACKTRACE=1 cargo run
```

但是 GDB 可以：

```nil
$ cargo build && gdb --quiet --args ./target/debug/waytoodeep
    Finished dev [unoptimized + debuginfo] target(s) in 0.04s
Reading symbols from ./target/debug/waytoodeep...
warning: Missing auto-load script at offset 0 in section .debug_gdb_scripts
of file /home/amos/ftl/waytoodeep/target/debug/waytoodeep.
Use `info auto-load python-scripts [REGEXP]' to list them.
(gdb) r
Starting program: /home/amos/ftl/waytoodeep/target/debug/waytoodeep
[Thread debugging using libthread_db enabled]
Using host libthread_db library "/lib/x86_64-linux-gnu/libthread_db.so.1".
[New Thread 0x7ffff7c28700 (LWP 129418)]
[New Thread 0x7ffff7a27700 (LWP 129419)]
[New Thread 0x7ffff7826700 (LWP 129420)]
[New Thread 0x7ffff7625700 (LWP 129421)]
[New Thread 0x7ffff7424700 (LWP 129422)]
[New Thread 0x7ffff7223700 (LWP 129423)]
[New Thread 0x7ffff7022700 (LWP 129424)]
[New Thread 0x7ffff6e1e700 (LWP 129425)]
[New Thread 0x7ffff6c1a700 (LWP 129426)]
[New Thread 0x7ffff6a16700 (LWP 129427)]
[New Thread 0x7ffff6812700 (LWP 129428)]
[New Thread 0x7ffff660e700 (LWP 129429)]
[New Thread 0x7ffff640a700 (LWP 129430)]
[New Thread 0x7ffff6206700 (LWP 129431)]
[New Thread 0x7ffff6002700 (LWP 129432)]
Jul 25 17:47:13.278  INFO waytoodeep: Building that dumb future...
Jul 25 17:47:13.279  INFO waytoodeep: Awaiting that dumb future...

Thread 1 "waytoodeep" received signal SIGSEGV, Segmentation fault.
<waytoodeep::dumb::DumbFuture as core::future::future::Future>::poll (self=..., _cx=0x7fffffffd690) at src/dumb.rs:15
15                  *(0xF00D as *mut u64) = 0x0;
(gdb) bt
#0  <waytoodeep::dumb::DumbFuture as core::future::future::Future>::poll (self=..., _cx=0x7fffffffd690) at src/dumb.rs:15
#1  0x00005555555ab3a3 in waytoodeep::main::{{closure}} () at src/main.rs:17
#2  0x00005555555adb29 in <core::future::from_generator::GenFuture<T> as core::future::future::Future>::poll (self=..., cx=0x7fffffffd690)
    at /home/amos/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/future/mod.rs:80
#3  0x00005555555adaa0 in tokio::park::thread::CachedParkThread::block_on::{{closure}} ()
    at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-1.9.0/src/park/thread.rs:263
#4  0x00005555555b1742 in tokio::coop::with_budget::{{closure}} (cell=0x7ffff7c2c412)
    at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-1.9.0/src/coop.rs:106
#5  0x00005555555a9f58 in std::thread::local::LocalKey<T>::try_with (self=0x555555925fc0, f=...)
    at /home/amos/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/std/src/thread/local.rs:376
#6  0x00005555555a9e3d in std::thread::local::LocalKey<T>::with (self=0x555555925fc0, f=...)
    at /home/amos/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/std/src/thread/local.rs:352
#7  0x00005555555ad7c8 in tokio::coop::with_budget (budget=..., f=...)
    at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-1.9.0/src/coop.rs:99
#8  tokio::coop::budget (f=...) at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-1.9.0/src/coop.rs:76
#9  tokio::park::thread::CachedParkThread::block_on (self=0x7fffffffd7a0, f=...)
    at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-1.9.0/src/park/thread.rs:263
#10 0x00005555555abcc9 in tokio::runtime::enter::Enter::block_on (self=0x7fffffffd7f0, f=...)
    at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-1.9.0/src/runtime/enter.rs:151
#11 0x00005555555acf2e in tokio::runtime::thread_pool::ThreadPool::block_on (self=0x7fffffffd908, future=...)
    at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-1.9.0/src/runtime/thread_pool/mod.rs:71
#12 0x00005555555b0dfd in tokio::runtime::Runtime::block_on (self=0x7fffffffd900, future=...)
    at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-1.9.0/src/runtime/mod.rs:452
#13 0x00005555555aa807 in waytoodeep::main () at src/main.rs:20
(gdb)
```

我们再次丢失了高亮颜色，这里可以看一下：
![](https://fasterthanli.me/content/articles/understanding-rust-futures-by-going-way-too-deep/assets/gdb-colors.b45af429c46a37d9.webp)

> 译注：我在本地环境并没有通过 GDB 复现带高亮的堆栈跟踪，反而是通过 LLDB 可以看到高亮的堆栈跟踪。

是不是很漂亮？

现在让我们回到正常有用的代码，移除所有关于自己实现的 future 代码： `src/dumb.rs` 和 `mod dumb` 。并使用一个获取 future 替代：

```rust
#[tokio::main]
async fn main() -> Result<(), Report> {
    setup()?;

    info!("Building that fetch future...");
    let client = Client::new();
    let fut = fetch_thing(&client, URL_1);
    info!("Awaiting that fetch future...");
    fut.await?;
    info!("Done awaiting that fetch future");

    Ok(())
}
```

```nil
$ RUST_BACKTRACE=1 cargo run
   Compiling waytoodeep v0.1.0 (/home/amos/ftl/waytoodeep)
    Finished dev [unoptimized + debuginfo] target(s) in 2.99s
     Running `target/debug/waytoodeep`
Jul 25 17:51:49.281  INFO waytoodeep: Building that fetch future...
Jul 25 17:51:49.282  INFO waytoodeep: Awaiting that fetch future...
Jul 25 17:51:49.437  INFO waytoodeep: Got a response! url=https://fasterthanli.me/articles/whats-in-the-box content_type=Some("text/html; charset=utf-8")
Jul 25 17:51:49.438  INFO waytoodeep: Done awaiting that fetch future
```

有两种方式考虑我们的函数，一个是语法糖层：也就是 `async fn` ：

```rust
async fn fetch_thing(client: &Client, url: &str) -> Result<(), Report> {
    let res = client.get(url).send().await?.error_for_status()?;
    info!(%url, content_type = ?res.headers().get("content-type"), "Got a response!");
    Ok(())
}
```

然后是核心实现层：一个普通的 `fn` 仅用来返回一个 future 对象：

```rust
use std::future::Future;

fn fetch_thing<'a>(
    client: &'a Client,
    url: &'a str,
) -> impl Future<Output = Result<(), Report>> + 'a {
    async move {
        let res = client.get(url).send().await?.error_for_status()?;
        info!(%url, content_type = ?res.headers().get("content-type"), "Got a response!");
        Ok(())
    }
}
```

由于借用 `client` 和 `url` ，所以 `Future` 对象的存活时间不能超过两者，这也是为什么我会将上面两个生命周期命名为 `'a` ，
并且返回的值也是任意实现了 `Future` （通过 `Output` ）同时生命周期也是 `'a` 。

整个 `async move {}` 快也仅仅是“构建状态” -- 等于一个实现了 `Future` 的类型。

我们只是无法命名它。

我们只能尽量获取它的描述：

```rust
fn type_name_of<T>(_: &T) -> &'static str {
    std::any::type_name::<T>()
}

// in main

#[tokio::main]
async fn main() -> Result<(), Report> {
    setup()?;

    info!("Building that fetch future...");
    let client = Client::new();
    let fut = fetch_thing(&client, URL_1);
    info!(
        type_name = type_name_of(&fut),
        "That fetch future has a type.."
    );
    info!("Awaiting that fetch future...");
    fut.await?;
    info!("Done awaiting that fetch future");

    Ok(())
}
```

```nil
$ cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.05s
     Running `target/debug/waytoodeep`
Jul 25 18:00:39.774  INFO waytoodeep: Building that fetch future...
Jul 25 18:00:39.775  INFO waytoodeep: That fetch future has a type.. type_name="core::future::from_generator::GenFuture<waytoodeep::fetch_thing::{{closure}}>"
Jul 25 18:00:39.775  INFO waytoodeep: Awaiting that fetch future...
Jul 25 18:00:39.882  INFO waytoodeep: Got a response! url=https://fasterthanli.me/articles/whats-in-the-box content_type=Some("text/html; charset=utf-8")
Jul 25 18:00:39.882  INFO waytoodeep: Done awaiting that fetch future
```

。。。但是等等，由于我们使用了 `async` 语法所以它是一个编译器生成的类型。某种意义上我们无法命名它也就意味这我们无法绑定这个对象，或者编写一个函数仅仅接受该类型。

为了让我们自己相信 future 对象在我们真正轮询它之前它不会做任何工作，我们可以打开 `reqwest` 的调试日志：

```nil
$ RUST_LOG=info,reqwest=debug cargo run
   Compiling waytoodeep v0.1.0 (/home/amos/ftl/waytoodeep)
    Finished dev [unoptimized + debuginfo] target(s) in 3.07s
     Running `target/debug/waytoodeep`
Jul 25 18:05:07.384  INFO waytoodeep: Building that fetch future...
Jul 25 18:05:07.385  INFO waytoodeep: That fetch future has a type.. type_name="core::future::from_generator::GenFuture<waytoodeep::fetch_thing::{{closure}}>"
Jul 25 18:05:07.385  INFO waytoodeep: Awaiting that fetch future...
Jul 25 18:05:07.385 DEBUG reqwest::connect: starting new connection: https://fasterthanli.me/
Jul 25 18:05:07.503 DEBUG reqwest::async_impl::client: response '200 OK' for https://fasterthanli.me/articles/whats-in-the-box
Jul 25 18:05:07.503  INFO waytoodeep: Got a response! url=https://fasterthanli.me/articles/whats-in-the-box content_type=Some("text/html; charset=utf-8")
Jul 25 18:05:07.503  INFO waytoodeep: Done awaiting that fetch future
```

甚至对于每一个包（crate），我们都可以通过监听 [hyper](https://lib.rs/crates/hyper) 和 [h2](https://lib.rs/crates/h2) 来观察：

```nil
$ RUST_LOG=debug cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.04s
     Running `target/debug/waytoodeep`
Jul 25 18:05:59.973  INFO waytoodeep: Building that fetch future...
Jul 25 18:05:59.973  INFO waytoodeep: That fetch future has a type.. type_name="core::future::from_generator::GenFuture<waytoodeep::fetch_thing::{{closure}}>"
Jul 25 18:05:59.973  INFO waytoodeep: Awaiting that fetch future...
Jul 25 18:05:59.974 DEBUG reqwest::connect: starting new connection: https://fasterthanli.me/
Jul 25 18:05:59.974 DEBUG hyper::client::connect::dns: resolving host="fasterthanli.me"
Jul 25 18:05:59.989 DEBUG hyper::client::connect::http: connecting to 172.67.196.144:443
Jul 25 18:06:00.000 DEBUG hyper::client::connect::http: connected to 172.67.196.144:443
Jul 25 18:06:00.000 DEBUG rustls::client::hs: No cached session for DNSNameRef("fasterthanli.me")
Jul 25 18:06:00.000 DEBUG rustls::client::hs: Not resuming any session
Jul 25 18:06:00.016 DEBUG rustls::client::hs: Using ciphersuite TLS13_CHACHA20_POLY1305_SHA256
Jul 25 18:06:00.016 DEBUG rustls::client::tls13: Not resuming
Jul 25 18:06:00.017 DEBUG rustls::client::tls13: TLS1.3 encrypted extensions: [ServerNameAck, Protocols([PayloadU8([104, 50])])]
Jul 25 18:06:00.017 DEBUG rustls::client::hs: ALPN protocol is Some(b"h2")
Jul 25 18:06:00.018 DEBUG h2::client: binding client connection
Jul 25 18:06:00.018 DEBUG h2::client: client connection bound
Jul 25 18:06:00.018 DEBUG h2::codec::framed_write: send frame=Settings { flags: (0x0), enable_push: 0, initial_window_size: 2097152, max_frame_size: 16384 }
Jul 25 18:06:00.019 DEBUG Connection{peer=Client}: h2::codec::framed_write: send frame=WindowUpdate { stream_id: StreamId(0), size_increment: 5177345 }
Jul 25 18:06:00.019 DEBUG hyper::client::pool: pooling idle connection for ("https", fasterthanli.me)
Jul 25 18:06:00.020 DEBUG Connection{peer=Client}: h2::codec::framed_write: send frame=Headers { stream_id: StreamId(1), flags: (0x5: END_HEADERS | END_STREAM) }
Jul 25 18:06:00.029 DEBUG Connection{peer=Client}: rustls::client::tls13: Ticket saved
Jul 25 18:06:00.029 DEBUG Connection{peer=Client}: rustls::client::tls13: Ticket saved
Jul 25 18:06:00.029 DEBUG Connection{peer=Client}: h2::codec::framed_read: received frame=Settings { flags: (0x0), max_concurrent_streams: 256, initial_window_size: 65536, max_frame_size: 16777215 }
Jul 25 18:06:00.030 DEBUG Connection{peer=Client}: h2::codec::framed_write: send frame=Settings { flags: (0x1: ACK) }
Jul 25 18:06:00.030 DEBUG Connection{peer=Client}: h2::codec::framed_read: received frame=WindowUpdate { stream_id: StreamId(0), size_increment: 2147418112 }
Jul 25 18:06:00.041 DEBUG Connection{peer=Client}: h2::codec::framed_read: received frame=Settings { flags: (0x1: ACK) }
Jul 25 18:06:00.041 DEBUG Connection{peer=Client}: h2::proto::settings: received settings ACK; applying Settings { flags: (0x0), enable_push: 0, initial_window_size: 2097152, max_frame_size: 16384 }
Jul 25 18:06:00.120 DEBUG Connection{peer=Client}: h2::codec::framed_read: received frame=Headers { stream_id: StreamId(1), flags: (0x4: END_HEADERS) }
Jul 25 18:06:00.120 DEBUG Connection{peer=Client}: h2::codec::framed_read: received frame=Data { stream_id: StreamId(1) }
Jul 25 18:06:00.121 DEBUG reqwest::async_impl::client: response '200 OK' for https://fasterthanli.me/articles/whats-in-the-box
Jul 25 18:06:00.121  INFO waytoodeep: Got a response! url=https://fasterthanli.me/articles/whats-in-the-box content_type=Some("text/html; charset=utf-8")
Jul 25 18:06:00.121  INFO waytoodeep: Done awaiting that fetch future
Jul 25 18:06:00.121 DEBUG Connection{peer=Client}: h2::codec::framed_read: received frame=Data { stream_id: StreamId(1) }
Jul 25 18:06:00.122 DEBUG Connection{peer=Client}: h2::codec::framed_write: send frame=Reset { stream_id: StreamId(1), error_code: CANCEL }
Jul 25 18:06:00.122 DEBUG Connection{peer=Client}: h2::codec::framed_write: send frame=GoAway { error_code: NO_ERROR, last_stream_id: StreamId(0) }
Jul 25 18:06:00.122 DEBUG Connection{peer=Client}: h2::proto::connection: Connection::poll; connection error error=NO_ERROR
Jul 25 18:06:00.122 DEBUG Connection{peer=Client}: rustls::session: Sending warning alert CloseNotify
```

> 上面出现了 rustls，并且使用了 TLS 1.3，作者做过[一期视频](https://www.youtube.com/watch?v=YHIiVsFybLA)介绍过 TLS 1.3。

这些应该足够说服你，除非你只相信内核所说的，所以让我们看看调用堆栈只为了更加确定。

我们在 `await` future 对象之前增加一秒钟的休眠：

```rust
use tokio::time::sleep;
use std::time::Duration;

#[tokio::main]
async fn main() -> Result<(), Report> {
    setup()?;

    info!("Building that fetch future...");
    let client = Client::new();
    let fut = fetch_thing(&client, URL_1);
    info!("Sleeping for a bit...");
    sleep(Duration::from_secs(1)).await;
    info!("Awaiting that fetch future...");
    fut.await?;
    info!("Done awaiting that fetch future");

    Ok(())
}
```

```nil
$ cargo build && strace -e 'connect' ./target/debug/waytoodeep
   Compiling waytoodeep v0.1.0 (/home/amos/ftl/waytoodeep)
    Finished dev [unoptimized + debuginfo] target(s) in 3.13s
Jul 25 18:09:36.595  INFO waytoodeep: Building that fetch future...
Jul 25 18:09:36.596  INFO waytoodeep: Sleeping for a bit...
Jul 25 18:09:37.599  INFO waytoodeep: Awaiting that fetch future...
connect(9, {sa_family=AF_INET, sin_port=htons(443), sin_addr=inet_addr("104.21.92.169")}, 16) = -1 EINPROGRESS (Operation now in progress)
Jul 25 18:09:37.720  INFO waytoodeep: Got a response! url=https://fasterthanli.me/articles/whats-in-the-box content_type=Some("text/html; charset=utf-8")
Jul 25 18:09:37.721  INFO waytoodeep: Done awaiting that fetch future
+++ exited with 0 +++
```

再次强调，附上会让显著提高上面信息的可读性，如果不让我选择它们的话我是非常喜欢高亮的。我本地看起来是这样的：
![](https://fasterthanli.me/content/articles/understanding-rust-futures-by-going-way-too-deep/assets/strace-colors.a4163f4bda179c2b.webp)
由于 `tracing-subscriber` 默认格式会输出时间戳，可以看到程序休眠了1分钟（外加3毫秒），而且只有我们真正调用 `await` 时我们的程序才会开始连接到托管文章的 CDN 节点。

好了！让我们再次尝试拉取两篇文章：

```rust
#[tokio::main]
async fn main() -> Result<(), Report> {
    setup()?;

    let client = Client::new();

    let fut1 = fetch_thing(&client, URL_1);
    let fut2 = fetch_thing(&client, URL_2);

    fut1.await?;
    fut2.await?;

    Ok(())
}
```

再次检查日志：

```nil
$ RUST_LOG=info,reqwest=debug cargo run --quiet
Jul 25 18:31:47.396 DEBUG reqwest::connect: starting new connection: https://fasterthanli.me/
Jul 25 18:31:47.536 DEBUG reqwest::async_impl::client: response '200 OK' for https://fasterthanli.me/articles/whats-in-the-box
Jul 25 18:31:47.537  INFO waytoodeep: Got a response! url=https://fasterthanli.me/articles/whats-in-the-box content_type=Some("text/html; charset=utf-8")
Jul 25 18:31:47.627 DEBUG reqwest::async_impl::client: response '200 OK' for https://fasterthanli.me/series/advent-of-code-2020/part-13
Jul 25 18:31:47.627  INFO waytoodeep: Got a response! url=https://fasterthanli.me/series/advent-of-code-2020/part-13 content_type=Some("text/html; charset=utf-8")
```

非常有趣。从这里可以看到， `reqwest` 为两个请求复用了相同的连接。我会这么说是因我只看到了一行 `reqwest::connect` 日志。

让我们快速通过 `strace` 检查一下：

```nil
$ cargo build --quiet && strace -e 'connect' ./target/debug/waytoodeep > /dev/null
connect(9, {sa_family=AF_INET, sin_port=htons(443), sin_addr=inet_addr("172.67.196.144")}, 16) = -1 EINPROGRESS (Operation now in progress)
+++ exited with 0 +++
```

现在可以确认了，只有一次连接。

但是，第一个请求完成后才开始了第二个请求。第一个耗费了 `536-396 = 140` 毫秒，但是第二个耗费了 `627-537 = 90` 毫秒！

> Emmm，现在我们运行构建的是 debug 版本不是吗？

这是真的。我确信我们面临的是 IO 密集型，而不是 CPU 密集型。

debug 版本的构建绝对有一些额外的开销，但是我怀疑这里它不会太影响延迟。无论如何，让我们检查一下：
（注意 --release）

```nil
$ RUST_LOG=info,reqwest=debug cargo run --quiet --release
Jul 25 18:34:59.211 DEBUG reqwest::connect: starting new connection: https://fasterthanli.me/
Jul 25 18:34:59.343 DEBUG reqwest::async_impl::client: response '200 OK' for https://fasterthanli.me/articles/whats-in-the-box
Jul 25 18:34:59.343  INFO waytoodeep: Got a response! url=https://fasterthanli.me/articles/whats-in-the-box content_type=Some("text/html; charset=utf-8")
Jul 25 18:34:59.427 DEBUG reqwest::async_impl::client: response '200 OK' for https://fasterthanli.me/series/advent-of-code-2020/part-13
Jul 25 18:34:59.427  INFO waytoodeep: Got a response! url=https://fasterthanli.me/series/advent-of-code-2020/part-13 content_type=Some("text/html; charset=utf-8")
```

我们计算一下延迟 `343-211 = 132ms` ， `427-343 = 84ms` 。

几毫秒的差异可能的解释是邻居打开了一个 YouTube 视频导致无线电波爆发，从而导致冲突（802.11 没有空中流量控制，全民自由（free-for-all））和重传。

或者另外一百万个原因。这也是我们不继续分析的原因。

让我们回到文章的主题。


### 等待第一个完成 {#等待第一个完成}

是的！等待第一个完成。所以我们如何让程序同时请求两个？

其实有一大堆方式！

例如，我们可以在一个执行器上执行（ `spawn` ）这些 future 对象，然后休眠一秒钟。1 秒钟足够了吧？

```rust
#[tokio::main]
async fn main() -> Result<(), Report> {
    setup()?;

    let client = Client::new();

    let fut1 = fetch_thing(&client, URL_1);
    tokio::spawn(fut1);
    let fut2 = fetch_thing(&client, URL_2);
    tokio::spawn(fut2);

    tokio::time::sleep(Duration::from_secs(1)).await;

    Ok(())
}
```

```nil
$ RUST_LOG=info,reqwest=debug cargo run --quiet --release
error[E0597]: `client` does not live long enough
  --> src/main.rs:17:28
   |
17 |     let fut1 = fetch_thing(&client, URL_1);
   |                ------------^^^^^^^--------
   |                |           |
   |                |           borrowed value does not live long enough
   |                argument requires that `client` is borrowed for `'static`
...
25 | }
   | - `client` dropped here while still borrowed

error: aborting due to previous error

For more information about this error, try `rustc --explain E0597`.
error: could not compile `waytoodeep`

To learn more, run the command again with --verbose.
```

额，除非我们不可以。不可以是因为。。。

> 我们将「future 对象交给执行器执行」并将 future 对象转交给执行器，对吧？我们转移了它和它的内容的所有权。
>
> 然后即使我们不对其进行 `await` ，future 对象因为是「执行器需要做」的一部分依然会被执行，所以即使我们从 `main` 返回 future 对象也会被轮询（polled）。
>
> 但是如果我们从 `main` 返回，则整个程序都会退出。
>
> 这里也可以是任何函数（这里是 `main` ）。重要的是如果函数返回了但是 future 对象借用了部分数据将无法通过借用检查器。

这让我很高兴，因为这意味着我们不会意外访问到一些被释放的资源：[UAF](https://cve.mitre.org/cgi-bin/cvekey.cgi?keyword=use+after+free)。

这里我们的例子没有完成。

所以。。。我们需要解决这个问题。如果 `fetch_thing` 返回的 future 对象是 `'static` 的呢？或者它不借用任何东西？

程序现在看起来如下：

```rust
use std::future::Future;

fn fetch_thing<'a>(
    client: &'a Client,
    url: &'a str,
) -> impl Future<Output = Result<(), Report>> + 'a {
    async move {
        let res = client.get(url).send().await?.error_for_status()?;
        info!(%url, content_type = ?res.headers().get("content-type"), "Got a response!");
        Ok(())
    }
}
```

好吧，之前我们用了 `async fn` ，但是为了更加深入的理解，我们不得不放弃漂亮的语法。

但是幸运的是，这正是我们想要的：

```rust
fn fetch_thing<'a>(
    client: &'a Client,
    url: &'a str,
//                                                 👇
) -> impl Future<Output = Result<(), Report>> + 'static {}
```

但是我们借用了 `client` 和 `url` 我们必须避免这个问题。

因为 `url` 本身就是常量，所以很容易解决：

```rust
pub const URL_1: &str = "https://fasterthanli.me/articles/whats-in-the-box";
pub const URL_2: &str = "https://fasterthanli.me/series/advent-of-code-2020/part-13";
```

它们本身就是 `'static` 。所以我们只需要调整需要 `'static` 就行:

```rust
fn fetch_thing<'a>(
    client: &'a Client,
    //       👇
    url: &'static str,
) -> impl Future<Output = Result<(), Report>> + 'static {}
```

非常好！解决了一个生命周期，还剩下一个。

我们可以要求 `client` 的生命周期为 `'static` 。由于它是一个  `Client` 的引用，意味着 `Cleint` 本身也需要是 `'static` 生命周期。

```rust
fn fetch_thing(
    //         👇
    client: &'static Client,
    url: &'static str,
) -> impl Future<Output = Result<(), Report>> + 'static {}
```

由于它被 `main` 所有，额，我们可以，可以。。。可以泄漏它：

```rust
#[tokio::main]
async fn main() -> Result<(), Report> {
    setup()?;

    let client = Client::new();
    let leaked_client = Box::leak(Box::new(client));

    let fut1 = fetch_thing(leaked_client, URL_1);
    let fut2 = fetch_thing(leaked_client, URL_2);

    tokio::spawn(fut1);
    tokio::spawn(fut2);

    tokio::time::sleep(Duration::from_secs(1)).await;

    Ok(())
}
```

完美！没有生命周期的问题了。

仅仅将所有东西泄漏就行。看到没？你不需要 C！

```nil
$ RUST_LOG=info,reqwest=debug cargo run --quiet --release
Jul 25 18:54:53.614 DEBUG reqwest::connect: starting new connection: https://fasterthanli.me/
Jul 25 18:54:53.614 DEBUG reqwest::connect: starting new connection: https://fasterthanli.me/
Jul 25 18:54:53.708 DEBUG reqwest::async_impl::client: response '200 OK' for https://fasterthanli.me/articles/whats-in-the-box
Jul 25 18:54:53.708  INFO waytoodeep: Got a response! url=https://fasterthanli.me/articles/whats-in-the-box content_type=Some("text/html; charset=utf-8")
Jul 25 18:54:53.733 DEBUG reqwest::async_impl::client: response '200 OK' for https://fasterthanli.me/series/advent-of-code-2020/part-13
Jul 25 18:54:53.733  INFO waytoodeep: Got a response! url=https://fasterthanli.me/series/advent-of-code-2020/part-13 content_type=Some("text/html; charset=utf-8")
```

非～～常有趣！

我们的两个请求肯定是并发的发出去了，我们之所以知道是因为从我的笔记本上请求我的网站大概耗时 80ms 到 140ms 之间，但是在日志中我们看到两个响应之间只有 ~25ms 的间隔。

我们还可以看到 `reqwest` 有连接池机制：同时创建了两个连接。可能是因为我们开始第二个连接的时候第一个请求的连接还没有建立完成。

也就意味着我们通过 `strace` 可以看到：

```nil
$ cargo build --quiet --release && strace -e 'connect' ./target/release/waytoodeep
Jul 25 18:58:16.425  INFO waytoodeep: Got a response! url=https://fasterthanli.me/articles/whats-in-the-box content_type=Some("text/html; charset=utf-8")
Jul 25 18:58:16.443  INFO waytoodeep: Got a response! url=https://fasterthanli.me/series/advent-of-code-2020/part-13 content_type=Some("text/html; charset=utf-8")
+++ exited with 0 +++
```

。。。两个 `connect` 调用！如我所料！

> 谬论：一个 `connect` 调用都没看到？因为 Rust 构建 HTTP/2 请求的时候甚至都需要建立 TCP 连接。真是革命性的！

这当然不是真的。可能在其他线程执行了？也许 `strace` 默认仅跟踪了主线程？

啊，对了， `-f` 可以跟踪所有「子进程」，就像大家知道的那样 Linux 线程仅仅是披了件风衣的进程（或者其他方式）。所以，让我们看一下：

```nil
$ cargo build --quiet --release && strace -f -e 'connect' ./target/release/waytoodeep
strace: Process 154612 attached
strace: Process 154613 attached
strace: Process 154614 attached
strace: Process 154615 attached
strace: Process 154616 attached
strace: Process 154617 attached
strace: Process 154618 attached
strace: Process 154619 attached
strace: Process 154620 attached
strace: Process 154621 attached
strace: Process 154622 attached
strace: Process 154623 attached
strace: Process 154624 attached
strace: Process 154625 attached
strace: Process 154626 attached
strace: Process 154627 attached
strace: Process 154628 attached
[pid 154627] connect(9, {sa_family=AF_UNIX, sun_path="/var/run/nscd/socket"}, 110) = -1 ENOENT (No such file or directory)
[pid 154628] connect(10, {sa_family=AF_UNIX, sun_path="/var/run/nscd/socket"}, 110) = -1 ENOENT (No such file or directory)
[pid 154627] connect(9, {sa_family=AF_UNIX, sun_path="/var/run/nscd/socket"}, 110) = -1 ENOENT (No such file or directory)
[pid 154628] connect(9, {sa_family=AF_INET, sin_port=htons(53), sin_addr=inet_addr("127.0.0.53")}, 16) = 0
[pid 154627] connect(10, {sa_family=AF_INET, sin_port=htons(53), sin_addr=inet_addr("127.0.0.53")}, 16) = 0
[pid 154627] connect(9, {sa_family=AF_INET6, sin6_port=htons(0), sin6_flowinfo=htonl(0), inet_pton(AF_INET6, "2606:4700:3034::6815:5ca9", &sin6_addr), sin6_scope_id=0}, 28) = -1 ENETUNREACH (Network is unreachable)
[pid 154627] connect(9, {sa_family=AF_UNSPEC, sa_data="\0\0\0\0\0\0\0\0\0\0\0\0\0\0"}, 16) = 0
[pid 154627] connect(9, {sa_family=AF_INET6, sin6_port=htons(0), sin6_flowinfo=htonl(0), inet_pton(AF_INET6, "2606:4700:3031::ac43:c490", &sin6_addr), sin6_scope_id=0}, 28) = -1 ENETUNREACH (Network is unreachable)
[pid 154627] connect(9, {sa_family=AF_UNSPEC, sa_data="\0\0\0\0\0\0\0\0\0\0\0\0\0\0"}, 16) = 0
[pid 154627] connect(9, {sa_family=AF_INET, sin_port=htons(0), sin_addr=inet_addr("104.21.92.169")}, 16) = 0
[pid 154627] connect(9, {sa_family=AF_UNSPEC, sa_data="\0\0\0\0\0\0\0\0\0\0\0\0\0\0"}, 16) = 0
[pid 154627] connect(9, {sa_family=AF_INET, sin_port=htons(0), sin_addr=inet_addr("172.67.196.144")}, 16) = 0
[pid 154628] connect(10, {sa_family=AF_INET6, sin6_port=htons(0), sin6_flowinfo=htonl(0), inet_pton(AF_INET6, "2606:4700:3034::6815:5ca9", &sin6_addr), sin6_scope_id=0}, 28) = -1 ENETUNREACH (Network is unreachable)
[pid 154628] connect(10, {sa_family=AF_UNSPEC, sa_data="\0\0\0\0\0\0\0\0\0\0\0\0\0\0"}, 16) = 0
[pid 154628] connect(10, {sa_family=AF_INET6, sin6_port=htons(0), sin6_flowinfo=htonl(0), inet_pton(AF_INET6, "2606:4700:3031::ac43:c490", &sin6_addr), sin6_scope_id=0}, 28) = -1 ENETUNREACH (Network is unreachable)
[pid 154628] connect(10, {sa_family=AF_UNSPEC, sa_data="\0\0\0\0\0\0\0\0\0\0\0\0\0\0"}, 16) = 0
[pid 154628] connect(10, {sa_family=AF_INET, sin_port=htons(0), sin_addr=inet_addr("104.21.92.169")}, 16) = 0
[pid 154628] connect(10, {sa_family=AF_UNSPEC, sa_data="\0\0\0\0\0\0\0\0\0\0\0\0\0\0"}, 16) = 0
[pid 154628] connect(10, {sa_family=AF_INET, sin_port=htons(0), sin_addr=inet_addr("172.67.196.144")}, 16) = 0
[pid 154625] connect(9, {sa_family=AF_INET, sin_port=htons(443), sin_addr=inet_addr("104.21.92.169")}, 16) = -1 EINPROGRESS (Operation now in progress)
[pid 154626] connect(10, {sa_family=AF_INET, sin_port=htons(443), sin_addr=inet_addr("104.21.92.169")}, 16) = -1 EINPROGRESS (Operation now in progress)
Jul 25 19:00:53.862  INFO waytoodeep: Got a response! url=https://fasterthanli.me/articles/whats-in-the-box content_type=Some("text/html; charset=utf-8")
Jul 25 19:00:53.880  INFO waytoodeep: Got a response! url=https://fasterthanli.me/series/advent-of-code-2020/part-13 content_type=Some("text/html; charset=utf-8")
[pid 154628] +++ exited with 0 +++
[pid 154627] +++ exited with 0 +++
[pid 154618] +++ exited with 0 +++
[pid 154614] +++ exited with 0 +++
[pid 154612] +++ exited with 0 +++
[pid 154619] +++ exited with 0 +++
[pid 154617] +++ exited with 0 +++
[pid 154613] +++ exited with 0 +++
[pid 154615] +++ exited with 0 +++
[pid 154623] +++ exited with 0 +++
[pid 154616] +++ exited with 0 +++
[pid 154624] +++ exited with 0 +++
[pid 154621] +++ exited with 0 +++
[pid 154622] +++ exited with 0 +++
[pid 154626] +++ exited with 0 +++
[pid 154620] +++ exited with 0 +++
[pid 154625] +++ exited with 0 +++
+++ exited with 0 +++shell
```

哇哦，一大堆 `connect` 。

所以程序首先尝试连接 [nscd](https://jameshfisher.com/2018/02/05/dont-use-nscd/) 因为显然我们依然生活在 90 年代：

```nil
[pid 154627] connect(9, {sa_family=AF_UNIX, sun_path="/var/run/nscd/socket"}, 110) = -1 ENOENT (No such file or directory)
```

。。。幸好我的系统没有它，所以它继续通过 `/etc/resolv.conf` 查询 DNS：

```nil
[pid 154628] connect(9, {sa_family=AF_INET, sin_port=htons(53), sin_addr=inet_addr("127.0.0.53")}, 16) = 0
```

然后最终获得一些 [Cloudflare 的 IP 地址](https://www.cloudflare.com/ips/)，如 `172.67.196.144` 和 `104.21.92.169` 。还有一些 IPv6 相关的，由于我禁用了 IPv6 所以并没有工作：

```nil
[pid 154627] connect(9, {sa_family=AF_INET6, sin6_port=htons(0), sin6_flowinfo=htonl(0), inet_pton(AF_INET6, "2606:4700:3034::6815:5ca9", &sin6_addr), sin6_scope_id=0}, 28) = -1 ENETUNREACH (Network is unreachable)
```

然后终于程序决定使用 IPv4 的地址 `104.21.92.169` 去构建请求，同时我们能看到这些都是非阻塞的（non-blocking）连接，因为 `connect` 返回 `-1` 而不是 `0` 表示「正在连接、正在连接、稍后回来检查」。

```nil
[pid 154625] connect(9, {sa_family=AF_INET, sin_port=htons(443), sin_addr=inet_addr("104.21.92.169")}, 16) = -1 EINPROGRESS (Operation now in progress)
[pid 154626] connect(10, {sa_family=AF_INET, sin_port=htons(443), sin_addr=inet_addr("104.21.92.169")}, 16) = -1 EINPROGRESS (Operation now in progress)
```

好了！所以忽略 [DNS](https://isitdns.com/) 的话我们看到了两个连接。

同时我们看到了一些线程。

这就是 Rust 异步的工作方式？我们只是用了一些线程？这也就是它能在「后台运行」的原因？

在我们回答这些问题前，让我们先调整我们的代码真正的去等待 future 完成，而不是随意的休眠 1 秒钟。

```rust
#[tokio::main]
async fn main() -> Result<(), Report> {
    setup()?;

    let client = Client::new();
    let leaked_client = Box::leak(Box::new(client));

    let fut1 = fetch_thing(leaked_client, URL_1);
    let fut2 = fetch_thing(leaked_client, URL_2);

    let handle1 = tokio::spawn(fut1);
    let handle2 = tokio::spawn(fut2);

    handle1.await.unwrap()?;
    handle2.await.unwrap()?;

    Ok(())
}
```

等等，我们这不又回到原点吗？等待第一个请求完成，然后才开始第二个请求。

当然不是！我们运行几次就可以看到：

```nil
$ RUST_LOG=info cargo run --quiet --release
Jul 25 19:11:07.934  INFO waytoodeep: Got a response! url=https://fasterthanli.me/articles/whats-in-the-box content_type=Some("text/html; charset=utf-8")
Jul 25 19:11:07.958  INFO waytoodeep: Got a response! url=https://fasterthanli.me/series/advent-of-code-2020/part-13 content_type=Some("text/html; charset=utf-8")
$ RUST_LOG=info cargo run --quiet --release
Jul 25 19:11:08.676  INFO waytoodeep: Got a response! url=https://fasterthanli.me/articles/whats-in-the-box content_type=Some("text/html; charset=utf-8")
Jul 25 19:11:08.680  INFO waytoodeep: Got a response! url=https://fasterthanli.me/series/advent-of-code-2020/part-13 content_type=Some("text/html; charset=utf-8")
$ RUST_LOG=info cargo run --quiet --release
Jul 25 19:11:09.325  INFO waytoodeep: Got a response! url=https://fasterthanli.me/articles/whats-in-the-box content_type=Some("text/html; charset=utf-8")
Jul 25 19:11:09.338  INFO waytoodeep: Got a response! url=https://fasterthanli.me/series/advent-of-code-2020/part-13 content_type=Some("text/html; charset=utf-8")
$ RUST_LOG=info cargo run --quiet --release
Jul 25 19:11:10.134  INFO waytoodeep: Got a response! url=https://fasterthanli.me/series/advent-of-code-2020/part-13 content_type=Some("text/html; charset=utf-8")
Jul 25 19:11:10.144  INFO waytoodeep: Got a response! url=https://fasterthanli.me/articles/whats-in-the-box content_type=Some("text/html; charset=utf-8")
```

。。。大部分情况下“whats-in-the-box”胜出了（它确实先开始），但是“advent-of-code-2020”也首先完成了几次。这也是我们希望看到的。

> 谬论：也就是说因为有线程请求被并行（parallel）的执行了。

不是的。但是不要相信我，让我们继续深入。


### 不是因为线程 {#不是因为线程}

让我们通过 GDB 运行我们的小程序，大部分原因是我还没有对 LLDB 形成肌肉记忆，我相信这是水到渠成的事。

```nil
$ cargo build --quiet && gdb --quiet --args ./target/debug/waytoodeep
Reading symbols from ./target/debug/waytoodeep...
warning: Missing auto-load script at offset 0 in section .debug_gdb_scripts
of file /home/amos/ftl/waytoodeep/target/debug/waytoodeep.
Use `info auto-load python-scripts [REGEXP]' to list them.
(gdb)
```

一切就绪！

在我们开始之前先设置一下断点。我说了断点？应该是捕捉点（catchpoint）。我不知道参与构造 HTTP/2 请求的所有函数名，但是我知道 `connect` 对应的系统调用（syscall），这也是我们需要打断点的地方，或者捕捉（catch）。

```nil
(gdb) catch syscall connect
Catchpoint 1 (syscall 'connect' [42])
```

现在我们开始！

```nil
$ Starting program: /home/amos/ftl/waytoodeep/target/debug/waytoodeep
[Thread debugging using libthread_db enabled]
Using host libthread_db library "/lib/x86_64-linux-gnu/libthread_db.so.1".
[New Thread 0x7ffff7c28700 (LWP 158945)]
[New Thread 0x7ffff7a27700 (LWP 158946)]
[New Thread 0x7fffef826700 (LWP 158947)]
[New Thread 0x7ffff7826700 (LWP 158948)]
[New Thread 0x7ffff7625700 (LWP 158949)]
[New Thread 0x7ffff7424700 (LWP 158950)]
[New Thread 0x7ffff7223700 (LWP 158951)]
[New Thread 0x7ffff701f700 (LWP 158952)]
[New Thread 0x7ffff6e1e700 (LWP 158953)]
[New Thread 0x7ffff6c1a700 (LWP 158954)]
[New Thread 0x7ffff6a16700 (LWP 158955)]
[New Thread 0x7ffff680f700 (LWP 158956)]
[New Thread 0x7ffff660e700 (LWP 158957)]
[New Thread 0x7ffff640a700 (LWP 158958)]
[New Thread 0x7ffff6206700 (LWP 158959)]
[New Thread 0x7ffff5f4b700 (LWP 158960)]
[New Thread 0x7ffff5d4a700 (LWP 158961)]
[Switching to Thread 0x7ffff5f4b700 (LWP 158960)]

Thread 17 "tokio-runtime-w" hit Catchpoint 1 (call to syscall connect), 0x00007ffff7d5033b in __libc_connect (fd=fd@entry=9, addr=..., addr@entry=...,
    len=len@entry=110) at ../sysdeps/unix/sysv/linux/connect.c:26
26      ../sysdeps/unix/sysv/linux/connect.c: No such file or directory.
(gdb)
```

不错不错，真快！我们停在了名为 `tokio-runtime-w` 的 `Thread 17` 中，因为我猜其他所有字母都被使用了。

> `w` 意味这 `worker` ，如果你不是第一天用 Unix 就会知道什么这么简写。

好的， `Thread 17` ，那么其他线程在做什么呢？

```nil
(gdb) info threads
  Id   Target Id                                            Frame
  1    Thread 0x7ffff7c2c6c0 (LWP 158941) "waytoodeep"      syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
  2    Thread 0x7ffff7c28700 (LWP 158945) "tokio-runtime-w" syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
  3    Thread 0x7ffff7a27700 (LWP 158946) "tokio-runtime-w" 0x00007ffff7d4f5ce in epoll_wait (epfd=3, events=0x555556338b60, maxevents=1024, timeout=-1)
    at ../sysdeps/unix/sysv/linux/epoll_wait.c:30
  4    Thread 0x7fffef826700 (LWP 158947) "tokio-runtime-w" syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
  5    Thread 0x7ffff7826700 (LWP 158948) "tokio-runtime-w" syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
  6    Thread 0x7ffff7625700 (LWP 158949) "tokio-runtime-w" syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
  7    Thread 0x7ffff7424700 (LWP 158950) "tokio-runtime-w" syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
  8    Thread 0x7ffff7223700 (LWP 158951) "tokio-runtime-w" syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
  9    Thread 0x7ffff701f700 (LWP 158952) "tokio-runtime-w" syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
  10   Thread 0x7ffff6e1e700 (LWP 158953) "tokio-runtime-w" syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
  11   Thread 0x7ffff6c1a700 (LWP 158954) "tokio-runtime-w" syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
  12   Thread 0x7ffff6a16700 (LWP 158955) "tokio-runtime-w" syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
  13   Thread 0x7ffff680f700 (LWP 158956) "tokio-runtime-w" syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
  14   Thread 0x7ffff660e700 (LWP 158957) "tokio-runtime-w" syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
  15   Thread 0x7ffff640a700 (LWP 158958) "tokio-runtime-w" syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
  16   Thread 0x7ffff6206700 (LWP 158959) "tokio-runtime-w" syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
 *17   Thread 0x7ffff5f4b700 (LWP 158960) "tokio-runtime-w" 0x00007ffff7d5033b in __libc_connect (fd=fd@entry=9, addr=..., addr@entry=..., len=len@entry=110)
    at ../sysdeps/unix/sysv/linux/connect.c:26
  18   Thread 0x7ffff5d4a700 (LWP 158961) "tokio-runtime-w" 0x00007ffff7d48a46 in __GI___mmap64 (offset=0, fd=-1, flags=16418, prot=0, len=134217728, addr=0x0)
    at ../sysdeps/unix/sysv/linux/mmap64.c:59
```

额。

我们可以获得更多的栈帧？

```nil
(gdb) thread apply all backtrace 2

Thread 18 (Thread 0x7ffff5d4a700 (LWP 158961)):
#0  0x00007ffff7d48a46 in __GI___mmap64 (offset=0, fd=-1, flags=16418, prot=0, len=134217728, addr=0x0) at ../sysdeps/unix/sysv/linux/mmap64.c:59
#1  __GI___mmap64 (addr=addr@entry=0x0, len=len@entry=134217728, prot=prot@entry=0, flags=flags@entry=16418, fd=fd@entry=-1, offset=offset@entry=0) at ../sysdeps/unix/sysv/linux/mmap64.c:47
(More stack frames follow...)

Thread 17 (Thread 0x7ffff5f4b700 (LWP 158960)):
#0  0x00007ffff7d5033b in __libc_connect (fd=fd@entry=9, addr=..., addr@entry=..., len=len@entry=110) at ../sysdeps/unix/sysv/linux/connect.c:26
#1  0x00007ffff7d8b713 in open_socket (type=type@entry=GETFDHST, key=key@entry=0x7ffff7de5ccb "hosts", keylen=keylen@entry=6) at nscd_helper.c:185
(More stack frames follow...)

Thread 16 (Thread 0x7ffff6206700 (LWP 158959)):
#0  syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
#1  0x0000555555b9f1d1 in parking_lot_core::thread_parker::imp::ThreadParker::futex_wait (self=0x7ffff6206498, ts=...) at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/parking_lot_core-0.8.3/src/thread_parker/linux.rs:112
(More stack frames follow...)

Thread 15 (Thread 0x7ffff640a700 (LWP 158958)):
#0  syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
#1  0x0000555555b9f1d1 in parking_lot_core::thread_parker::imp::ThreadParker::futex_wait (self=0x7ffff640a498, ts=...) at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/parking_lot_core-0.8.3/src/thread_parker/linux.rs:112
(More stack frames follow...)

Thread 14 (Thread 0x7ffff660e700 (LWP 158957)):
#0  syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
#1  0x0000555555b9f1d1 in parking_lot_core::thread_parker::imp::ThreadParker::futex_wait (self=0x7ffff660e498, ts=...) at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/parking_lot_core-0.8.3/src/thread_parker/linux.rs:112
(More stack frames follow...)

Thread 13 (Thread 0x7ffff680f700 (LWP 158956)):
#0  syscall () at ../sysdeps/unix/sysv/linux/x86_64/syscall.S:38
#1  0x0000555555b9f1d1 in parking_lot_core::thread_parker::imp::ThreadParker::futex_wait (self=0x7ffff680f498, ts=...) at /home/amos/.cargo/registry/src/github.com-1ecc6299db9ec823/parking_lot_core-0.8.3/src/thread_parker/linux.rs:112
(More stack frames follow...)
```

额。大部分都是挂起的。也就是空闲的。更准确的是它们在等待工作。

我们也可以通过 htop 查看这些所有线程，我知道我们已经看到了，但是我仅仅是觉得 htop 很棒。感谢 [Hisham](https://twitter.com/hisham%5Fhm)！
![](https://fasterthanli.me/content/articles/understanding-rust-futures-by-going-way-too-deep/assets/htop-colors.571c5effbff8a0b3.webp)
所以，我们注意到一些线程，同时也有一些 CPU 核心。可能是一个 CPU 核心一个线程？
![](https://fasterthanli.me/content/articles/understanding-rust-futures-by-going-way-too-deep/assets/worker-threads.64dbe39e33ccfc4f.webp)
是的。然后还有一些阻塞的线程，正如我们从上面 `strace` 输出看到的那样， 它会进行一些阻塞的 `connect` 调用去查询 DNS（实际是 glibc 在执行），
所以它通过运行在工作线程之外避免阻塞其他任务。

> 所以多个线程，这就是为什么一次可以运行多个请求的原因？

实际上文档上表明这是一个单线程的执行器，我也不能确定，所以让我们试一下：

```rust
//                           👇
#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Report> {
        // (same as before)
}
```

```nil
$ RUST_LOG=info cargo run --quiet --release
Jul 25 19:50:15.977  INFO waytoodeep: Got a response! url=https://fasterthanli.me/articles/whats-in-the-box content_type=Some("text/html; charset=utf-8")
Jul 25 19:50:15.994  INFO waytoodeep: Got a response! url=https://fasterthanli.me/series/advent-of-code-2020/part-13 content_type=Some("text/html; charset=utf-8")
```

两个响应间隔 `17ms` ，这个时间不够构造一个完整的请求，所以请求并行（parallel）的执行了。如果你依然坚持它内部使用了线程，让我们进一步确认我们只有一个线程：
![](https://fasterthanli.me/content/articles/understanding-rust-futures-by-going-way-too-deep/assets/current-thread.cd7b619ed644899b.webp)
确实有多个线程，但是这些都是阻塞线程。仅仅是 DNS 查询。可以通过 htop 看到已经没有无数（15）的工作线程了：
![](https://fasterthanli.me/content/articles/understanding-rust-futures-by-going-way-too-deep/assets/htop-current-thread.fe28174abc5d15fa.webp)
（顺便说一下 15 个工作线程的原因，这是因为我预留了一个 CPU 核心没有分配给虚拟机，这样即使虚拟机全速运行也不会导致宿主机停止响应）。

如果我们将 DNS 查询排除在外，我们就可以看到实际上仅仅使用了一个线程，我们将继续下去，以防你依然存疑！


### 插曲：让我们避免泄漏内存 {#插曲-让我们避免泄漏内存}

但是在那之前：正在泄漏 reqwest 的 `Client` 让我很不爽。

为了避免，我们可以创建一个原子引用计数（atomically-reference-counted），这样它就可以随着任务运行而存活。

修改起来非常简单：

```rust
//             👇 Atomically Reference Counted = Arc
use std::sync::Arc;

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Report> {
    setup()?;

    //           👇 there we go
    let client = Arc::new(Client::new());

    //                              👇
    let fut1 = fetch_thing(client.clone(), URL_1);
    // (cloning it only increases the reference count)
    let fut2 = fetch_thing(client.clone(), URL_2);

    let handle1 = tokio::spawn(fut1);
    let handle2 = tokio::spawn(fut2);

    handle1.await.unwrap()?;
    handle2.await.unwrap()?;

    Ok(())
}

#[allow(clippy::manual_async_fn)]
fn fetch_thing(
    //       👇 now taking this, we have shared ownership of it
    client: Arc<Client>,
    url: &'static str,
) -> impl Future<Output = Result<(), Report>> + 'static {
    async move {
        // luckily this  👇 only requires `&self`
        let res = client.get(url).send().await?.error_for_status()?;
        info!(%url, content_type = ?res.headers().get("content-type"), "Got a response!");
        Ok(())
    }
}
```

好了，现在我感觉好多了。我们的程序不再泄漏一些字节即使它永远不会运行超过几秒钟。一切都还好。

让我们看一下 `reqwest` 的 `Client` 定义:

```rust
#[derive(Clone)]
pub struct Client {
    inner: Arc<ClientRef>,
}
```

它已经是引用计数的了，所以我们可以直接接受一个 `Client`:

```rust
#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Report> {
    setup()?;

    //             👇
    let client = Client::new();

    //                              👇
    let fut1 = fetch_thing(client.clone(), URL_1);
    // no need to clone a second time
    let fut2 = fetch_thing(client, URL_2);

    let handle1 = tokio::spawn(fut1);
    let handle2 = tokio::spawn(fut2);

    handle1.await.unwrap()?;
    handle2.await.unwrap()?;

    Ok(())
}

#[allow(clippy::manual_async_fn)]
fn fetch_thing(
    //        👇
    client: Client,
    url: &'static str,
) -> impl Future<Output = Result<(), Report>> + 'static {
    async move {
        let res = client.get(url).send().await?.error_for_status()?;
        info!(%url, content_type = ?res.headers().get("content-type"), "Got a response!");
        Ok(())
    }
}
```

好了。

对了，仅供参考，更简单的 `async fn` 也可以工作了：

```rust
async fn fetch_thing(client: Client, url: &str) -> Result<(), Report> {
    let res = client.get(url).send().await?.error_for_status()?;
    info!(%url, content_type = ?res.headers().get("content-type"), "Got a response!");
    Ok(())
}
```

我们甚至不需要要求 `url` 的借用生命周期是 `'static` 。如果 `url` 是 `'static` 的则返回的 Future 也是，反之亦然。

作为例子，下面代码无法通过编译：

```rust
#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Report> {
    setup()?;

    let client = Client::new();

    // this is a `String`, owned by main
    let url1 = String::from(URL_1);

    // we're borrowing from main           👇
    let fut1 = fetch_thing(client.clone(), &url1);
    let fut2 = fetch_thing(client, URL_2);

    let handle1 = tokio::spawn(fut1);
    let handle2 = tokio::spawn(fut2);

    handle1.await.unwrap()?;
    handle2.await.unwrap()?;

    Ok(())
}
```

```nil
$ cargo check
    Checking waytoodeep v0.1.0 (/home/amos/ftl/waytoodeep)

error[E0597]: `url1` does not live long enough
  --> src/main.rs:18:44
   |
18 |     let fut1 = fetch_thing(client.clone(), &url1);
   |                ----------------------------^^^^^-
   |                |                           |
   |                |                           borrowed value does not live long enough
   |                argument requires that `url1` is borrowed for `'static`
...
28 | }
   | - `url1` dropped here while still borrowed
```

> 你面对的考验就是：修改了一些代码，然后突然间整个 `Future` 不再实现 `Send` ，但是你需要它实现 `Send` 。参考[Getting in and out of trouble with Rust futures](https://fasterthanli.me/articles/getting-in-and-out-of-trouble-with-rust-futures)。

在我们进一步深入之前，我们还想提一下，除了通过 `tokio::spawn` 可以同时运行两个 future 并且立即等待两个 future 完成，还是使用 `FuturesUnordered` 实现相同目的。

```nil
$ cargo add futures@0.3.16
    Updating 'https://github.com/rust-lang/crates.io-index' index
      Adding futures v0.3.16 to dependencies
```

```rust
use futures::{stream::FuturesUnordered, StreamExt};


#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Report> {
    setup()?;

    let client = Client::new();

    let mut group = vec![
        fetch_thing(client.clone(), URL_1),
        fetch_thing(client, URL_2),
    ]
    .into_iter()
    .collect::<FuturesUnordered<_>>();

    while let Some(item) = group.next().await {
        // propagate errors
        item?;
    }

    Ok(())
}
```

通过这个解决半啃啊，我们可以 await 任意数量的 future 对象，同时也是并发的被轮询（polled）。

```nil
$ RUST_LOG=info cargo run --quiet --release
Jul 25 20:12:37.208  INFO waytoodeep: Got a response! url=https://fasterthanli.me/articles/whats-in-the-box content_type=Some("text/html; charset=utf-8")
Jul 25 20:12:37.227  INFO waytoodeep: Got a response! url=https://fasterthanli.me/series/advent-of-code-2020/part-13 content_type=Some("text/html; charset=utf-8")
```

仅仅。。。19 毫秒的间隔 -- 可以确定是并发的。


### 彻底摆脱 DNS {#彻底摆脱-dns}

现在让我们暂时忘掉 `reqwest` 。

HTTP [并不难](https://fasterthanli.me/articles/aiming-for-correctness-with-types) ，我们可以自己构建。只要 TCP 就行：

```rust
use std::net::SocketAddr;
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::TcpStream,
};

async fn fetch_thing(name: &str) -> Result<(), Report> {
    // look mom, no DNS!
    let addr: SocketAddr = ([1, 1, 1, 1], 80).into();
    let mut socket = TcpStream::connect(addr).await?;

    // we're writing straight to the socket, there's no buffering
    // so no need to flush
    socket.write_all(b"GET / HTTP/1.1\r\n").await?;
    socket.write_all(b"Host: 1.1.1.1\r\n").await?;
    socket.write_all(b"User-Agent: cool-bear\r\n").await?;
    socket.write_all(b"Connection: close\r\n").await?;
    socket.write_all(b"\r\n").await?;

    let mut response = String::with_capacity(256);
    socket.read_to_string(&mut response).await?;

    let status = response.lines().next().unwrap_or_default();
    info!(%status, , "Got response!");

    // dropping the socket will close the connection

    Ok(())
}
```

可以正常运行：

```nil
$ RUST_LOG=info cargo run --quiet --release
Jul 25 20:24:05.158  INFO waytoodeep: Got response! status=HTTP/1.1 301 Moved Permanently name=second
Jul 25 20:24:05.159  INFO waytoodeep: Got response! status=HTTP/1.1 301 Moved Permanently name=first
```

（看，第二个赢得了胜利）。

同时再也没有 DNS 查询了。

当然 `http://1.1.1.1` 将我们重定向到 HTTPS 的页面，技术上实现 TLS 并不困难，但是我们的篇幅已经很长了，所以。。。

```nil
$ cargo add tokio-rustls@0.22.0
    Updating 'https://github.com/rust-lang/crates.io-index' index
      Adding tokio-rustls v0.22.0 to dependencies
$ cargo add webpki@0.21.4
    Updating 'https://github.com/rust-lang/crates.io-index' index
      Adding webpki v0.21.4 to dependencies
$ cargo add webpki-roots@0.21.1
    Updating 'https://github.com/rust-lang/crates.io-index' index
      Adding webpki-roots v0.21.1 to dependencies
```

然后。。。

```nil
$ cargo rm reqwest
    Removing reqwest from dependencies
```

```rust
use std::sync::Arc;
use webpki::DNSNameRef;
use tokio_rustls::{rustls::ClientConfig, TlsConnector};

async fn fetch_thing(name: &str) -> Result<(), Report> {
    // look out it's port 443 now
    let addr: SocketAddr = ([1, 1, 1, 1], 443).into();
    let socket = TcpStream::connect(addr).await?;

    // establish a TLS session...
    let connector: TlsConnector = {
        let mut config = ClientConfig::new();
        config
            .root_store
            .add_server_trust_anchors(&webpki_roots::TLS_SERVER_ROOTS);
        Arc::new(config).into()
    };
    // we have to use the proper DNS name now      👇
    let dnsname = DNSNameRef::try_from_ascii_str("one.one.one.one")?;
    let mut socket = connector.connect(dnsname, socket).await?;

    // we're writing straight to the socket, there's no buffering
    // so no need to flush
    socket.write_all(b"GET / HTTP/1.1\r\n").await?;
    //                        👇
    socket.write_all(b"Host: one.one.one.one\r\n").await?;
    socket.write_all(b"User-Agent: cool-bear\r\n").await?;
    socket.write_all(b"Connection: close\r\n").await?;
    socket.write_all(b"\r\n").await?;

    let mut response = String::with_capacity(256);
    socket.read_to_string(&mut response).await?;

    let status = response.lines().next().unwrap_or_default();
    info!(%status, , "Got response!");

    // dropping the socket will close the connection

    Ok(())
}
```

```nil
$ RUST_LOG=info cargo run --quiet --release
Jul 25 20:31:32.627  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=second
Jul 25 20:31:32.658  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=first
```

好了，现在返回状态码 200！

我们的目标是了解 Rust 的 future，我们已经获得了不错的进展。

但是让我们考虑以下场景：我们想并发的执行两个请求，一旦其中一个失败，另外一个也应该立即请求失败，或者两个一起成功。


### tokio 的 try\_join 宏 {#tokio-的-try-join-宏}

实际上，又一个宏可以做这个！

```rust
#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Report> {
    setup()?;

    let res = tokio::try_join!(fetch_thing("first"), fetch_thing("second"),)?;
    info!(?res, "All done!");

    Ok(())
}
```

这就是我们想要的！

```nil
$ RUST_LOG=info cargo run --quiet --release
Jul 25 20:44:52.150  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=first
Jul 25 20:44:52.165  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=second
Jul 25 20:44:52.165  INFO waytoodeep: All done! res=((), ())
```

再次快速检查以下：响应间隔在 15ms -- 也就是确定是并发的发送。

`try_join!` 帮我们进行了 `await` ，同时帮我们处理了结果。如果一切正常，我们得到所有 future 对象的结果：内容为 `Ok` 的空元组（有序的）。

所以我们可以取到我们 future 返回的对象：

```rust
//                                          👇
async fn fetch_thing(name: &str) -> Result<&str, Report> {
    // (omitted)

    //  👇
    Ok(name)
}
```

为了方便我们自己，它们按照顺序返回，无论哪个先被执行：

```nil
$ RUST_LOG=info cargo run --quiet --release
Jul 25 20:47:56.967  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=second
Jul 25 20:47:56.967  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=first
Jul 25 20:47:56.967  INFO waytoodeep: All done! res=("first", "second")
$ RUST_LOG=info cargo run --quiet --release
Jul 25 20:47:57.933  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=first
Jul 25 20:47:57.935  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=second
Jul 25 20:47:57.935  INFO waytoodeep: All done! res=("first", "second")
$ RUST_LOG=info cargo run --quiet --release
Jul 25 20:47:58.942  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=second
Jul 25 20:47:58.946  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=first
Jul 25 20:47:58.946  INFO waytoodeep: All done! res=("first", "second")
```

好了，现在我们没有 DNS 查询，我们就可以消除“同时”请求是由于多线程实现的。

因为，如果我们在 `strace` 下运行程序，并通过 `-f` 请求跟踪子线程（ BTW `f` 意思是跟踪（ `follow` ）子线程）：

```nil
$ cargo build --quiet --release && strace -f -e 'connect' ./target/release/waytoodeep
connect(9, {sa_family=AF_INET, sin_port=htons(443), sin_addr=inet_addr("1.1.1.1")}, 16) = -1 EINPROGRESS (Operation now in progress)
connect(10, {sa_family=AF_INET, sin_port=htons(443), sin_addr=inet_addr("1.1.1.1")}, 16) = -1 EINPROGRESS (Operation now in progress)
Jul 25 20:51:54.004  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=first
Jul 25 20:51:54.013  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=second
Jul 25 20:51:54.015  INFO waytoodeep: All done! res=("first", "second")
+++ exited with 0 +++
```

。。。现在我们看到了预期的两次 `connect` 调用，但是没有任何子线程。而且在这个运行中，响应之间的间隔时间是 9 毫秒！少于我直接 ping 1.1.1.1:

```nil
$ ping -c 1 1.1.1.1
PING 1.1.1.1 (1.1.1.1) 56(84) bytes of data.
64 bytes from 1.1.1.1: icmp_seq=1 ttl=57 time=13.7 ms

--- 1.1.1.1 ping statistics ---
1 packets transmitted, 1 received, 0% packet loss, time 0ms
rtt min/avg/max/mdev = 13.748/13.748/13.748/0.000 ms
```

这是因为执行器通过 Event Loop 构建非阻塞的系统调用，然后订阅 Event Loop 管理的资源相关的事件，
然后就可以知道一个 socket 什么时间可以进行读写。

所以，future 对象只是一些状态，接下来就可以进行 await，那么在哪订阅的事件呢？

让我们尝试创建一个我们自己的 `try_join` -- 一个函数，并且只接受两个 future。然后我们就能看到发生了什么。

我们已经实现了自己的 future，实现一个 `try_join` 函数会有多麻烦？


### 事实证明很麻烦 {#事实证明很麻烦}

我们先从简单的开始！我们想实现一个函数接受两个 future 对象然后返回一个 future 对象。

```rust
// in `src/main.rs`
mod tj;
```

```rust
// in `src/tj.rs`

use std::future::Future;

pub fn try_join<A, B>(a: A, b: B) -> impl Future<Output = ()>
where
    A: Future,
    B: Future,
{
    todo!("implement me!");
}
```

额。函数不应该只返回一个空元组，它需要返回一个包含成功结果的元组。或者遇到的第一个错误。

所以我们需要添加一些更多的范型参数：一个错误类型（我们假设两个 future 对象返回同样的错误类型），另一个是 future 对象返回的 `Ok` 的类型。

```rust
pub fn try_join<A, B, AR, BR, E>(a: A, b: B) -> impl Future<Output = Result<(AR, BR), E>>
where
    A: Future<Output = Result<AR, E>>,
    B: Future<Output = Result<BR, E>>,
{
    todo!("implement me!");
}
```

好了！这非常绕口，但是我相信我们已经实现了需求。

需要注意的是我们使用了 `impl Trait` 语法，让我们不用暴露我们自己的 `try join future` 。这不重要，但是可以让我们用更少的 `pub` 关键字，同时我们的手指已经码累了。非常累。

所以，让我们来创建这个类型！

类型需要持续 `A` 和 `B` ，并注意 `AR` 、 `BR` 和 `E` 类型。所以，希望您对这些范型参数沙拉有个好胃口。

```rust
struct TryJoin<A, B, AR, BR, E>
where
    A: Future<Output = Result<AR, E>>,
    B: Future<Output = Result<BR, E>>,
{
    a: A,
    b: B,
}
```

然后可以在我们的 `try_join` 函数中返回它：

```rust
pub fn try_join<A, B, AR, BR, E>(a: A, b: B) -> impl Future<Output = Result<(AR, BR), E>>
where
    A: Future<Output = Result<AR, E>>,
    B: Future<Output = Result<BR, E>>,
{
    // so simple!
    TryJoin { a, b }
}
```

我认为这很好的说明一个事实：创建 future 对象仅仅是构建状态。不需要任何额外的工作。

当然，这个不会通过编译，因为 `TryJoin` 还没有实现 `Future` 。

但是不要担心！ `rust-analyzer` 可以帮助我们生成缺失的部分：

```rust
use std::{
    future::Future,
    pin::Pin,
    task::{Context, Poll},
};

impl<A, B, AR, BR, E> Future for TryJoin<A, B, AR, BR, E>
where
    A: Future<Output = Result<AR, E>>,
    B: Future<Output = Result<BR, E>>,
{
    type Output = Result<(AR, BR), E>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        todo!()
    }
}
```

如果我们真正的实现了，我们将按照下面方式使用：

```rust
#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Report> {
    setup()?;

    let res = tj::try_join(fetch_thing("first"), fetch_thing("second")).await?;
    info!(?res, "All done!");

    Ok(())
}
```

当然，现在只是会崩溃：

```nil
$ RUST_LOG=info cargo run --quiet --release

The application panicked (crashed).
Message:  not yet implemented
Location: src/tj.rs:32

Backtrace omitted.
Run with RUST_BACKTRACE=1 environment variable to display it.
Run with RUST_BACKTRACE=full to include source snippets.
```

所以，我猜我们需要实现它！

好吧，让我们先尝试至少轮询（polling）一个 future 对象。

```rust
fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
    let a = self.a.poll(cx);

    todo!()
}
```

```nil
$ RUST_LOG=info cargo run --quiet --release
error[E0599]: no method named `poll` found for type parameter `A` in the current scope
   --> src/tj.rs:32:24
    |
32  |         let a = self.a.poll(cx);
    |                        ^^^^ method not found in `A`
    |
   ::: /home/amos/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/future/future.rs:100:8
    |
100 |     fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output>;
    |        ---- the method is available for `Pin<&mut A>` here
    |
help: consider wrapping the receiver expression with the appropriate type
    |
32  |         let a = Pin::new(&mut self.a).poll(cx);
    |                 ^^^^^^^^^^^^^       ^
```

额！一个好的开始，好的开始。

我已经在这里[详细的解释了](https://fasterthanli.me/articles/pin-and-suffering) Pin，所以这里我们就简单的介绍一下。

方法通常通过如下方式定义接收者（receiver）：

```rust
struct MyType {
    fn do_thing(&self) {
        println!("my value is {}", self.value)
    }
}
```

也就是下面的简写：

```rust
struct MyType {
    fn do_thing(self: &Self) {
        println!("my value is {}", self.value)
    }
}
```

因为我们在 `impl MyType` 代码块中 `Self` 就是 `MyType` 。

很清晰吧？好了，还可以定义其他很多类型作为接收者， `Pin<&mut Self>` 就是其中之一：

```rust
struct MyType {
    fn do_thing(self: Pin<&mut Self>) {
        // good luck!1
    }
}
```

那么 `MyType` 必须是固定的（pinned）是什么意思呢？比如，它保证不进行转移（move）。除非它实现了 `Unpin` ，
然后它就可以是非固定的（unpinned），可移动，然后被再一次固定。

对于剩下的文章，我们不会假设我们的 future `A` 和 `B` 都是 `Unpin` ，也就是说我们自己不会移动（move）它们（只销毁（drop）它们）。

你可以说我们不需要 `A` 和 `B` 是 `Unpin` 的，因为我们没有添加指定的 where clause 来标记需要它们是 `Unpin` 。
因为如果我们需要，我们就要像下面这样添加额外的 `trait bound` ：

```rust
struct TryJoin<A, B, AR, BR, E>
where
    //                                    👇
    A: Future<Output = Result<AR, E>> + Unpin,
    B: Future<Output = Result<BR, E>> + Unpin,
{}
```

但是我们没有，所以我们不能假设 `A` 或 `B` 是 `Unpin` 的。

所以！我们现在真的只是面临固定（pin）保护的问题。

我们现在只持有一个 `Pin<&mut TryJoin<A, B, ...>>` 但是我希望持续一个 `Pin<&mut A>` （因为这就是我们因为需要轮询 `A` ）。

另外一个解决方法，我倾向于通过一些类似 [pin-project](https://lib.rs/crates/pin-project) 包，或者类似 [pin-project-lite](https://lib.rs/crates/pin-project-lite)，但是在我们前进的方向直接使用 `pin-project` 真的很尴尬，
所以我们这里使用 `unsafe` 作为替代：

```rust
fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
    let a = unsafe { self.map_unchecked_mut(|this| &mut this.a) };
    let a = a.poll(cx);

    todo!()
}
```

可以通过编译。但是我们在使用 `unsafe` ，也就意味着编译器正式停止 ~~照顾~~ 检查我们的代码。
我们自己必须强制执行一些不变量（invariants），并且非常非常小心让，同时让其他人审查（review）我们的工作，
但是依然可能会出错，因为他们也会休息。

现在，非常棒的是我们可以轮询 `a` 。它如果完成会返回 `Poll::Ready(Result<AR, E>)` ，
否则就是等会会完成则返回 `Poll::Pending` 。

我们可以观察到：

```rust
fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
    let a = unsafe { self.map_unchecked_mut(|this| &mut this.a) };
    match a.poll(cx) {
        Poll::Pending => {
            info!("A is pending...");
            return Poll::Pending;
        }
        Poll::Ready(res) => match res {
            Ok(_) => info!("A is ready!"),
            Err(e) => return Poll::Ready(Err(e)),
        },
    }

    todo!()
}
```

我们通过打印日志“A is pending”知道准备完成。这可能需要几个回合：毕竟，我们正在做一些重要的事情。
我们建立一个 TCP 连接，接着在上面进行 TLS 会话，接着是一些分开的写，最后读到 EOF（end of file）。

当然，如果我们运行它的话：

```nil
aytoodeep::tj: A is pending...
Jul 25 22:54:14.495  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.495  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.495  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.495  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.495  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.495  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.495  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.513  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.513  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.513  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.513  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.513  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.514  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.522  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.522  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.522  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.522  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.522  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.523  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.523  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.530  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.530  INFO waytoodeep::tj: A is pending...
Jul 25 22:54:14.530  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=first
Jul 25 22:54:14.530  INFO waytoodeep::tj: A is ready!
The application panicked (crashed).
Message:  not yet implemented
Location: src/tj.rs:46

Backtrace omitted.
Run with RUST_BACKTRACE=1 environment variable to display it.
Run with RUST_BACKTRACE=full to include source snippets.
```

我们可以看到它确实花费了几个回合。

注意如果 `A` 返回错误我们的代码也会返回 `Poll:Ready` ，因为我们想收集 A 和 B 的结果。

所以我们对 B 做相同的事情：

```rust
fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
    let a = unsafe { self.map_unchecked_mut(|this| &mut this.a) };
    match a.poll(cx) {
        Poll::Pending => {
            info!("A is pending...");
            return Poll::Pending;
        }
        Poll::Ready(res) => match res {
            Ok(_) => info!("A is ready!"),
            Err(e) => return Poll::Ready(Err(e)),
        },
    }

    let b = unsafe { self.map_unchecked_mut(|this| &mut this.a) };
    match b.poll(cx) {
        Poll::Pending => {
            info!("B is pending...");
            return Poll::Pending;
        }
        Poll::Ready(res) => match res {
            Ok(_) => info!("B is ready!"),
            Err(e) => return Poll::Ready(Err(e)),
        },
    }

    todo!()
}
```

然后。。whoops：

```nil
RUST_LOG=info cargo run --quiet --release
error[E0382]: use of moved value: `self`
   --> src/tj.rs:46:26
    |
33  |     fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
    |             ---- move occurs because `self` has type `Pin<&mut TryJoin<A, B, AR, BR, E>>`, which does not implement the `Copy` trait
34  |         let a = unsafe { self.map_unchecked_mut(|this| &mut this.a) };
    |                               ------------------------------------- `self` moved due to this method call
...
46  |         let b = unsafe { self.map_unchecked_mut(|this| &mut this.a) };
    |                          ^^^^ value used here after move
    |
note: this function takes ownership of the receiver `self`, which moves `self`
   --> /home/amos/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/pin.rs:776:43
    |
776 |     pub unsafe fn map_unchecked_mut<U, F>(self, func: F) -> Pin<&'a mut U>
    |                                           ^^^^
```

是的。 `map_unchecked_mut` 占有了 `self` 。

不用担心，我们可以使用 `.as_mut()` ：

```rust
//       👇
fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
    //                      👇
    let a = unsafe { self.as_mut().map_unchecked_mut(|this| &mut this.a) };
    match a.poll(cx) {
        Poll::Pending => {
            info!("A is pending...");
            return Poll::Pending;
        }
        Poll::Ready(res) => match res {
            Ok(_) => info!("A is ready!"),
            Err(e) => return Poll::Ready(Err(e)),
        },
    }

    //                      👇
    let b = unsafe { self.as_mut().map_unchecked_mut(|this| &mut this.a) };
    match b.poll(cx) {
        Poll::Pending => {
            info!("B is pending...");
            return Poll::Pending;
        }
        Poll::Ready(res) => match res {
            Ok(_) => info!("B is ready!"),
            Err(e) => return Poll::Ready(Err(e)),
        },
    }

    todo!()
}
```

但是依然无法通过编译：

```nil
$ RUST_LOG=info cargo run --quiet --release
(cut)
Jul 25 22:57:07.913  INFO waytoodeep::tj: A is pending...
Jul 25 22:57:07.913  INFO waytoodeep::tj: A is pending...
Jul 25 22:57:07.913  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=first
Jul 25 22:57:07.913  INFO waytoodeep::tj: A is ready!
The application panicked (crashed).
Message:  `async fn` resumed after completion
Location: src/main.rs:24

Backtrace omitted.
Run with RUST_BACKTRACE=1 environment variable to display it.
Run with RUST_BACKTRACE=full to include source snippets.
```

可以看到，一旦 `Future` 返回 `Poll::Ready` 我们就不能再次轮询它了。我们为什么会这样？因为 `Future` 已经返回了结果。如果结果是非 `Copy` 的，它可能只能返回一次。

所以，我们需要 1）跟踪 `A` 是否完成，然后 2）在某个地方存储它的返回结果。

我们只需要在我们的结构体中添加一些字段：

```rust
struct TryJoin<A, B, AR, BR, E>
where
    A: Future<Output = Result<AR, E>>,
    B: Future<Output = Result<BR, E>>,
{
    a: A,
    b: B,
    // 👇
    a_res: Option<AR>,
    b_res: Option<BR>,
}
```

不要忘记初始化它们：

```rust
pub fn try_join<A, B, AR, BR, E>(a: A, b: B) -> impl Future<Output = Result<(AR, BR), E>>
where
    A: Future<Output = Result<AR, E>>,
    B: Future<Output = Result<BR, E>>,
{
    TryJoin {
        a,
        b,
        // 👇
        a_res: None,
        b_res: None,
    }
}
```

现在计划是：

-   如果 `a_res` 是 `Some` ，然后我们就不需要轮询 `a` ,因为它已经完成了
-   同样的逻辑处理 `b_res` 和 `b`

让我们实现它。同时，因为我们已经在使用了 `unsafe` ，所以我们已经负责维护不变量（invariants），
所以我决定同时固定 `a` 和 `b` ，如下：

```rust
fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
    let this = unsafe { self.get_unchecked_mut() };
    let (a, b) = unsafe {
        (
            Pin::new_unchecked(&mut this.a),
            Pin::new_unchecked(&mut this.b),
        )
    };

    if this.a_res.is_none() {
        match a.poll(cx) {
            Poll::Pending => return Poll::Pending,
            Poll::Ready(res) => match res {
                Ok(x) => this.a_res = Some(x),
                Err(e) => return Poll::Ready(Err(e)),
            },
        }
    }

    if this.b_res.is_none() {
        match b.poll(cx) {
            Poll::Pending => return Poll::Pending,
            Poll::Ready(res) => match res {
                Ok(x) => this.b_res = Some(x),
                Err(e) => return Poll::Ready(Err(e)),
            },
        }
    }

    todo!()
}
```

好了，这个应该能让 `a` 和 `b` 有机会在我们崩溃之前完成：

```nil
$ RUST_LOG=info cargo run --quiet --release
Jul 25 23:11:03.851  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=first
Jul 25 23:11:04.380  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=second
The application panicked (crashed).
Message:  not yet implemented
Location: src/tj.rs:69

Backtrace omitted.
Run with RUST_BACKTRACE=1 environment variable to display it.
Run with RUST_BACKTRACE=full to include source snippets.
```

很好！现在我们需要做的就是解出两个结果并返回它们：

```rust
// instead of the `todo!()`:

if let (Some(_), Some(_)) = (&this.a_res, &this.b_res) {
    let a = this.a_res.take().unwrap();
    let b = this.b_res.take().unwrap();
    Poll::Ready(Ok((a, b)))
} else {
    Poll::Pending
}
```

可以工作：

```nil
$ RUST_LOG=info cargo run --quiet --release
Jul 25 23:13:32.497  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=first
Jul 25 23:13:32.829  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=second
Jul 25 23:13:32.829  INFO waytoodeep: All done! res=("first", "second")
```

。。。但是这不是 `try_join` 的实现。我们正在做的和这个完全一样：

```rust
// (pseudo-code, buncha things are missing)
async fn try_join(a: A, b: B) {
    let a = self.a.await?;
    let b = self.b.await?;
    Ok((a, b))
}
```

是顺序的执行的。请记住，仅仅是因为 tokio 的执行器可能用了一堆线程并意味着同时运行是自动的。
前面我们不得不使用 `tokio::spwan` 或 `UnorderedFutures` 或 `try_join!` 让其同时运行。

所以让我们重新看一下。。。当我们轮询 `a` 的是后发生了什么？

```rust
if this.a_res.is_none() {
    match a.poll(cx) {
        Poll::Pending => return Poll::Pending,
        Poll::Ready(res) => match res {
            Ok(x) => this.a_res = Some(x),
            Err(e) => return Poll::Ready(Err(e)),
        },
    }
}
```

额，当轮询 `a` 的时候返回 `Poll::Pending` 时我们也会返回 `Poll::Pending`  。所以这就是问题。
如果 `a` 正在等待（pending）我们不应该返回。因为如果这时候 `b` 已经准备好或者有错误呢？

或者如果，我们像这样调用 `try_join` 呢：

```rust
info!("Joining...");
let res = tj::try_join(
    async move {
        sleep(Duration::from_millis(2000)).await;
        Ok(())
    },
    async move {
        sleep(Duration::from_millis(10)).await;
        Err::<(), Report>(color_eyre::eyre::eyre!("uh oh"))
    },
)
.await;
```

。。。然后 `a` 花费了 2 秒钟才准备好，同时 `b` 会在 10毫秒之后返回一个错误，如果我们轮询它！

嗐，我们并没有：

```nil
$ RUST_LOG=info cargo run --quiet --release
Jul 25 23:19:26.972  INFO waytoodeep: Joining...
Jul 25 23:19:28.990  INFO waytoodeep: All done! res=Err(
   0: uh oh

Location:
   src/main.rs:28
(cut)
```

（注意时间戳）

重点是 `try_join` 会提前失败：一旦 Future 返回 `Result::Err~` 。

所以我们必须同时轮询 `a` 和 `b` 。好吧。。。不是严格意义的同时。我们必须每次我们的 `TryJoin` future 对象被轮询时并发（concurrently）的轮询它们，
直到它们返回结果。

有一个简单解决办法 -- 在任一 future 对象返回 `Poll::Pending` 时不返回 `Poll::Pending` 。

同时，我厌倦了输入 `Poll::Ready` 并且 `Poll<T>` 实现了 `From<T>` ,所以我们可以使用 `.into()` 了：

```rust
fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
    let this = unsafe { self.get_unchecked_mut() };
    let (a, b) = unsafe {
        (
            Pin::new_unchecked(&mut this.a),
            Pin::new_unchecked(&mut this.b),
        )
    };

    if this.a_res.is_none() {
        if let Poll::Ready(res) = a.poll(cx) {
            match res {
                Ok(x) => this.a_res = Some(x),
                Err(e) => return Err(e).into(),
            }
        }
    }

    if this.b_res.is_none() {
        if let Poll::Ready(res) = b.poll(cx) {
            match res {
                Ok(x) => this.b_res = Some(x),
                Err(e) => return Err(e).into(),
            }
        }
    }

    if let (Some(_), Some(_)) = (&this.a_res, &this.b_res) {
        let a = this.a_res.take().unwrap();
        let b = this.b_res.take().unwrap();
        Ok((a, b)).into()
    } else {
        Poll::Pending
    }
}
```

让我们再次运行

```nil
$ RUST_LOG=info cargo run --quiet --release
Jul 25 23:22:40.238  INFO waytoodeep: Joining...
Jul 25 23:22:40.253  INFO waytoodeep: All done! res=Err(
   0: uh oh

Location:
   src/main.rs:28

(cut)
```

。。。可以了！我是说它按照预期的失败了。预期的失败就是成功。

然后我们将这个方法应用到调用 `try_join` :

```rust
#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Report> {
    setup()?;

    info!("Joining...");
    let res = tj::try_join(fetch_thing("first"), fetch_thing("second")).await?;
    info!(?res, "All done!");

    Ok(())
}
```

我们可以看到竞争又回来了：有时 `first` 先完成，有时则不是：

```nil
$ RUST_LOG=info cargo run --quiet --release
Jul 25 23:25:25.925  INFO waytoodeep: Joining...
Jul 25 23:25:26.224  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=first
Jul 25 23:25:26.236  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=second
Jul 25 23:25:26.236  INFO waytoodeep: All done! res=("first", "second")
$ RUST_LOG=info cargo run --quiet --release
Jul 25 23:25:26.937  INFO waytoodeep: Joining...
Jul 25 23:25:27.237  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=first
Jul 25 23:25:27.242  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=second
Jul 25 23:25:27.242  INFO waytoodeep: All done! res=("first", "second")
$ RUST_LOG=info cargo run --quiet --release
Jul 25 23:25:27.865  INFO waytoodeep: Joining...
Jul 25 23:25:28.164  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=second
Jul 25 23:25:28.818  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=first
Jul 25 23:25:28.818  INFO waytoodeep: All done! res=("first", "second")
$ RUST_LOG=info cargo run --quiet --release
Jul 25 23:25:30.153  INFO waytoodeep: Joining...
Jul 25 23:25:31.477  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=second
Jul 25 23:25:31.496  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=first
Jul 25 23:25:31.496  INFO waytoodeep: All done! res=("first", "second")
```

。。。同时结果的顺序是正确的。

非常好，我们实现了它！

但是！


### 我们可以做的更好 {#我们可以做的更好}

幸运的是，坏就是好。

下面这个类型困扰着我：

```rust
struct TryJoin<A, B, AR, BR, E>
where
    A: Future<Output = Result<AR, E>>,
    B: Future<Output = Result<BR, E>>,
{
    a: A,
    b: B,
    a_res: Option<AR>,
    b_res: Option<BR>,
}
```

我其实只有在 `a` 完成后才需要 `a_res` 。一旦 `a` 完成然后将结果存储到 `a_res` ，我们就不再需要 `a` 了。

实际上，甚至我们不应该再碰 `a`  。

这听起来更像我们要么持有 `A` 要么持有 `AR` ，但是永远不会同时持有。

> `SUM TYPES` : Rust 的枚举就是一个汇总类型。

所以！汇总类型。Rust 枚举。这就是我们想要的。让我们创建一个叫做 `State` 的类型，然后它有两个变体：
一个用于它还是 future 对象，一个用于它是一个结果。非常简单！

```rust
enum State<F, T, E>
where
    F: Future<Output = Result<T, E>>,
{
    Future(F),
    Ok(T),
}
```

这将会非常棒！

让我们赋给我们的 `TryJoin` 结构体：

```rust
struct TryJoin<A, B, AR, BR, E>
where
    A: Future<Output = Result<AR, E>>,
    B: Future<Output = Result<BR, E>>,
{
    a: State<A, AR, E>,
    b: State<B, BR, E>,
}
```

（是不是非常漂亮）

然后初始化它们：

```rust
pub fn try_join<A, B, AR, BR, E>(a: A, b: B) -> impl Future<Output = Result<(AR, BR), E>>
where
    A: Future<Output = Result<AR, E>>,
    B: Future<Output = Result<BR, E>>,
{
    TryJoin {
        a: State::Future(a),
        b: State::Future(b),
    }
}
```

非常酷。然后我们只需要稍微调整一下我们的 `poll` 方法，我们需要将 `Pin<&mut Self>` 转换为 `&mut Self` 。。。

```rust
fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
    let this = unsafe { self.get_unchecked_mut() };
```

这是可以的，因为我们承诺维护不变量，也就是说我们不会转移（move） `State::Future` 内部。

然后如果 `a` 是 `State::Future` 我们就轮询它，然后我们再传播错误或者保存它的结果：

```rust
if let State::Future(a) = &mut this.a {
    let a = unsafe { Pin::new_unchecked(a) };
    if let Poll::Ready(res) = a.poll(cx) {
        match res {
            Ok(t) => this.a = State::Ok(t),
            Err(e) => return Err(e).into(),
        }
    }
}
```

然后同样的修改 `b` 。。。

```rust
// you can figure that one out, I believe in you
```

然后我们就完成了如果它们都是 `State::Ok` ！否则我们就返回 `Poll::Pending` :

```rust
match (this.a, this.b) {
    (State::Ok(a), State::Ok(b)) => Ok((a, b)).into(),
    _ => Poll::Pending,
}
```

非常好。

除了它无法通过编译：

```nil
$ RUST_LOG=info cargo run --quiet --release
error[E0507]: cannot move out of `this.a` which is behind a mutable reference
  --> src/tj.rs:65:16
   |
65 |         match (this.a, this.b) {
   |                ^^^^^^ move occurs because `this.a` has type `State<A, AR, E>`, which does not implement the `Copy` trait

error[E0507]: cannot move out of `this.b` which is behind a mutable reference
  --> src/tj.rs:65:24
   |
65 |         match (this.a, this.b) {
   |                        ^^^^^^ move occurs because `this.b` has type `State<B, BR, E>`, which does not implement the `Copy` trait

error: aborting due to 2 previous errors

For more information about this error, try `rustc --explain E0507`.
error: could not compile `waytoodeep`

To learn more, run the command again with --verbose.
```

因为。。。我们只有 `&mut Self` 而不是 `Self` 。

我们没有自己的所有权，仅仅是借用我们自己。

所以，我们不能将将我们的成员转移（move）出去，因为我们不能阻止其他人再次轮询 `TryJoin` 。
所以这种情况，我们需要崩溃（panic）。

当然，如果我们像 `Option<T>` 那样有一个 `.take()` 方法事情就会大大简化。
它返回 Option 拥有的任何内容，或者 `None` 。

但是我们没有 `None` 。我们只有 `State::Future` 和 `State::OK` ，没有“中立”（neutral）状态。

让我们创建一个：

```rust
enum State<F, T, E>
where
    F: Future<Output = Result<T, E>>,
{
    Future(F),
    Ok(T),
    Gone,
}
```

现在，我们可以将 `this.a` 和 `this.b` 替换为 `State::Gone` 。。。或者它的返回结果（我们拥有所有权）。
然后我们就可以将它们转移（move）出去。

但是同时。。。我们需要再次对其进行模式匹配（pattern match）。

就像：

```rust
match (&this.a, &this.b) {
    (State::Ok(_), State::Ok(_)) => {
        let a = match std::mem::replace(&mut this.a, State::Gone) {
            State::Ok(t) => t,
            _ => unreachable!(),
        };
        let b = match std::mem::replace(&mut this.b, State::Gone) {
            State::Ok(t) => t,
            _ => unreachable!(),
        };
        Ok((a, b)).into()
    }
    _ => Poll::Pending,
}
```

实话说。。。我看过更糟的代码。它只是没那么[DRY](https://en.wikipedia.org/wiki/Don%27t%5Frepeat%5Fyourself)。

非常好的实现！

```nil
$ RUST_LOG=info cargo run --quiet --release
Jul 25 23:52:24.097  INFO waytoodeep: Joining...
Jul 25 23:52:25.050  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=second
Jul 25 23:52:25.061  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=first
Jul 25 23:52:25.061  INFO waytoodeep: All done! res=("first", "second")
```

看，只有 11ms 的间隔。


### 更进一步？ {#更进一步}

这段代码再次困扰了我：

```rust
struct TryJoin<A, B, AR, BR, E>
where
    A: Future<Output = Result<AR, E>>,
    B: Future<Output = Result<BR, E>>,
{
    a: State<A, AR, E>,
    b: State<B, BR, E>,
}
```

因为现在 `a` 和 `b` 是三态的（tri-state）： `Future` 、 `Ok` 或者 `Gone` 。

如果 `a` 和 `b` 都是 `Gone` 呢？这个状态不合理！

如果发生了这个状态，我们现在将会永远返回 `Poll::Pending` -- 这不太好 -- 一个死锁。

我们真正想要的是。。。两个枚举。实际上我们想要整个 `TryJoin` 类型是一个 `enum` 。

```rust
enum TryJoin<A, B, AR, BR, E>
where
    A: Future<Output = Result<AR, E>>,
    B: Future<Output = Result<BR, E>>,
{
    Polling {
        a: State<A, AR, E>,
        b: State<B, BR, E>,
    },
    Done,
}
```

像这样初始化：

```rust
pub fn try_join<A, B, AR, BR, E>(a: A, b: B) -> impl Future<Output = Result<(AR, BR), E>>
where
    A: Future<Output = Result<AR, E>>,
    B: Future<Output = Result<BR, E>>,
{
    TryJoin::Polling {
        a: State::Future(a),
        b: State::Future(b),
    }
}
```

然后，surprice！ `Poll<T>` 实现了 [Try](https://doc.rust-lang.org/stable/std/ops/trait.Try.html) trait。所以我们可以使用 `?` 。
所以最终我们的代码实际上非常短小：

```rust
impl<A, B, AR, BR, E> Future for TryJoin<A, B, AR, BR, E>
where
    A: Future<Output = Result<AR, E>>,
    B: Future<Output = Result<BR, E>>,
{
    type Output = Result<(AR, BR), E>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = unsafe { self.get_unchecked_mut() };
        let (a, b) = match this {
            Self::Polling { a, b } => (a, b),
            Self::Done => panic!("TryJoin future polled after completion"),
        };

        if let State::Future(fut) = a {
            if let Poll::Ready(res) = unsafe { Pin::new_unchecked(fut) }.poll(cx) {
                *a = State::Ok(res?);
            }
        }

        if let State::Future(fut) = b {
            if let Poll::Ready(res) = unsafe { Pin::new_unchecked(fut) }.poll(cx) {
                *b = State::Ok(res?);
            }
        }

        match (a, b) {
            (State::Ok(_), State::Ok(_)) => match std::mem::replace(this, Self::Done) {
                Self::Polling {
                    a: State::Ok(a),
                    b: State::Ok(b),
                } => Ok((a, b)).into(),
                _ => unreachable!(),
            },
            _ => Poll::Pending,
        }
    }
}
```

现在，我知道你在想什么。 `Pin<&mut T>` 不是恰恰用来避免像 `std::mem::swap` 和 `std::mem::replace` 吗？
这些所有的转移（move）都是围绕着内存！这是被禁止的！是的，我们承诺了不去转移（move）它。
但是在这个情况下，我们只是在完成轮询两个 future 对象后转移了 `self` / `this` 。

然后我们就再也没有使用过两个 future 对象，无论固定还是非固定。同时我们从来也没保证过结果自身是否将要被固定（pinned）！

我们只需要决定某些东西是“永远固定”还是“永不固定”，然后我们可能会编写结果正确的代码。

在我们的场景下，只有 `TryJoin::Polling(State::Future(_))` 就是“永远固定” 的，其他都不是。

当然，我们快速的从 `Pin<&mut Self>` 切换到 `&mut Self` ，然后又回到 `Pin<&mut A>` ，
但只要我们不要在中间移动就没有问题。

如果我们在持有 future 对象的情况下使用 `std::mem:replace` 或 `std::mem::swap` 就会不妙。
所以，我们还好，我想，我不太确定。如果不是，有人应该会留言。


### 就这样 {#就这样}

让我们回顾我们的工作：

```rust
// in `src/tj.rs`

use std::{
    future::Future,
    pin::Pin,
    task::{Context, Poll},
};

pub fn try_join<A, B, AR, BR, E>(a: A, b: B) -> impl Future<Output = Result<(AR, BR), E>>
where
    A: Future<Output = Result<AR, E>>,
    B: Future<Output = Result<BR, E>>,
{
    TryJoin::Polling {
        a: State::Future(a),
        b: State::Future(b),
    }
}

enum State<F, T, E>
where
    F: Future<Output = Result<T, E>>,
{
    Future(F),
    Ok(T),
}

enum TryJoin<A, B, AR, BR, E>
where
    A: Future<Output = Result<AR, E>>,
    B: Future<Output = Result<BR, E>>,
{
    Polling {
        a: State<A, AR, E>,
        b: State<B, BR, E>,
    },
    Done,
}

impl<A, B, AR, BR, E> Future for TryJoin<A, B, AR, BR, E>
where
    A: Future<Output = Result<AR, E>>,
    B: Future<Output = Result<BR, E>>,
{
    type Output = Result<(AR, BR), E>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = unsafe { self.get_unchecked_mut() };
        let (a, b) = match this {
            Self::Polling { a, b } => (a, b),
            Self::Done => panic!("TryJoin future polled after completion"),
        };

        if let State::Future(fut) = a {
            if let Poll::Ready(res) = unsafe { Pin::new_unchecked(fut) }.poll(cx) {
                *a = State::Ok(res?);
            }
        }

        if let State::Future(fut) = b {
            if let Poll::Ready(res) = unsafe { Pin::new_unchecked(fut) }.poll(cx) {
                *b = State::Ok(res?);
            }
        }

        match (a, b) {
            (State::Ok(_), State::Ok(_)) => match std::mem::replace(this, Self::Done) {
                Self::Polling {
                    a: State::Ok(a),
                    b: State::Ok(b),
                } => Ok((a, b)).into(),
                _ => unreachable!(),
            },
            _ => Poll::Pending,
        }
    }
}
```

还有我们小小的 HTTPS 客户端：

```rust
// in `src/main.rs`

use color_eyre::Report;
use std::{net::SocketAddr, sync::Arc};
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::TcpStream,
};
use tokio_rustls::{rustls::ClientConfig, TlsConnector};
use tracing::info;
use tracing_subscriber::EnvFilter;
use webpki::DNSNameRef;

mod tj;

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Report> {
    setup()?;

    info!("Joining...");
    let res = tj::try_join(fetch_thing("first"), fetch_thing("second")).await?;
    info!(?res, "All done!");

    Ok(())
}

#[allow(dead_code)]
async fn fetch_thing(name: &str) -> Result<&str, Report> {
    // look out it's port 443 now
    let addr: SocketAddr = ([1, 1, 1, 1], 443).into();
    let socket = TcpStream::connect(addr).await?;

    // establish a TLS session...
    let connector: TlsConnector = {
        let mut config = ClientConfig::new();
        config
            .root_store
            .add_server_trust_anchors(&webpki_roots::TLS_SERVER_ROOTS);
        Arc::new(config).into()
    };
    let dnsname = DNSNameRef::try_from_ascii_str("one.one.one.one")?;
    let mut socket = connector.connect(dnsname, socket).await?;

    // we're writing straight to the socket, there's no buffering
    // so no need to flush
    socket.write_all(b"GET / HTTP/1.1\r\n").await?;
    socket.write_all(b"Host: one.one.one.one\r\n").await?;
    socket.write_all(b"User-Agent: cool-bear\r\n").await?;
    socket.write_all(b"Connection: close\r\n").await?;
    socket.write_all(b"\r\n").await?;

    let mut response = String::with_capacity(256);
    socket.read_to_string(&mut response).await?;

    let status = response.lines().next().unwrap_or_default();
    info!(%status, , "Got response!");

    // dropping the socket will close the connection

    Ok(name)
}

fn setup() -> Result<(), Report> {
    if std::env::var("RUST_LIB_BACKTRACE").is_err() {
        std::env::set_var("RUST_LIB_BACKTRACE", "1")
    }
    color_eyre::install()?;

    if std::env::var("RUST_LOG").is_err() {
        std::env::set_var("RUST_LOG", "info")
    }
    tracing_subscriber::fmt::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    Ok(())
}
```

And it works.

```nil
$ RUST_LOG=info cargo run --quiet --release
Jul 26 00:08:13.399  INFO waytoodeep: Joining...
Jul 26 00:08:13.707  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=first
Jul 26 00:08:13.709  INFO waytoodeep: Got response! status=HTTP/1.1 200 OK name=second
Jul 26 00:08:13.710  INFO waytoodeep: All done! res=("first", "second")
```

2ms 间隔！这是一个新的记录。
