Title: Python 3.8 新增 multiprocessing.SharedMemory 支持共享内存
Date: 2019-02-28
Category: Python
Tags: Python, 3.8, multiprocessing, 共享, 内存, shared, memory
Slug: python-38-shared-memory

Python 在 2019-02-25 释出了 3.8 早期预览版 [3.8.0a2]，其中新增了 [multiprocessing.SharedMemory](https://docs.python.org/3.8/library/multiprocessing.shared_memory.html) 用以支持共享内存，大大提高多进程之间通信效率。简单看了一下实现代码主要涉及如下 Python 模块

- 内置类型 [memoryview](https://docs.python.org/3.8/library/stdtypes.html?highlight=memoryview#memoryview)
- [mmap](https://docs.python.org/3.8/library/mmap.html)

在 POSIX 平台下共享内存创建过程如下：

1. 基于 `tmpfs` 打开或创建具名（文件名）的共享内存，得到文件描述符
2. 通过 `mmap` 将文件描述符映射进程的内存地址空间
3. 通过 `memoryview` 直接访问经过 `mmap` 映射后的的内存地址空间

## 锁的问题

`memoryview` 通过如下方式使用：

```python
s = bytearray(b'aaa')
m = memoryview(s)
m[0] = 98
print(s)  # outputs: bytearray(b'baa')
```

当上面代码执行 `m[0] = 98` 时实际上调用的是 C 代码 [memory_ass_sub](https://sourcegraph.com/github.com/python/cpython@23f4589b4b7c4a51950a87175ce7fb31b89c8532/-/blob/Objects/memoryobject.c#L2455:1)，然后调用 [PACK_SINGLE](https://sourcegraph.com/github.com/python/cpython@23f4589b4b7c4a51950a87175ce7fb31b89c8532/-/blob/Objects/memoryobject.c#L1743:9) 通过 `memcpy` 覆盖指针原有的值。

所以直接操作 `multiprocessing.SharedMemory` 会产生数据竞争，不应该直接使用，应该使用 [multiprocessing.Value] 和 [multiprocessing.Array] 这种更高层的抽象，锁在这一层级实现。


## 参见

更多关于共享内存参见：

- [浅谈 Linux 共享内存](http://hustcat.github.io/shared-memory-tmpfs/)
- [POSIX Shared Memory](http://man7.org/training/download/posix_shm_slides.pdf)
- [共享内存（上）](https://www.ibm.com/developerworks/cn/linux/l-ipc/part5/index1.html)
- [共享内存（下）](https://www.ibm.com/developerworks/cn/linux/l-ipc/part5/index2.html)

----

[multiprocessing.Value]: https://docs.python.org/3.8/library/multiprocessing.html#multiprocessing.Value
[multiprocessing.Array]: https://docs.python.org/3.8/library/multiprocessing.html#multiprocessing.Array
[3.8.0a2]: https://www.python.org/downloads/release/python-380a2/
