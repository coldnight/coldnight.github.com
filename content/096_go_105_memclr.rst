Go 1.5 内存清除汇编源码注释
===========================

:tags: golang,go,源码,plan9 汇编,内存清除,memclr
:date: 2019-02-11
:category: Go
:author: cold
:status: published
:slug: golang-105-memclr-assembly-comment
:summary: Go 为了实现内存复用会将已经回收到 cache 的内存清除，清除逻辑使用汇编实现，本文是基于自己的理解进行的注释。

最近在阅读 Go1.5 的源码，发现源码中多处调用了了 ``memclr`` ，进一步深入了解发现原来 Go 为了实现内存复用会将已经回收到 ``cache`` 的内存清除，清除逻辑使用汇编实现，本文是基于自己的理解进行的注释。

**注意**:

1. 最新版的 Go 中（1.11）该函数已经改名为 ``memclrNoHeapPointers``
2. 我自己添加的注释以 ``//#`` 开始
3. Go 针对不同的平台实现了不同的内存清除，这里仅对 ``amd64`` 平台下的实现进行注释：


源文件: https://github.com/golang/go/blob/release-branch.go1.5/src/runtime/memclr_amd64.s

.. code-block:: assembly

   // Copyright 2014 The Go Authors. All rights reserved.
   // Use of this source code is governed by a BSD-style
   // license that can be found in the LICENSE file.

   // +build !plan9

   #include "textflag.h"

   // NOTE: Windows externalthreadhandler expects memclr to preserve DX.

   // void runtime·memclr(void* ptr, uintptr n)

   //# - TEXT 在 Plan9 中用于声明函数
   //# - runtime·memclr 为函数名，中间的·不是普通的点在 Mac 下通过 Option+Shift+9 打出，· 前是包名后面是函数名
   //# - SB（伪寄存器）全局静态基指针，用来声明函数或全局变量（此处用来声明函数）
   //# - NOSPLIT 定义在 https://github.com/golang/go/blob/master/src/runtime/textflag.h
   //# - $0-16：
   //#   + $0 栈帧大小为0（局部变量+可能需要的额外调用函数的参数空间总大小，不包括调用其他函数时的 ret address 的大小）
   //#   + 16 参数基返回值的大小（16 表示两个双四字的参数）
   TEXT runtime·memclr(SB), NOSPLIT, $0-16
        //# FP（伪寄存器）：通过 `symbol+offset(FP)` 的方式引用输入参数
        //# symbol 没有任何用，只是增加可读性，但不能省略
        //# FP 指向整个栈帧的底部的 BP 寄存器
   	MOVQ	ptr+0(FP), DI   //# 第一个参数移动到 DI 寄存器（DI 目标索引寄存器）
   	MOVQ	n+8(FP), BX     //# 第二个参数移动到 BX 寄存器（BX 为基址寄存器，用于内存寻址）
   	XORQ	AX, AX          //# 清零 AX 寄存器（AX 为累加寄存器）

   // MOVOU seems always faster than REP STOSQ.
   tail:
   	TESTQ	BX, BX          // set ZF to 1 if n is 0
   	JEQ	_0              // jump to _0 if ZF == 1(returns)
   	CMPQ	BX, $2
   	JBE	_1or2           // jump to _1or2 if n <= 2
   	CMPQ	BX, $4
   	JBE	_3or4           // jump to _3or4 if n > 2 and n <= 4
   	CMPQ	BX, $8
        // ...
   	JBE	_5through8
   	CMPQ	BX, $16
   	JBE	_9through16

        //# 大于 16 开始使用 128 位寄存器 X0，
        //# PXOR 将 X0 寄存器置为 0
   	PXOR	X0, X0
   	CMPQ	BX, $32
   	JBE	_17through32
   	CMPQ	BX, $64
   	JBE	_33through64
   	CMPQ	BX, $128
   	JBE	_65through128
   	CMPQ	BX, $256
   	JBE	_129through256
   	// TODO: use branch table and BSR to make this just a single dispatch
   	// TODO: for really big clears, use MOVNTDQ.

   //# 大于 256 则通过循环
   loop:
        //# MOVOU 相当于 AT&T/Intel 的 MOVDQU -- 移动非对齐的双四字
        //# X0 相当与 AT&T/Intel 的 SSE 新增的 %xmm0(128位元暂存器)
        //# 参见 https://zh.wikipedia.org/wiki/SSE
   	MOVOU	X0, 0(DI)
   	MOVOU	X0, 16(DI)
   	MOVOU	X0, 32(DI)
   	MOVOU	X0, 48(DI)
   	MOVOU	X0, 64(DI)
   	MOVOU	X0, 80(DI)
   	MOVOU	X0, 96(DI)
   	MOVOU	X0, 112(DI)
   	MOVOU	X0, 128(DI)
   	MOVOU	X0, 144(DI)
   	MOVOU	X0, 160(DI)
   	MOVOU	X0, 176(DI)
   	MOVOU	X0, 192(DI)
   	MOVOU	X0, 208(DI)
   	MOVOU	X0, 224(DI)
   	MOVOU	X0, 240(DI)
   	SUBQ	$256, BX  //# 递减 BX
   	ADDQ	$256, DI  //# 递增 DI
   	CMPQ	BX, $256
   	JAE	loop      //# 如果 BX 依然大于 256 则继续循环
   	JMP	tail      //# 否则进入 tail

   _1or2:
   	MOVB	AX, (DI)
   	MOVB	AX, -1(DI)(BX*1)
   	RET
   _0:
   	RET
   _3or4:
   	MOVW	AX, (DI)
   	MOVW	AX, -2(DI)(BX*1)
   	RET
   _5through8:
   	MOVL	AX, (DI)
   	MOVL	AX, -4(DI)(BX*1)
   	RET
   _9through16:
   	MOVQ	AX, (DI)
   	MOVQ	AX, -8(DI)(BX*1)
   	RET
   _17through32:
   	MOVOU	X0, (DI)
   	MOVOU	X0, -16(DI)(BX*1)
   	RET
   _33through64:
   	MOVOU	X0, (DI)
   	MOVOU	X0, 16(DI)
   	MOVOU	X0, -32(DI)(BX*1)
   	MOVOU	X0, -16(DI)(BX*1)
   	RET
   _65through128:
   	MOVOU	X0, (DI)
   	MOVOU	X0, 16(DI)
   	MOVOU	X0, 32(DI)
   	MOVOU	X0, 48(DI)
   	MOVOU	X0, -64(DI)(BX*1)
   	MOVOU	X0, -48(DI)(BX*1)
   	MOVOU	X0, -32(DI)(BX*1)
   	MOVOU	X0, -16(DI)(BX*1)
   	RET
   _129through256:
   	MOVOU	X0, (DI)
   	MOVOU	X0, 16(DI)
   	MOVOU	X0, 32(DI)
   	MOVOU	X0, 48(DI)
   	MOVOU	X0, 64(DI)
   	MOVOU	X0, 80(DI)
   	MOVOU	X0, 96(DI)
   	MOVOU	X0, 112(DI)
   	MOVOU	X0, -128(DI)(BX*1)
   	MOVOU	X0, -112(DI)(BX*1)
   	MOVOU	X0, -96(DI)(BX*1)
   	MOVOU	X0, -80(DI)(BX*1)
   	MOVOU	X0, -64(DI)(BX*1)
   	MOVOU	X0, -48(DI)(BX*1)
   	MOVOU	X0, -32(DI)(BX*1)
   	MOVOU	X0, -16(DI)(BX*1)
   	RET


在此期间参阅了大量的资料，最大的坑就是 Go 使用的汇编是 Plan9而非常见的 x86 汇编，参考资料如下：

- https://quasilyte.github.io/blog/post/go-asm-complementary-reference/
- https://gocn.vip/article/733
- https://github.com/golang/arch/blob/master/x86/x86.csv
- https://golang.org/doc/asm
