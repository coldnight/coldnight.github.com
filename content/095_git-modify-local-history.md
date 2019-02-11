Title: 修改本地 Git 历史
Date: 2018-11-21 16:33
Category: Git
Tags: Git

> 很早之前一篇发表在内部的文章，抽时间整理了一下发布出来。

**以下操作会修改提交历史, 可能会造成一些不可恢复的问题, *不是* 下面情况不要这么操作**

1. 基于 GitHub Fork -> Pull Request 流程仅针对 Fork 后的仓库进行操作
2. 非第一种情况的前提是当前修改的提交还未提交到远端

**操作下面命令前最好先备份**

## 修改上一条提交的信息

有时候我们在用 Git 提交后发现提交信息(commit message)不是我们预期的内容(错别字或描述错误等), 这个时候我们可能想修改一下上一条提交的信息, 有两种方式可以进行:

```
$ git commit --amend
```
这条命令会直接打开你的终端编辑器让你可以修改提交信息, 也可以组合 `reset` 命令重新提交:

```
$ git reset HEAD^ --soft
$ git commit -m 'new message'
```
这条命令会撤销上一条提交, 并将上一条提交的内容放在工作区内(git add 之后的区域), 然后就可以通过 `git commit` 重新提交.

## 合并多个提交

有时我们为什么了方便记录, 会频繁的创建一些 **临时** 提交, 我们在将提交推送到远端之前可以通过 `git rebase` 来对现有的多条提交进行合并和编辑:

```
$ git rebase -i origin/master   # master 换成你要推送到远端得分支
                                # 基于 Fork -> PR 的流程可以将 origin/master 替换为上游所在的远端分支
```

这条命令会打开你的终端编辑器, 并显示类似如下的内容:

```gitrebase
pick 48c9e10 tmp
pick 23354c6 tmp

# Rebase 3b92b89..23354c6 onto 3b92b89 (2 commands)
#
# Commands:
# p, pick = use commit
# r, reword = use commit, but edit the commit message
# e, edit = use commit, but stop for amending
# s, squash = use commit, but meld into previous commit
# f, fixup = like "squash", but discard this commit's log message
# x, exec = run command (the rest of the line) using shell
# d, drop = remove commit
#
# These lines can be re-ordered; they are executed from top to bottom.
#
# If you remove a line here THAT COMMIT WILL BE LOST.
#
# However, if you remove everything, the rebase will be aborted.
#
# Note that empty commits are commented out
```

内容里面已经说的很明显了, 我们翻译一下关键信息:

```gitrebase
# p, pick = 使用此提交
# r, reword = 使用此提交, 但编辑当前提交的提交信息
# e, edit = 使用此提交, 但停止修改
# s, squash = 使用此提交, 但将当前提交合并到上一条提交
# f, fixup = 和 squash 一样, 但是忽略提交信息
# x, exec = 使用 shell 运行当前行的命令
# d, drop = 移除当前提交
```

那么我想合并这两条提交并更改一下 `commit message` 内容如下:

```gitrebase
r 48c9e10 一个有意义的提交信息
s 23354c6 tmp  # 用 f 也行
```

## 总结

大家可以创建一个练习仓库也体验一下上面命令的不同点, 另外建议多看看 [Pro Git Book](https://git-scm.com/book/en/v2)。
