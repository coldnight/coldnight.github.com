Title: Python github 私有项目通过 buildbot 进行 Review
Date: 2016-05-22
Category: Python
Tags: Python, github, 私有, 可持续集成, homu, buildbot, review

## 背景
随着公司开发团队的壮大, 团队中每个人的水平参差不齐, 为了保证项目质量我们打算对
提交的代码进行 review, 但是苦于一直没有好的 review 机制. 前段时间我在逛 
[Rust][r] 社区是发现了他们有一个 review 机器人 [Homu][h] 非常不错, 
研究一下后我将其应用到我们当前 Python 项目中来配合 review, 我感觉非常棒, 
今天抽空就分享给大家.


## 技术栈
本文涉及的项目和技术有:

- [Homu][h]
- [buildbot][b]
- [git-pylint-commit-hook][gpch]
- [Docker][d]


## 0. 隔离 Github 部署 Key
Github 可以添加部署 Key 来实现部署, 但是每个项目必须是不同的部署 key. 这就给
多个私有项目的可持续集成带来一定的困难, 因为 buildbot 是通过轮询来获取 git 分支
变更的, 并且 buildbot 不支持指定私钥. 

我们前期想到的是通过 [Docker][d] 来分别跑每个项目的 buildbot, 这样部署 key 就
可以很容易进行隔离. 后面我们找到一种更好的方式来指定部署 Key: 通过 `ssh_config`
来指定别名.

```shell
$ cat ~/.ssh/config
Host github-project-a-alias
    Hostname github.com
    IdentityFile "~/.ssh/project_a_deploy_key"

Host github-project-b-alias
    Hostname github.com
    IdentityFile "~/.ssh/project_b_deploy_key"
```

这样就可以将原有的项目地址做如下转换
```diff
- git@github.com:Owner/a
+ git@github-project-a-alias:Owner/a

- git@github.com:Owner/b
+ git@github-project-b-alias:Owner/b
```

## 1. 搭建 homu

[Homu][h] 是一个构建在 Github 和 buildbot(或 Travis) 之上的工具, 它通过监听
PR 里的评论来触发 buildbot 构建, 并监听构建结果, 如果构建成功则自动合并 PR

[Homu][h] 的搭建可以参见其项目地址有简单的介绍, 非常简单这里就不在赘述.


Homu 是针对 Rust 开发, 并没有考虑私有项目, 所以其信息都是公开的, 为了减少
暴漏信息带来的安全性问题, 我们将 Homu 搭建在内网并通过 ssh 端口转发将 Homu 的
端口发送到一台公网服务器:

```shell
ssh -o TCPKeepAlive=yes -CfNgR 127.0.0.1:54856:127.0.0.1:54856 user@remote.ip.address
```

然后通过以下 nginx 配置文件来仅暴漏供 Github webhook 调用的地址:
```
upstream homu {
    server 127.0.0.1:54856;
}


server {
    listen public.ip.address:54856;

    location /github {
        proxy_set_header Host $http_host;
        proxy_redirect off;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Scheme $scheme;
        proxy_pass http://homu;
    }

    location ~* (?:^|/)\. {
        deny all;
    }
}
```

这样就可以隐藏 Homu 暴露的信息, 其他完整功能可以通过内网访问.

## 2. 搭建 buildbot

buildbot 其官方文档也比较详细, 可以参考其官方文档进行搭建, 需要注意的是目前 
[homu][h] 适配的 buildbot 是 0.8, 目前 buildbot-0.9 正在发布 beta, 和 0.8
有很多地方都不兼容.

这里着重说一下结合 pylint 做代码检查.

### 代码检查
我们希望通过 buildbot 使用 pylint 来做代码检查, 但是考虑到历史遗留问题我们仅
希望对当前提交的变更做检查. 综合一下几个方面 [git-pylint-commit-hook][gpch] 可
以完美的满足需求:

- [git-pylint-commit-hook][gpch] 配置成 pre-commit 可以对`将要被提交(git add 过)的文件`进行检查
- 通过 `git reset HEAD^ --soft` 可以将上一次提交的文件变成`将要被提交的文件`
- [Homu][h] 在接受(r+)某一 PR 时会将变更通过 `--no-ff`(no fast-forward) 的方式合并到 `auto` 分支
- 通过 no fast-forward 合并相当于将所涉及的提交打了一个节点, 在通过 `git reset HEAD^ --soft` 进行重置时会将所涉及的提交的文件都变成`将要被提交的文件`

```shell
$ git checkout -b feature-test
$ # edit file a
$ git add a && git commit -m 'Change file a'
$ # edit file b
$ git add b && git commit -m 'Change file b'
$ git checkout master
$ git merge --no-ff feature-test
$ git reset HEAD^ --soft
$ git status
On branch master
Changes to be committed:
  (use "git reset HEAD <file>..." to unstage)

        modified:   a
        modified:   b

```

buildbot 配置文件片段如下:
```python
lint_factory = util.BuildFactory()
lint_factory.addStep(steps.Git(repourl=repo, mode='full', branch='auto', progress=True))
lint_factory.addStep(steps.ShellCommand(command=["pip", "install", "-U", "-r", "requirements.txt"]))
lint_factory.addStep(steps.ShellCommand(command=["git", "reset", "HEAD^", "--soft"]))
lint_factory.addStep(steps.ShellCommand(command=["git-pylint-commit-hook", "--limit=8", "--pylintrc=./.pylintrc"]))

c['builders'].append(
    util.BuilderConfig(name="auto-lint",
      slavenames=["example-slave"],
      factory=lint_factory))
```

可根据自身需求做变更, 比如 `--limit=8` 来限制最低分数, 低于这个分数将导致构建
失败.

buildbot 可自定义性很强, homu 是可以支持多个 builder 的. 所以除了代码检查我们
还有测试. Homu 在检测这两个 Builder 都构建成功后才会合并当前 PR.

## 3. 工作流
实现了如上功能之后我们对工作流进行了调整:

1. 通过主题分支开发并往长期分支提交 PR
2. reviewer 在 `LGTM` 后提交评论 `r+` 并 @ Homu 账号(@homu r+)
3. 之后 Homu 会合并 PR 变更到 `auto` 分支
4. buildbot 监听到 `auto` 分支的变更后就会运行相应构建并在完成后后通知 Homu
5. Homu 在收到成功的通知后合并 PR 到长期分支

## 4. 已知问题
Homu 在将 `auto` 分支合并到长期分支时会采用 `fast-forward` 的方式进行合并, 
所以不会产生新的 commit, buildbot 0.8 对同一提交(sha 相同) 仅触发一次 build.
所以会导致其他一些比如自动部署的 builder 无法触发.


[g]: https://github.com
[r]: https://github.com/rust-lang/rust
[h]: https://github.com/barosl/homu
[gpch]: https://github.com/sebdah/git-pylint-commit-hook
[b]: https://github.com/buildbot/buildbot
[d]: https://www.docker.com
