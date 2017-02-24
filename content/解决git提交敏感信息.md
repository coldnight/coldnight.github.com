Title: 解决git提交敏感信息(回退git版本库到某一个commit)
Category: git
tags: git, 敏感信息, 回退, commit, 版本库
date: 2013-06-07

git是一个很好的版本库, 现在很多人用它, 并在github上创建项目, 相信大家都有过将敏感信息提交版本的经历, 如何删除? 好像只有删除版本库来解决, 其实我们可以通过回退版本库删除相应的commit来将提交的敏感信息去掉.

## 备份本地代码
首先我们将本地代码的更改备份一下, 以防丢失更改

## 回退本地代码的commit
备份完数据, 我们就可以先回退本地的版本库
```bash
git reset --hard HEAD~1        # 回退到上一次的提交, 如果是上n次就将1改成对应的数字
```

## 回退远端版本库
接下来如果你直接提交会发现提交不了, 说远端做了更改需要先pull一下, 如果pull咱们就白白做上面的操作, 所以我们可以在别的分支操作

### 新建一个分支, 并提交
```bash
git checkout -b temp
git push origin temp:temp
```

### 重建主分支
下面我们可以删除并重建主分支, 如果是`github`的话需要将`Default Branch`切换到别的分支(项目主页->Settings即可看到)
```bash
git push origin --delete master   # 删除远端主分支
git branch -d master              # 删除本地主分支
git checkout -b master            # 新建主分支并切换到主分支
git push origin master            # 提交主分支
```

这样我们就删除之前提交的敏感信息(如果是`github`现在就可以把默认分支切换到 `master`)

### 删除临时分支
```bash
git branch -d temp
git push origin --delete temp
```
