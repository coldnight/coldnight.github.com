Title: 通过 pyenv 在生产环境安装 Python 3
Date: 2018-11-21
Category: Python
Tags: Python, 2to3

[pyenv](https://github.com/pyenv/pyenv) 是一个简单的 Python 版本管理, 可以安装对应版本的 Python 不依赖系统的包管理, 我用它来在生产和测试环境安装  Python 3.6.

它的基本原理是安装对应版本的 Python 在它自己的目录下, 然后将对应的 `bin` 目录通过插入 `PATH` 变量里实现.

安装可以参考[官方文档](https://github.com/pyenv/pyenv#basic-github-checkout), 但是用它部署 安装在 HOME 目录下会引起一些权限问题, 所以我将安装目录放在了 `/srv/pyenv` 下:

```
$ git clone https://github.com/pyenv/pyenv.git /srv/pyenv
$ echo 'export PYENV_ROOT="/srv/pyenv"' >> ~/.bash_profile
$ echo 'export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.bash_profile
$ source ~/.bash_profile
```
上面是针对 Bash shell 的安装, zsh 需将 `~/.bash_profile` 替换为 `~/.zshrc`

然后就可以使用 `pyenv` 来安装 Python 了, 我们来安装 Python 3.6.2：

```
$ apt-get install -y make build-essential libssl-dev zlib1g-dev libbz2-dev \
libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev \
xz-utils tk-dev
$ pyenv install 3.6.2
```

安装完成后可以在 Supervisor 等配置文件中通过 `/srv/pyenv/versions/3.6.2/bin/python` 来使用 Python 3.6.2

## 加速

通常情况下 pyenv 下载 Python 包会比较慢，我们可以手动下载相关包上传到比较快的文件服务，然后进行替换。
假设我们上传后得到的 URL 为： https://file.example.com/sources/python/Python-3.6.2.tar.xz ，
可通过如下命令进行替换：

```shell
$ PYENV_PY_VERSION=3.6.2
$ SPEEDUP_URL=https://file.example.com/sources/python/Python-${PYENV_PY_VERSION}.tar.xz
$ sed -i -e "s;https://www.python.org/ftp/python/${PYENV_PY_VERSION}/Python-${PYENV_PY_VERSION}.tar.xz;${SPEEDUP_URL};" /srv/pyenv/plugins/python-build/share/python-build/${PYENV_PY_VERSION}
$ pyenv install ${PYENV_PY_VERSION}
```
