Title: 解决 macOS 下安装 pycurl 后导入错误
Date: 2018-11-20 15:00
Category: Python
Tags: pycurl, macOS, OSX

在 macOS 下安装 PycURL 后 `import curl` 会提示:

```
ImportError: pycurl: libcurl link-time version (7.43.0) is older than compile-time version (7.49.1)
```
这是因为系统中的 curl 版本过老导致, 可以通过使用  `Homebrew` 安装最新版来解决:

```
$ brew install curl
```

安装完成后会提示一些信息, 按照提示的信息将 `curl` 加入到 PATH 路径, 参考:

```
$ echo 'export PATH="/usr/local/opt/curl/bin:$PATH"' >> ~/.zshrc
```

如果使用的非 zsh 则将上面替换为:
```
$ echo 'export PATH="/usr/local/opt/curl/bin:$PATH"' >> ~/.bashrc
```

完成后重新初始化 shell:

```
$ source ~/.zshrc
```

然后禁用 `pip` 缓存重新安装 `pycurl`:

```
$ pip uninstall pycurl
$ pip install -U pycurl --no-cache-dir
```

如果使用 `tox` 可以使用  `-r` 参数重建 `virtualenv`:

```
$ tox -r -e py36
```

若依然存在问题可以手动删除 `pip` 缓存目录重试:

```
$ rm -rf ~/Library/Caches/pip/
```

如遇到下面问题：
```
    __main__.ConfigurationError: Curl is configured to use SSL, but we have not been able to determine which SSL backend it is using. Please see PycURL documentation for how to specify the SSL backend manually.
```

可以通过如下命令重新安装 `curl`

```shell
brew reinstall curl --with-openssl
```

然后重新安装 `pycurl`，如果还不行可以尝试重启 Shell。

如果通过 `tox` 依然不行可以设置如下环境变量：


```shell
export LDFLAGS=-L/usr/local/opt/openssl/lib
export CPPFLAGS=-I/usr/local/opt/openssl/include
```
