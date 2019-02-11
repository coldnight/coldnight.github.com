Docker Snippets
================

:tags: docker, snippets
:date: 2019-02-11 17:30
:category: Docker
:author: cold
:status: published
:slug: docker-snippets
:summary: 分享关于 Docker 的一些坑


Docker 和 iptables
-------------------

如果你用 Docker 启动了一个服务暴露了一个端口，同时你又想通过 iptables 限制该端口的访问，这时候你按照经验往 ``INPUT`` 链插入一条规则发现并没有卵用，你应该看看 Docker 的官方文档 `Docker and iptables <https://docs.docker.com/network/iptables/#add-iptables-policies-before-dockers-rules>`_ 。这是因为 Docker 使用 iptables 提供网络隔离，只有在 ``DOCKER`` 和 ``DOCKER-USER`` 链上的规则才对容器暴露的端口生效，官方推荐用户在 ``DOCKER-USER`` 链上新增自定义规则。


使用 Docker 多阶段构建
----------------------

自 Docker 17.05 Docker 开始支持多阶段构建，使用 Docker 多阶段构建有两个好处：

1. 优化镜像大小
2. 在镜像中排除源代码


比如一个典型的 Go 应用如果想要在镜像构建过程中编译，但是又不想镜像中包含源代码可以按照如下方式编写 ``Dockerfile``

.. code-block:: dockerfile

   FROM golang:1.10 as intermediate

   ENV PROJECT_PATH $GOPATH/src/github.com/coldnight/foobar
   ADD . $PROJECT_PATH/
   WORKDIR $PROJECT_PATH
   RUN go install

   FROM alpine:latest

   COPY --from=intermediate /go/bin/foobar /go/bin/foobar
   CMD ["/go/bin/foobar"]


具体使用方式参见官方文档 `Use multi-stage builds <https://docs.docker.com/develop/develop-images/multistage-build/>`_ 。

CMD vs ENTRYPOINT
------------------

我在一开始使用 Docker 时无法确定该使用 CMD 和 ENTRYPOINT 中的哪一个，其实很简单 CMD 可以在 ``docker run`` 时被覆盖，而 ENTRYPOINT 不可以，比如如下 ``Dockerfile``:

.. code-block:: dockerfile

   FROM alpine:latest

   CMD ['/bin/sh']

假设通过如下命令构建镜像:

.. code-block:: shell

   $ docker build . -t demo


如果通过如下命令运行容器则默认启动 ``/bin/bash``:

.. code-block:: shell

   $ docker run -it demo

但如果想覆盖则可以通过如下方式运行容器:

.. code-block:: shell

   $ docker run -it demo echo 'hello, world'


如果将 Dockerfile 中 CMD 替换为 ENTRYPOINT 则无法通过上述方式覆盖默认的启动命令。


Docker Web 管理系统
--------------------

觉得手动敲命令管理本地 Docker 麻烦？可以试试本地安装一个 `portainer <https://github.com/portainer/portainer>`_ ，通过 Web 界面管理本地 Docker。
