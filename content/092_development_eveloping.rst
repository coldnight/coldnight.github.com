创业公司技术进化之路
====================

:tags: Python,技术选型,CI/CD,GitLab
:date: 2018-08-04
:category: Python
:author: cold
:status: published
:slug: development-eveloping-in-startup
:summary: 本文分享创业公司在技术层面的进化之路。包括技术选型、技术迁移和 CI/CD。

TL;DR

4 年前我有幸加入到现在这家公司，成为了公司早期员工之一，后来在开发上慢慢的得到了一些主动权，然后我就开始凭借着自己的摸索慢慢的完善了一些技术层面相关的东西。
这篇文章主要分享一下这其中的过程，前面的一些文章也有涉及但不全面，这里进行一个总结。其实这篇文章也可以叫作《我在创业公司这 4 年》。

面向招聘的技术选型
------------------

开始公司的业务主要是车载物联网硬件，涉及大量 TCP 连接建立和数据传输，所以此时的技术选型主要是 Tornado，主要用于以下两个方面：

- TCP Server 处理硬件 TCP 连接
- Web 框架，实现客户端 API

中间也尝试使用 Flask 实现管理后台，但当时的我对 Tornado 有非常执着的爱，所以在后面的项目中技术选型依然采用 Tornado 作为主要框架。
所以当时的技术选型是：

- Tornado 作为 TCP Server 和客户端 API Web 框架
- 采用 Flask 作为后台管理的框架
- 前端采用 Bootstrap 3 + jQuery

后面公司转型做互联网车险 SaaS 平台也一直沿用这些选型，直到后来业务开始增长需要招人的时候才发现市面上熟悉 Tornado 的人员比较难招，
这时候又面临公司业务拆分，同时当下前后端分离也比较火，后端模板渲染的情况下需要后端了解一些基本的前端知识，而且这种情况下同前端工程师并不太好协同，
所以这时候新的产品线开始采用如下技术选型

- Django 作为 Web 框架
- 前端使用 React

选择 React 完全是是出于对 React 社区和 JSX 的喜爱，但是做完一个项目之后发现了一个相同的问题，就是招人非常困难，特别对于小公司来说就更难了。
所以后续就把一些新项目改为采用 Vue，招人也从 React 调整为 Vue。所以就目前来讲最终采用的技术选型就是：

- 后端使用 Django 作为 Web 框架
- 前端使用 Vue 作为开发框架

从这些就能看出来小公司在技术选型时就不能完全靠喜好，需要结合市面上的人才占比来选择合适的技术。

采用前后端分离架构之后好处是便是对后端的前端相关知识不再是硬性要求，前后端开发可以并行进行，同时前端代码的质量也比较好把控。
我们就出现很多后端工程师连 Bootstrap 都用不好的情况。

但是后端框架使用 Django 好处就是对于人事来说可以更好的筛选简历，因为人事看来简历上出现 Tornado 关键字的实在是太少了，
所以我们当前年度的 3、4 月份招聘旺季算是基本完成招聘任务的。同时也对 Django 有一个全新的认识，纠正了我很多偏见也学到了很多东西。


基于 Git 的内部包管理
----------------------

由于后期的业务转型的时候项目拆分比较细，所以从这时候起就开始有意识的提取一些公共逻辑放在单独的包里，
早期这个包是通过拷贝的方式放在不同的项目里，这样更新就比较麻烦，后续了解到 ``pip`` 是可以通过 `git` 来安装包的，
所以后来就将这个包放在单独的仓库，每个项目通过在 ``requirements.txt`` 增加类似下面的内容来引用：

.. code-block:: text

   git+ssh://git@github.com/Owner/foobar@v2.14


这样有好处也有一些坏处，好处就是更新迭代方便，坏处就是增加部署成本。

部署
----


Fabric
^^^^^^

早期项目只有一个所以部署基本是手动，后期项目开始变得多起来，手动部署就变得麻烦起来，这时候开始使用 Fabric 写一些部署脚本。
后来就写了一套通用的规则，然后暴漏出一些配置接口，配置诸如仓库 URL，Supervisor 启动项等等，然后将之放在通用库里，
同时编写一套 ``Makefile`` 的规则放在通用库里，我们的通用库取名是 ``allspark``。

这样直接在项目中放一个 ``Makefile`` 添加类似下面的内容：

.. code-block:: makefile

   ALLSPARK_PTH=$(shell python -c 'import os; import allspark;print(os.path.dirname(allspark.__file__))')
   include $(ALLSPARK_PTH)/mkrules.mk

这样就可以通过 ``make`` 进行一键部署，这套机制用了很长时间，目前还有部分维护较少的项目还再使用。

Ansible
^^^^^^^

后来开始立项 Django 的项目，由于 ``fabric`` 相关的封装都在 Tornado 的通用库里，再继续使用就比较麻烦，这时候我就开始寻求新的解决方案，
所以我又开始研究 Ansible，经过简单的了解和尝试我发现只要定义几个简单 role 就可以满足需求，所以我新建了一个仓库存放这些角色，
Ansible 提供了 ``ansible-galaxy`` 来安装依赖，所以再每个项目下都有如下 Ansible 配置文件：

- ansible.cfg -- 指定 hosts 位置
- deploy/ansible/hosts -- 配置主机
- deploy/ansible/requirements.yml -- 指定依赖

  内容如下

  .. code-block:: yml

     - src: git@gitlab.example.com:username/ansible-roles
       scm: git
       name: ansible-roles

- deploy/ansible/dev-playbook.yml  -- 测试环境部署规则，指定变量和角色实现部署
- deploy/ansible/prod-playbook.yml -- 生产环境部署，指定变量和角色实现部署

现在部署相关的就都开始使用 Ansible，部署命令如下：

.. code-block:: shell

   # 需要在控制机上安装依赖
   $ ansible-galaxy install -r deploy/ansible/requirements.yml
   $ ansible-playbook deploy/ansible/dev-playbook.yml


这里 ansible-galaxy 有个坑，就是不支持更新，要想更新已安装的 ansible 角色需要手动删除并重新安装：

.. code-block:: shell

   $ rm ~/.ansible/roles/ansible-roles
   $ ansible-galaxy install -r deploy/ansible/requirements.yml


Deploy Key 到 SSH agent forwarding
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _Using SSH agent forwarding: https://developer.github.com/v3/guides/using-ssh-agent-forwarding/

在部署的过程中需要在服务器上拉取代码，就涉及到仓库权限的问题我们一开始的解决办法是部署之前通过 ``Fabric`` 上传一个 Deploy Key
到目标服务器，在部署完成之后再将对应的 Deploy Key 删除。

一个 Deploy Key 只对应一个仓库的只读权限，这种模式在前期是没有问题的，但是到了后期我们把通用库拆分到独立的仓库通过 ``pip`` 进行安装时就遇到了问题，
这个时候我们开始抛弃 Deploy Key 改为使用 SSH agent forwarding，具体请参见 `Using SSH agent forwarding`_ 。


ORM 使用
---------

手写 SQL
^^^^^^^^^
早期我们使用手写 SQL 的方式与数据库交互，慢慢的我们发现这种方式存在一些问题：

- 大块 SQL 语句在代码中异常丑陋
- 非常容易编写错误的 SQL 语句
- 代码 Review 过程中要额外注意 SQL 注入相关问题
- 为了防止注入，根据条件拼接 SQL 语句比较困难同时拼接代码看起来丑陋并且难以理解

SQLAlchemy Core
^^^^^^^^^^^^^^^^

后来发现 SQLAlchemy 对外提供的接口是分为两层的：

- Core -- 语句生成引擎
- ORM -- 基于语句生成引擎的 ORM

发现只使用 Core 和手写 SQL 并无太大区别，但是解决了上面的所有问题，请看下面示例：

.. code-block:: python

   import sqlalchemy as sa

   from . import db_engine
   from . import Table


   with db_engine.connect() as db:
       db.execute(
           Table.select()
           .where(
                (Table.c.id == 1)
                &
                (Table.c.sex == "male")
                &
                Table.c.is_valid
           )
           .order_by(sa.desc(Table.c.id))
       )

同时也可以支持复杂的 SQL 语句，具体请参见 `文档 <https://docs.sqlalchemy.org/en/latest/core/tutorial.html>`_ 。

Django ORM
^^^^^^^^^^^

再后来的新项目都采用了 Django 并使用 Django 自带的 ORM。

本地开发
---------

我们应用的服务依赖较少，目前只依赖 MySQL 和 Redis。一开始大家都通过统一连接内网的同一服务进行本地开发，
这种开发模式会带来一个问题：

1. 假设其中一名开发人员删除了一个字段，并调整了对应的代码但是没有提交
2. 由于我们基本上是 TDD 的开发模式，此时就会导致其他人的单元测试无法正常运行

基于这种模式我们本地开发引入 Docker，使用 Docker 在本地启动 MySQL 和 Redis 服务，在我们的通用库里提供以下两个文件：

- docker-compose.yml

  .. code-block:: yml

      version: '3'
      services:
          mysql:
              image: mysql:5.6
              restart: always
              ports:
                  - "3306:3306"
              volumes:
                  - ~/.botpy/etc/mysql/conf.d:/etc/mysql/conf.d
                  - ~/.botpy/data/mysql:/var/lib/mysql
              environment:
                  MYSQL_ROOT_PASSWORD: root-password-you-should-replace

          redis:
              image: redis
              restart: always
              ports:
                  - "6379:6379"

- init-db.py -- 包装 mysqldump 和 mysql 命令实现同步内网数据库的脚本

数据库迁移最终流程就变为：

1. 在本地编写并调试，并先应用到本地
2. 提交 MR 在内网的测试数据库进行验证
3. 验证通过后合并 MR 触发部署

   1. 迁移应用到内网的开发数据库
   2. 部署到测试数据库

数据库迁移
----------

SQLAlchemy Migrate
^^^^^^^^^^^^^^^^^^^

.. _SQLAlchemy Migrate: http://sqlalchemy-migrate.readthedocs.io/en/latest/

早期我们是通过手动收集变更的 SQL 语句到指定文件下，然后在上线之前手动在数据库进行执行。后来这块就导致了很多上线问题，
主要是忘记收集和忘记执行，后面在找这方面的解决方案时发现了 `SQLAlchemy Migrate`_ ，经过简单的改造之后用起来还算舒心。

我们在用的过程中遇到的主要问题中文编码问题，经过排查是由于 sqlparse 库导致的，由于没有暴漏相关接口加上 `SQLAlchemy Migrate`_
是由 OpenStack 维护，想要贡献代码非常困难，所以就通过在 ``manage.py`` 增加如下代码来解决：

.. code-block:: python

   import functools
   import sqlparse


   # HACK!!: 替换原函数修复 sqlalchemy-migrate 的编码 bug
   sqlparse.format = functools.partial(sqlparse.format, encoding="utf8")


Alembic
^^^^^^^

.. _Alembic: http://alembic.zzzcomputing.com/en/latest/

虽然 `SQLAlchemy Migrate`_ 已经满足需求，但是在用的过程中发现会有两个问题：

1. 社区更新不积极，
2. 由于 `SQLAlchemy Migrate`_ 仅记录最后一个版本号，不便于多人开发，考虑如下场景：

   1. A 增加了版本 001-foo
   2. B 没有拉取代码也进行迁移就会也增加一个 001-bar
   3. A 的代码合并并部署，001-foo 会被执行（数据库标记 001 已执行）
   4. B 的代码合并并部署，由于 001 被标记为已执行 001-bar 不会进行执行

这时候发现 SQLAlchemy 的作者推出了 `Alembic`_，经过简单的尝试发现该工具更加强大支持类似 Git 的版本控制，
开发也比较活跃，解决了上面两个问题。所以在后续就使用 `Alembic`_ 替换了 `SQLAlchemy Migrate`_ 。

Django Migrate
^^^^^^^^^^^^^^^

后续 Django 的新项目就开始采用 Django 自带的数据库迁移方案，这里就不再细述。

单元测试
--------

测试数据库
^^^^^^^^^^

我觉的我们之所以能成功的推行了单元测试，并将之作为日常开发中衡量代码的标准之一，最大的功劳就是解决了测试数据库相关问题，
就像我之前的文章提到的:

  很长一段时间以来写单元测试都类似写执行脚本，运行一下然后看一下结果。

很大的原因就是没有解决测试数据库相关的问题，比如我写了一个测试然后在我本地数据库填充了数据，测试通过了。然后后面数据再变动测试就失败了。
为了解决这个问题我自己首先实现了一个基于 unittest 的测试收集和运行器，然后在测试运行开始之前插入一段代码做如下事情：

1. 连接配置文件中的数据库并读取表结构信息
2. 根据一定规则生成创建一个新的数据库
3. 将读取的表结构信息应用到新数据库
4. 加载测试包下的一些 SQL 文件并在新的数据库中执行
5. Patch 配置文件，将数据库名调整到新的数据库

后面我们切换到 pytest 作为 test runner 后将这一块逻辑封装成了一个 pytest 的插件。加上后面上的 mock 我们的单元测试才真正的完善。
这也造就了我们大部分项目都达到了 80% ~ 95% 的单元测试覆盖率，同时也为我们之后迁移 Python 3 打下了很好的基础。


pytet 作为 test runner
^^^^^^^^^^^^^^^^^^^^^^^

前面也提到了，一开始我们用的是基于 unittest 自己实现的测试发现、收集和运行的工具，这一块也是我受之前一家公司的影响，
后来发现 pytest 除了是一个强大的测试框架，同时也可以单独用来作为 test runner 使用，我不太喜欢 pytest 这种函数式方式
编写测试，很多 fixture markup 感觉太过隐式，所以我只拿 pytest 作为 test runner，单元测试还是使用 unittest 那一套。

pytest 作为 test runner 对比 unittest 的好处是：

- 社区活跃
- 生态好，插件多
- 结合插件系统非常容易扩展和自定义


mock
^^^^^

参见之前的 `博文 </python-mock-shi-yong-xin-de.html>`_ 。

tox
^^^^

.. _tox: http://tox.readthedocs.io/en/latest/

开始每次运行测试构建测试相关的 Python 环境会比较麻烦，后面接触了 `tox`_ 可以很方便的构建测试环境，
同时支持多个 Python 版本环境构建。

CI/CD
-----

结识 bors 和 homu
^^^^^^^^^^^^^^^^^^^

.. _bors: https://github.com/graydon/bors

.. _homu: https://github.com/barosl/homu

有一段时间我特别关注 Rust 社区，看到他们有一个机器人专门跑单元测试和合并 PR，我就开始思考能不能用到我们的项目中，
经过简单的观察我首先接触到了 `bors`_ ，初次使用后感觉真心酷炫，但是由于它是采用轮询会有如下两个问题：

1. 时效性不好
2. 容易达到 GitHub 接口请求次数上限
3. 还有其他一些功能上的不全

这时候我就发现 Rust 社区早一不使用 `bors`_ 而是改为使用 `homu`_ 了，具体的信息可以参见之前的一篇
`文章 </python-github-si-you-xiang-mu-tong-guo-buildbot-jin-xing-review.html>`_ 。

GitHub
^^^^^^^

参见 `Python github 私有项目通过 buildbot 进行 Review </python-github-si-you-xiang-mu-tong-guo-buildbot-jin-xing-review.html>`_ 。
后面也尝试迁移到 buildbot 0.9，具体方案参见 `Add buildbot 0.9 support steps in README.md #119  <https://github.com/servo/homu/pull/119>`_ 。

GitLab
^^^^^^^

Pipelines
+++++++++

迁移到 GitLab 之后就弃用了 buildbot 改用 GitLab 自身的那一套 CI/CD。具体参见 `GitLab CI/CD <https://about.gitlab.com/features/gitlab-ci-cd/>`_ 。

我们 CI 构建用的自己构建的 Docker 镜像，里面集成了

homu-gitlab
++++++++++++

由于业务扩张 GitHub 上的私有仓库成本开始提升，所以就开始考虑迁移自建的 GitLab，这时面临一个问题就是怎么保持现有的工作流不变，
首先就是要找到一个 `homu`_ 的 GitLab 版实现，经过自己的搜寻后并没有发现合适的，所以我就尝试 Fork 了一份尝试自己迁移到 GitLab，
最终成功的迁移并应用到 GitLab 中来，参见 `coldnight/homu-gitlab <https://github.com/coldnight/homu-gitlab>`_ 。

一些不完美的地方

1. 需要依赖 SSH 私玥

   由于 GitLab 的接口并没有 GitHub 那么完善，没有相关合并 MR 的接口，所以将之前主要依赖 GitHub 接口的部分都统一改成了通过 git 命令操作。

2. 同步功能未实现，每次同步需要通过重启实现

持续进化
^^^^^^^^

在上面完成之后最近还做了一些调整和优化：

1. 启用 GitLab CI/CD 缓存来加速和优化 CI 构建
2. 内网搭建 Docker Registry 统一托管和构建 CI Docker 镜像（这部分也是自动化的）
3. 通过一个仓库托管 homu 的配置文件，每次调整提交后自动重启 homu 服务

pre-commit
^^^^^^^^^^^^

.. _pre-commit: https://github.com/pre-commit/pre-commit

我们一开始代码检测主要利用 Git 的 pre-commit 自己编写 shell 脚本来组合，后来发现 `pre-commit`_ 后开始统一替换为 `pre-commit`_ 。
`pre-commit`_ 的作者也推出了其他很多代码检测相关的工具，我也应用到了我们自己的项目上。

Python
-------

参见 `迁移到 Python 3 </qian-yi-dao-python-3.html>`_ 。

总结
----

简单总结一下目前整个技术栈：

- Python 3

  + Django/Tornado
  + SQLAlchemy
  + pytest
  + tox
  + Alembic
  + Celery

- Ansible
- GitLab

  + CI/CD
  + homu-gitlab
  + pre-commit

- Docker

  + docker-compose

- Vue
- Sentry
