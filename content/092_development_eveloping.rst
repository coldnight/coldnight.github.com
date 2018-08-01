创业公司技术进化之路
====================

:tags: Python
:date: 2018-07-31
:category: Python
:author: cold
:status: draft
:slug: development-eveloping-in-startup
:summary:

    本文分享创业公司在技术层面的进化之路。包括技术选型、技术迁移和 CI/CD。

4 年前我有幸加入到现在这家公司，成为了公司早期员工之一，后来在开发上慢慢的得到了一些主动权，然后我就开始凭借着自己的摸索慢慢的完善了一些技术层面的基础设施。
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

但是后端框架使用 Django 好处就是对于人事来说可以更好的筛选简历，因为人事看来简历上出现 Tornado 关键字的实在是太少的，
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


这里 ansible-galaxy 有个坑，就是不支持更新，要想更新以安装的 ansible 角色需要手动删除并重新安装：

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

数据库迁移
----------

SQLAlchemy Migrate
^^^^^^^^^^^^^^^^^^^

.. _SQLAlchemy Migrate: http://sqlalchemy-migrate.readthedocs.io/en/latest/

早期我们是通过手动收集变更的 SQL 语句到指定文件下，然后在上线之前手动在数据库进行执行。后来这块就导致了很多上线问题，
主要是忘记收集和忘记执行，后面在找这方面的解决方案时发现了 `SQLAlchemy Migrate`_ ，经过简单的改造之后用起来还算舒心。

Alembic
^^^^^^^

前面也说过我们对项目拆分的比较细，所以就会产生多个项目连接操作同一个数据库的情况，这里就会产生如下问题：

1. User1 拥有 Repo1 的权限，User2 拥有 Repo2 的权限，Repo1 和 Repo2 同时连接操作 DB1
2. User1 在 Repo1 针对 DB1 进行了表结构变更
3. 这时候 User2 要同时让这些变更生效


Django Migrate
^^^^^^^^^^^^^^^


单元测试
--------

pytet
^^^^^^

mock
^^^^^

测试数据库
^^^^^^^^^^


uWSGI vs Gunicorn
-----------------

Django vs Tornado
------------------

Vue vs React
------------


CI/CD
-----

GitHub 到 GitLab
----------------


Sentry
-------


目录结构
---------
