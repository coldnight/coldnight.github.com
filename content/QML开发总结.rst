PyQt + QML 快速开发GUI总结
##########################
:tags: QML, PyQt, Gui, Python, Qt
:date: 2013-11-06 15:25
:category: PyQt
:author: cold


最近结束一个使用PyQt+QML开发的项目, 在此对一些经验做出总结分享出来. 
结合QML确实可以快速的构建出GUI程序, 但是相关资料太少, 特别是中文资料, 
而且坑太多, 特别是和后端PyQt结合的时候有很多莫名奇妙的问题.
这篇文章会总结这些问题, 避免以后碰到无从下手.

PS:QML的一些基础问题不会在这里讨论, 本篇文章仅讨论一些经验性的问题, 本篇文章使用PyQt4

*请留意文章中间的"注意"*

如何和PyQt交互
==============
QML和PyQt交互主要有三种方法: PyQt渲染数据, 信号传递, QML提供接口

使用PyQt显示QML
---------------
要想在Python里使用PyQt来调用QML显示, 需要用到 ``PyQt4.QtDeclarative.QDeclarativeView`` 实例的 ``setSource`` 将一个 ``PyQt4.QtCore.QUrl`` 对象传递进去, 然后调用 ``PyQt4.QtDeclarative.QDeclarativeView`` 对象的 ``show`` 方法, 下面是一个例子:


.. code-block:: python

    from PyQt4.QtDeclarative import QDeclarativeView
    from PyQt4.QtGui import QApplication
    from PyQt4.QtCore import QUrl

    app = QApplication([])

    view = QDeclarativeView()
    view.setSource(QUrl("/path/to/demo.qml"))
    view.show()

    app.exec_()

通过PyQt渲染数据到QML
---------------------
渲染QML变量有两种方法, 一种是QML中没有定义的变量, 一种是在QML中已经定义好的变量,
第一种是预定义变量, 第二种是设置变量

预定义变量
^^^^^^^^^^
``PyQt4.QtDeclarative.QDeclarativeView`` 的 ``rootContext`` 方法会返回一个QML上下文, 通过这个对象可以对QML进行一些变量的预定义

*注意*, 预定义变量必须在 ``setSource`` 调用之前进行, 还有如果是字符串类型如要使用 ``PyQt4.QtCore.QString`` 进行转换,
数字可以不用转换, 如果是列表或字典需要使用 ``PyQt4.QtCore.QVariant`` 进行转换, 下面是一个例子:

test.qml:

.. code-block:: qml

    import Qt 4.7

    Rectangle {
        id: test
        width: 100; height: 30

        Text {
            anchors.fill:parent;
            text: textData;
        }
    }

test.py:

.. code-block:: python

    from PyQt4.QtDeclarative import QDeclarativeView
    from PyQt4.QtGui import QApplication
    from PyQt4.QtCore import QUrl, QString


    app = QApplication([])

    view = QDeclarativeView()
    rootContext = view.rootContext()
    rootContext.setContextProperty("textData", QString("hi"))
    view.setSource(QUrl("test.qml"))
    view.show()
    app.exec_()

如果将 ``rootContext`` 两行移到 ``view.setSource`` 下面, QML里将找不到对 ``textData`` 的引用


设置变量
^^^^^^^^
如果QML里已经定义好了变量, 而我们就可以在PyQt里对它进行更改, 对QML里的变量更改需要使用 ``PyQt4.QtDeclarative.QDeclarativeView`` 对象的 ``rootObject`` 方法返回的 ``rootObject`` 对象, 调用 ``rootObject`` 对象的 ``setProperty`` 方法即可对QML里变量做出更改

*注意*, 使用 ``rootObject`` 对QML的更改必须在 ``setSource`` 之后, 否则 ``rootObject`` 方法将返回 *None*, 下面是一个例子

test.qml:

.. code-block:: qml

    import Qt 4.7

    Rectangle {
        id: test
        property string textData;
        width: 100; height: 30

        Text {
            anchors.fill:parent;
            text: textData;
        }
    }

test.py:

.. code-block:: python

    from PyQt4.QtDeclarative import QDeclarativeView
    from PyQt4.QtGui import QApplication
    from PyQt4.QtCore import QUrl, QString


    app = QApplication([])

    view = QDeclarativeView()
    view.setSource(QUrl("test.qml"))
    rootObject = view.rootObject()
    rootObject.setProperty("textData", QString("hi"))
    view.show()
    app.exec_()

上面例子我们在QML定义了 ``textData`` 变量, 并在 ``setSource`` 之后使用 ``rootObject`` 的 ``setProperty`` 对 ``textData`` 变量进行了更改


QML信号的传递
-------------
QML有信号机制, 可以在QML之间使用JavaScript进行触发和接收, 当然也可以将信号传递给后端的PyQt, 
我们在此不讨论QML内部的信号, 我们仅讨论QML信号传递到PyQt这部分, QML使用signal创建信号, 信号可以携带参数,
使用调用函数的方法可以触发信号, 下面是一个例子:

test.qml

.. code-block:: qml
    
    import Qt 4.7

    Rectangle {
        id: test;

        signal mclicked;   // 定义信号

        Text {
            anchors.fill:parent;
            text: "Click Me"
        }

        MouseArea {
            onClicked: {
                mclicked();  // 触发信号
            }
        }
    }


上面我们定义了一个 ``mclicked`` 的信号, 并且在点击时会触发这个信号, PyQt 可以通过 ``rootObject`` 获取这个信号, 并为这个信号绑定槽:

test.py

.. code-block:: python

    from PyQt4.QtDeclarative import QDeclarativeView
    from PyQt4.QtGui import QApplication
    from PyQt4.QtCore import QUrl, QString


    app = QApplication([])

    view = QDeclarativeView()
    view.setSource(QUrl("test.qml"))
    def on_click():
        print "hi"

    rootObject = view.rootObject()
    rootObject.mclicked.connect(on_click)
    view.show()
    app.exec_()


上面为QML的 ``mclicked`` 信号绑定了一个函数, 当点击QML窗体时, 控制台就会输出 hi


QML提供接口
-----------
上面我们定义了信号, 如果响应信号仅仅在控制台输出没有意义, 我们可以通过QML在顶级元素定义JavaScript函数
向PyQt提供接口, 下面是例子:

test.qml

.. code-block:: qml
    
    import Qt 4.7

    Rectangle {
        id: test;

        signal mclicked;   // 定义信号

        Text {
            id: testText
            anchors.fill:parent;
            text: "Click Me"
        }

        MouseArea {
            onClicked: {
                mclicked();  // 触发信号
            }
        }

        function set_text(text){
            testText.text = text
        }
    }


上面我们在QML顶级元素定义了一个 ``set_text`` 函数, 接下来我们就可以通过 ``rootObject`` 进行调用

test.py

.. code-block:: python

    from PyQt4.QtDeclarative import QDeclarativeView
    from PyQt4.QtGui import QApplication
    from PyQt4.QtCore import QUrl, QString


    app = QApplication([])

    view = QDeclarativeView()
    view.setSource(QUrl("test.qml"))

    def on_click():
        rootObject.set_text("Clicked")

    rootObject = view.rootObject()
    rootObject.mclicked.connect(on_click)
    view.show()
    app.exec_()

上面我们用响应信号的槽, 通过 ``rootObject`` 调用QML提供的函数接口, 当点击窗体时, 显示文字会从 ``Click Me`` 变成 ``Clicked``,
当然这仅仅是个例子, 这种响应可以在QML里直接完成

.. code-block:: qml
    
    import Qt 4.7

    Rectangle {
        id: test;

        signal mclicked;   // 定义信号

        Text {
            id: testText
            anchors.fill:parent;
            text: "Click Me"
        }

        MouseArea {
            onClicked: {
                mclicked();  // 触发信号
            }
        }

        function set_text(){
            testText.text = "Clicked"
        }

        Component.onCompleted : {
            mclicked.connect(set_text);  // 在加载完成后为mclicked信号绑定槽
        }
    }


一些坑
======
QML全部变量内部状态更改
-----------------------
QML定义使用 ``property`` 定义的变量是全局的, 这些变量是无法对其内部状态进行更改的, 
当然数字和字符串没有这方面问题, 但是数组和对象的更改就不行, 比如下面的更改是无效的

.. code-block:: qml

    import Qt 4.7

    Rectangle {
        id: test;
        property variant testData: {"a":"1", "b": "2"}

        Repeater {
            model:testData;

            Text {
                text: modelData
            }
        }

        MouseArea {
            id: testArea
            anchors.fill;
            onClicked: {
                testData.a = "0"
                testData.b = "1"
            }
        }
    }

上面是无法更改testData的内部状态的, 解决办法是先将全局变量赋值给局部变量, 更改完毕后覆盖全局变量

.. code-block:: qml

    import Qt 4.7

    Rectangle {
        id: test;
        property variant testData: {"a":"1", "b": "2"}

        Repeater {
            model:testData;

            Text {
                text: modelData
            }
        }

        MouseArea {
            id: testArea
            anchors.fill;
            onClicked: {
                var tmp = testData;  // 赋值给局部变量
                tmp.a = "0"
                tmp.b = "1"
                testData = tmp;
            }
        }
    }

上面即可成功更改 ``testData`` 的内部状态


空列表引发的段错误
------------------
从PyQt向QML中渲染数据, 难免会渲染空列表, 但是这在某些平台(Windows)会引发段错误, 搞得人莫名其妙,
解决办法就是将空列表转换为 *None*


一些技巧
========

结合js
------
给元素的属性赋值是可以使用js语句的, 当然复杂的语句可以写成函数,指定这个函数, 使用函数的返回值,
下面是一个隔行变色的例子:

.. code-block:: qml

    import Qt 4.7

    Rectangle {
        width: 100; height: 400;
        Repeater{
            model: [0, 1, 2, 3]
            Rectangle {
                width: 100; height: 100;
                color: index % 2 == 0 ? "blue" : "black"
            }
        }
    }

自定义元素
----------
多处都使用的内容, 可以提取出来放在一个以大写开头的文件名的文件里, 然后同目录使用文件名就可以引用这个元素,
其他目录可以使用 ``import`` 导入这个目录(相对路径导入), 就可以使用目录里的元素了,

比如我在 ``Demo.qml``  定义了一些内容,  同目录或导入这个目录的其他QML文件我就可以直接使用 ``Demo`` 元素了

自定义元素属性
--------------
为了更好的可重用, 定义一些属性是必须的 可以用通过  ``property alias source:target`` 定义属性别名, 当别人引用
这个元素, 并在元素内定义 ``source`` 属性, 就会自动映射到 ``target`` 上, 下面是一个例子

Demo.qml

.. code-block:: qml

    import Qt 4.7

    Rectangle {
        id: demo
        width: 100; height: 50;

        property alias text: demoText.text;

        Text{
            id: demoText;
            text: "Demo"
        }
    }

test.qml

.. code-block:: qml

    import Qt 4.7

    Demo {
        text: "Nice Demo"
    }


使用qmlviwer 查看 test.qml, 会显示 "Nice Demo" 文字

当然也可以直接在自定义元素使用 ``property`` 定义变量, 在使用该元素时定义该属性:


Demo.qml

.. code-block:: qml

    import Qt 4.7

    Rectangle {
        id: demo
        width: 100; height: 50;

        property string textData:"Demo";

        Text{
            id: demoText;
            text: textData 
        }
    }

test.qml

.. code-block:: qml

    import Qt 4.7

    Demo {
        textData: "Nice Demo"
    }
