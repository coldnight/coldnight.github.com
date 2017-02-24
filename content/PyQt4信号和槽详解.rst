PyQt4 信号和槽详解
##################
:tags: PyQt4, 信号, 槽, Qt, Python, GUI
:date: 2013-09-22 10:10
:category: PyQt
:author: cold
:summary:
    最近在开发一个基于 ``PyQt4`` 的的 GUI程序, 使用过程中一些对信号和槽的理解分享给大家


Python 可以开发GUI有很多库可以选择, 之前使用过 ``wxPython``, 已经很长时间没用过, 基本都忘光了, 由于单位也使用 ``PyQt`` , 之前熟悉项目时看过一段时间, 但是由于没有实际项目经验, 所以 ``PyQt`` 很生疏, 最近正好给家里写个小的财务软件练练手. 在用的过程中对 ``PyQt`` 熟悉不少, 之前觉得 ``PyQt`` 最难掌握的就是 ``信号`` 和 ``槽`` , 所以现在先来讨论这个


概览
====
信号和槽可以说是 ``Qt`` 的精髓所在, 有些类似 Javascript 的事件响应机制, 所以咱先以 ``JQuery`` 来做个示例, 在 ``JQuery`` 中我们获取一个元素, 并且绑定一个事件, 传递一个匿名函数来响应该事件

.. code-block:: html

    <!-- 需导入 jquery 库 -->
    <button id="test">点我</button>
    <script type="text/javascript">
        $("#test").click(function(){
            alert("点疼我了!!!");
        )};
    </script>

以上代码, 当点击按钮 ``点我`` 时会弹出一个对话框, 这就是响应了点击事件.

信号和槽有着类似的机制, ``QObject.connect`` 可以连接一个 ``QObject`` 的信号到另一个 ``QObject`` 的槽, ``PyQt`` 同时可以将信号连接到一个 ``callback``

.. code-block:: python

    from PyQt4 import QtGui, QtCore

    app = QtGui.QApplication([])

    w = QtGui.QWidget()

    def showMsg():
        QtGui.QMessageBox.information(w, u"信息", u"ok")

    btn = QtGui.QPushButton(u"点我", w)
    w.connect(btn, QtCore.SIGNAL("clicked()"), showMsg)

    w.show()
    app.exec_()

上面例子将一个 button 对象的 ``clicked()`` 信号连接到 ``showMsg`` 函数. 也就是说 ``ShowMsg`` 响应了一个按钮的点击事件.

PyQt 有一种类似 ``JQuery`` 响应事件的方式, 来连接信号和槽

.. code-block:: python

    from PyQt4 import QtGui, QtCore

    app = QtGui.QApplication([])

    w = QtGui.QWidget()

    def showMsg():
        QtGui.QMessageBox.information(w, u"信息", u"ok")

    btn = QtGui.QPushButton(u"点我", w)

    btn.clicked.connect(showMsg)   # XXX 此处与上面例子不同

    app.exec_()


信号
====
要想了解信号的本质, 我们需要了解信号的创建和 ``QObject.emit`` 方法, ``emit`` 方法用来发射信号

定义信号
--------
``PyQt4.QtCore.pyqtSignal`` 函数可以为 ``QObject`` 创建一个信号

.. code-block:: python

    from PyQt4 import QtGui, QtCore

    class MyButton(QtGui.QPushButton)
        myclicked = QtCore.pyqtSignal()

上面例子为 ``MyButton`` 创建了一个 ``myclicked()`` 的信号


带参数的信号
------------
信号可以携带参数, 并在发射信号时携带传递给槽

.. code-block:: python

    from PyQt4 import QtGui, QtCore

    class MyButton(QtGui.QPushButton)
        myclicked = QtCore.pyqtSignal(int)


上面例子定义了 ``myclicked(int)`` 信号, 可以携带一个, 发射时信号时可以携带一个整数


发射信号
--------
为了发射我们自定义的信号, 我们对 ``QPushButton`` 进行一下封装, 自动绑定 ``clicked()`` 信号, 并发射自定义的信号

.. code-block:: python

    from PyQt4 import QtGui, QtCore

    class MyButton(QtGui.QPushButton):
        myclicked = QtCore.pyqtSignal()

        def __init__(self, *args, **kwargs):
            QtGui.QPushButton.__init__(self, *args, **kwargs)

            self.connect(self, QtCore.SIGNAL("clicked()"), self.myclicked.emit)

    app = QtGui.QApplication([])

    w = QtGui.QWidget()

    def showMsg():
        QtGui.QMessageBox.information(w, u"信息", u"ok")

    btn = MyButton(u"点我", w)
    w.connect(btn, QtCore.SIGNAL("myclicked()"), showMsg)

    w.show()
    app.exec_()


上面我们封装了 ``QPushButton`` 让他在收到点击信号时同时发送 ``myclicked()`` 信号.

我们也可以不定义信号, 直接发送信号, 上面的例子也可以这么写

.. code-block:: python

    from PyQt4 import QtGui, QtCore

    class MyButton(QtGui.QPushButton):
        def __init__(self, *args, **kwargs):
            QtGui.QPushButton.__init__(self, *args, **kwargs)

            self.connect(self, QtCore.SIGNAL("clicked()"), self.emitClicked)

        def emitClicked(self):
            self.emit(QtCore.SIGNAL("myclicked()"))


    app = QtGui.QApplication([])

    w = QtGui.QWidget()

    def showMsg():
        QtGui.QMessageBox.information(w, u"信息", u"ok")


    btn = MyButton(u"点我", w)
    w.connect(btn, QtCore.SIGNAL("myclicked()"), showMsg)
    w.show()

    app.exec_()

上面例子我们没有定义信号, 仅仅是在响应 ``clicked()`` 信号的函数内直接发送 ``myclicked()`` 信号

发射带参数的信号
----------------
有时我们展示了一个列表, 并想提供查看某项列表的详细内容, 我们会在列表项的末端加一个查看按钮, 这时我们如何在按按钮的时候得知这是那一项呢? 这时就需要带参数的信号,  信号是可以带参数的, 参数会在信号发送时携带, 并传递给接收此信号的槽

.. code-block:: python

    from PyQt4 import QtGui, QtCore

    class MyButton(QtGui.QPushButton):
        myclicked = QtCore.pyqtSignal(int)

        def __init__(self, _id, *args, **kwargs):
            QtGui.QPushButton.__init__(self, *args, **kwargs)

            self._id = _id

            self.connect(self, QtCore.SIGNAL("clicked()"), self.emitMyclicked)

        def emitMyclicked(self):
            self.myclicked.emit(self._id)


    app = QtGui.QApplication([])

    w = QtGui.QWidget()
    w.resize(100, 100)

    def showMsg(_id):
        QtGui.QMessageBox.information(w, u"信息", u"查看 %d" % _id)


    btn = MyButton(1, u"查看1", w)
    w.connect(btn, QtCore.SIGNAL("myclicked(int)"), showMsg)

    btn2 = MyButton(2, u"查看2", w)
    btn2.move(0, 30)
    w.connect(btn2, QtCore.SIGNAL("myclicked(int)"), showMsg)

    w.show()
    app.exec_()

上面例子可以看出, ``QObject.emit`` 发送带参数的信号时要携带参数. 当然上面例子也可以用下面方式来写

.. code-block:: python

    from PyQt4 import QtGui, QtCore

    class MyButton(QtGui.QPushButton):
        def __init__(self, _id, *args, **kwargs):
            self._id = _id
            QtGui.QPushButton.__init__(self, *args, **kwargs)

            self.connect(self, QtCore.SIGNAL("clicked()"), self.emitClicked)

        def emitClicked(self):
            self.emit(QtCore.SIGNAL("myclicked(int)"), self._id)


    app = QtGui.QApplication([])

    w = QtGui.QWidget()
    w.resize(100, 100)

    def showMsg(_id):
        QtGui.QMessageBox.information(w, u"信息", u"查看 %d" % _id)


    btn = MyButton(1, u"查看1", w)
    w.connect(btn, QtCore.SIGNAL("myclicked(int)"), showMsg)

    btn2 = MyButton(2, u"查看2", w)
    btn2.move(0, 30)
    w.connect(btn2, QtCore.SIGNAL("myclicked(int)"), showMsg)
    w.show()

    app.exec_()


槽
==
我一开始学习 PyQt 的信号和槽的时候看到那个 `滑块的例子 <http://jimmykuu.sinaapp.com/static/PyQt4_Tutorial/html/events_and_signals.html#id1>`_  一直搞的我很迷糊, 不知所以, 也没学会怎么用.
这里咱就自己创建一个槽, 就能了解什么是槽和槽该怎么用.

上面我们一直使用 ``函数`` (callback)作为槽, 下面我们来介绍使用 `真正` 的槽

创建槽
------
``QtCore.pyqtSlot`` 函数返回一个 ``装饰器`` 用于装饰 ``QObject`` 的方法, 使之成为一个槽(我开始一直以为 ``QObject`` 的一个方法就是一个槽 囧rz), 下面例子我们创建一个槽

.. code-block:: python

    from PyQt4 import QtGui, QtCore

    class MainWidget(QtGui.QWidget):
        def __init__(self):
            QtGui.QWidget.__init__(self)


            btn = QtGui.QPushButton(u"点我", self)

            self.connect(btn, QtCore.SIGNAL("clicked()"), self,
                         QtCore.SLOT("onClicked()"))

        @QtCore.pyqtSlot()
        def onClicked(self):
            QtGui.QMessageBox.information(self, u"信息", u"由槽弹出")


    app = QtGui.QApplication([])
    m = MainWidget()
    m.show()
    app.exec_()

上面例子我们为 ``MainWidget`` 创建了一个槽, 并将 ``btn`` 的 ``clicked()`` 信号连接到这个槽


本文所有例子都是经过测试可以运行的, 所以大家如果对信号和槽还是有点迷糊不妨将例子中的代码敲一下并运行, 建议改改例子, 解答自己的疑惑
