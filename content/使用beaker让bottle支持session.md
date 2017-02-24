Title: 使用beaker让bottle支持session
Tags: web,session,python,bottle,beaker
Category: Python
Date: 2012-05-26 11:38
bottle是一个小型web框架,很小只有一个文件,但功能确很强大,学起来也简单,简单和小巧的同时也有很多不足,某些功能支持还不是很完善,比如session.但是也有它自身的好处,我们可以自己或使用别的模块来扩展它,不像django,很强大,但是想要进一步扩展的时候确无从下手.我们可以把非常简单而强大的bottle自己动手将它变得更加强大和完善.

bottle小巧支持cookie但是不支持session.为了安全起见我们有时候希望使用的session.我们可以使用中间件beaker来扩展bottle,使我们的bottle应用支持session.废话不多说.首先beaker不是内置模块,我们首先来安装它.当然你可以网上下包手动安装,我们使用最简单的:
```bash
easy_install beaker
```
没有easy_install这个命令?google吧,装了之后还是没有,如过时win的话检查环境变量,将Python安装目录下的Scripts目录添加到环境变量.

安装好后我们如何使用它,下面一段带面是使用的:
```python
#!/usr/bin/env python
from bottle import route, default_app, run, request
from beaker.middleware import SessionMiddleware

session_opts = {
                'session.type':'file',
                'session.cookei_expires':300,
                'session.data_dir':'./sessions',
                'sessioni.auto':True
                }

@route('/test')
def test():
    s = request.environ.get('beaker.session')
    s['test'] = s.get('test', 0) + 1
    s.save()
    return 'Test conter: %d' % s['test']

app = default_app()
app = SessionMiddleware(app, session_opts)
run(app=app)
```
运行这段代码,会提示:
```
Bottle server starting up (using WSGIRefServer())...
Listening on http://127.0.0.1:8080/
Hit Ctrl-C to quit.
```
现在打开浏览器访问`http://127.0.0.1:8080/test`

不断刷新就会发现数值不断在增大.说明我们的session已经正常工作了
