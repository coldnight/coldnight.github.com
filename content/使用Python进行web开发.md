Title: 使用Python进行web开发
Tags: 开发,web.py,Ubuntu,python,Linux
Category: Python
Date: 2012-04-13 16:39
最近有一个小的web项目,想用喜爱都python,但是想到之前接触过都django我感觉一阵不寒而栗,为什么?Django的配置太过复杂,而且小项目不太适合MVC的开发模式,所以我将目光转向了web.py这个小型web框架,并且真正让我动心都是其官方网站上都一句话:"Django lets you write web apps in Django. TurboGears lets you write web apps in TurboGears. Web.py lets you write web apps in Python." —  Adam Atlas

最近切换了Ubuntu替换了Win7系统,所以这里介绍下Ubuntu都安装web.py
### 安装easy_install
```
sudo apt-get install python-pip
```
### 使用easy_install安装web.py
```
sudo easy_install web.py
```
### 测试是否安装成功:
在python shell中执行:
```
import web
```
如果没有报错则web.py安装成功.
下面开始我们第一个hello,world
```python
import web

urls = ("/.*", "hello")                # 指定任何url都指向hello类
app = web.application(urls, globals()) # 绑定url

# 定义相应类
class hello:
def GET(self):
return 'Hello, world!'

if __name__ == "__main__":
app.run()
```
然后保存为`hello.py`并运行它
```
python hello.py
```
然后会看到输出:http://0.0.0.0:8080/

然后浏览器访问:http://localhost:8080即可看到
`Hello, world!`
我们第一个用python写的web程序就建立完成.
