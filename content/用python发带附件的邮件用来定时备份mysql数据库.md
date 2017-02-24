Title: 用python发带附件的邮件用来定时备份mysql数据库
Tags: 附件,邮件,定时,备份,python,pymail,MySQL,mail
Category: Python
Date: 2012-04-21 18:26
最近迁移了wordpress,系统升级为CentOS 6,很奇怪的一个问题,在原来CentOS 5.8下用的很正常的定时备份数据库并通过邮件发送的脚本不能发送附件,其他都正常,邮件内容也是uuencode生成的文件编码,但是就是不产生附件.而且找不出原因,望有知道的不吝赐教.

为了解决这一问题,我用Python写了一个mail客户端,可以发送附件,是一个命令行程序.废话不多说.贴代码:
```python
#!/usr/bin/env python
#-*- coding: utf8 -*-
'''
#=============================================================================
#     FileName: mail.py
#         Desc: To send email
#       Author: cold
#        Email: wh_linux@126.com
#     HomePage: http://www.linuxzen.com
#      Version: 0.0.1
#   LastChange: 2012-04-21 16:37:20
#      History:
#=============================================================================
'''

'''
用于发送邮件,可以发送附件
命令行程序
'''
import smtplib
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
import sys



# 打印帮助信息
def helpinfo():
	print '''
	Useage: pymail -u user@domain -p passwd -h smtp server host -t to who [-a attachment file path] [-n attachment name]
	Useage: email content use . to end
	-h  specify smtp server host
	-u	which user you login the smtp server,and must with it domain
	-p  the password of the smtp user
	-t  The email recipient,multiple addresses can use ',' split
	-a  Add attachment
	-n  Secify attachment name in the email

	Author:cold(wh_linux@126.com)
	Homepge:http://www.linuxzen.com
	'''


# 所有选项
options = ['-t', '-a', '-n', '-h', '-u', '-p', '-s']

# 获取选项长度
argvnum = len(sys.argv)

# 检测命令行参数
for i in range(argvnum):
	if ( i %2 != 0):
		if (sys.argv[i] not in options):
			print 'Unknow option ', sys.argv[i] , ', Please use -h see help!'
			sys.exit(3)

# 如果是-h或者没有命令行参数则显示帮助
try:
	if sys.argv[1] == '-h' or len(sys.argv) == 0:
		helpinfo()
except:
	helpinfo()


# 检测-n参数
if ('-n' in sys.argv) and ('-a' not in sys.argv):
	print 'Error:option "-n" must use after -a'
	sys.exit(2)

# 下面则是获取各个参数内容
try:
	tmpmailto = sys.argv[sys.argv.index('-t')  + 1]
	if ',' in tmpmailto:
		mailto = tmpmailto.split(',')
	else:
		mailto = [tmpmailto,]
except ValueError:
	print 'Error: need Mail Recipient'
	sys.exit(1)

haveattr=True

try:
	attrpath = sys.argv[sys.argv.index('-a') + 1]
	try:
		attrname = sys.argv[sys.argv.index('-n') +1 ]
	except ValueError:
		attrname = attrpath.split('/')[-1]
except:
	attrname = None
	haveattr = False
	attrpath = None


try:
	mail_host = sys.argv[sys.argv.index('-h') +1]
except ValueError:
	print 'Waring: No specify smtp server use 127.0.0.1'
	mail_host = '127.0.0.1'


try:
	mail_useremail = sys.argv[sys.argv.index('-u') +1]
except ValueError:
	print 'Waring: No specify user, use root'
	mail_useremail = 'root@localhost'

try:
	mail_sub = sys.argv[sys.argv.index('-s') + 1]
except:
	mail_sub = 'No Subject'

mail_user = mail_useremail.split('@')[0]
mail_postfix = mail_useremail.split('@')[1]

try:
	mail_pass = sys.argv[sys.argv.index('-p') +1]
except ValueError:
	mail_pass = ''




# 定义邮件发送函数
def send_mail(to_list, sub, content, haveattr, attrpath, attrname):
	me = mail_user + "<" + mail_user+"@"+mail_postfix +">"

	# 判断是否有附件
	if (haveattr):
		if (not attrpath):
			print 'Error : no input file of attachments'
			return False

		# 有附件则创建一个带附件的实例
		msg = MIMEMultipart()

		# 构造附件
		att = MIMEText(open(attrpath, 'rb').read(),'base64', 'utf8')
		att["Content-Type"] = 'application/octest-stream'
		att["Content-Disposition"] = 'attachment;filename="'+ attrname +'"'
		msg.attach(att)
		msg.attach(MIMEText(content))
	else:
		# 无责创建一个文本的实例
		msg = MIMEText(content)


	# 邮件头
	msg['Subject'] = sub
	msg['From'] = me
	msg['To'] = ";".join(to_list)
	try:
		# 发送邮件
		s = smtplib.SMTP()
		s.connect(mail_host)
		if (mail_host != '127.0.0.1'):
			s.login(mail_user, mail_pass)
		s.sendmail(me, to_list, msg.as_string())
		s.close()
		return True
	except Exception, e:
		print str(e)
		return False

if __name__ == '__main__':

	try:
		content = ''
		while True:
			c = raw_input('')
			if c == '.':
				break
			content += c + '\n'
	except EOFError:
		for line in sys.stdin:
			content += line
	if send_mail(mailto, mail_sub, content, haveattr, attrpath, attrname):
		print "Success"
	else:
		print "Failed"

```

将这个脚本保存为pymail放到/usr/bin/下,并赋予其执行权限:
```
chmod +x /usr/bin/pymail
```
可以使用 -h指定smtp发件服务器,默认认为指定-h需要认证, 所以就需要smtp服务器支持认证,同时需要-u指定用户名(需加"@域名"),-p指定密码.
如果不指定-h就会使用本地smtp服务器,默认不需要认证,所以本地的smtp服务器就不能支持认证,同时不需指定-u,-p参数
```
-t 指定收件人多个可用,号分割.
-a 指定附件路径
-n 指定附件名(可省略)
-h 显示帮助信息.
-s 指定邮件主题
```
执行后会要求输入邮件内容,写完用.结束
也可以用管道下面给出几个实例:
```bash
#使用本地smtp服务发送
echo 'linuxzen.com backup' | pymail -s "Linuxzen backup" -t 123456@qq.com -a /tmp/linuxzen.tar.gz
# 使用126邮箱发送
echo 'linuxzen.com backup' | pymail -u linuxzen@126.com -p linuxzen.com -h smtp.126.com -s 'Linuxzen backup " -t 123456@qq.com -a /tmp/linuxzen.tar.gz 

# 不使用管道发送
pymail -u linuxzen@126.com -p linuxzen.com -h smtp.126.com -s 'hello world' -t 123456@qq.com -a /tmp/linuxzen.tar.gz
Hello
this is a test mail
.
```
下面之前使用的mysql定时备份的脚本:
```
#!/bin/bash
export PATH=/usr/kerberos/sbin:/usr/kerberos/bin:/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin:/root/bin
DATE=`date +%Y%m%d`
mysqldump -u root blogdata > /tmp/blogdate."$DATE".sql
cd /tmp
tar -zcf blogdata."$DATE".sql.tar.gz blogdata."$DATE".sql
uuencode blogdata."$DATE".sql.tar.gz blogdata."$DATE".sql.tar.gz | mail -s 'MySQL Backup' 123456@qq.com
```
没有命令uuencode安装sharutils包即可
```
yum -y install sharutils
```
然后使用crontab调用这个脚本定时执行,前面说了 这个脚本在CentOS5.x下正常工作,但是放到CentOS6下就不带附件,所以使用我们自己编写的python脚本脚本内容如下:
```
#!/bin/bash
export PATH=/usr/kerberos/sbin:/usr/kerberos/bin:/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin:/root/bin
DATE=`date +%Y%m%d`
mysqldump -u root blogdata > /tmp/myblog."$DATE".sql
cd /tmp
tar -zcf blogdata."$DATE".sql.tar.gz myblog."$DATE".sql
echo 'MySQL backup' | pymail -u linuzen@126.com -p linuxzen.com -h smtp.126.com -s 'MySQL backup' -a /tmp/blogdata."$DATE".sql.tar.gz -t 123456@qq.com
```
我们使用126邮箱来发送 这样就可以把自带的sendmail 停掉:
```
service sendmail stop
chkonfig --del sendmail
```
