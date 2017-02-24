Title: ssh证书登录错误
Tags: 证书,权限,ssh,authorized_keys
Category: Linux
Date: 2012-08-10 16:11
<h2>错误描述</h2>
使用证书ssh链接的时候提示下面错误信息
<h2><code>Permission denied (publickey,gssapi-keyex,gssapi-with-mic,password).</code>
可能原因</h2>
authorizedkeys 或.ssh的权限太open .ssh 目录改成755 权限 authorizedkeys 改成600
<h2>解决</h2>
查看日志:
<code>cat /var/log/secure</code>
发现
<code>Aug 8 17:15:13 CentOS62 sshd[5624]: Authentication refused: bad ownership or modes for file /home/abc/.ssh/authorized_keys</code>
查看.ssh权限为775
.ssh 手动创建的时候是775权限,改成755权限后正常
<code># chmod 755 ~/.ssh</code>
