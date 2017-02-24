Title: 部署Tornado时iptables引发的的一个问题
Tags: tornado,nginx,iptables,bad gateway,502
Category: Linux
Date: 2012-09-29 19:11
今天在CentOS上部署了一个Tornado,使用nginx做代理,

tornado使用8888,端口,使用nginx作为反向代理,配置文件如下:
```nginx
server {
    listen 80;
    server_name www.linuxzen.com;

    location / {
        proxy_pass_header Server;
        proxy_redirect off;
        proxy_set_header X-Real_IP $remote_addr;
        proxy_set_header X-Scheme $scheme;
        proxy_pass http://127.0.0.1:8888;
    }
}
```
iptables filter表的INPUT链是DROP的,所以添加如下规则:
```bash
iptables -A INPUT -p tcp  -s 127.0.0.1 --dport 8888 -j ACCEPT
```
但是访问nginx总是返回502 Bad Gateway.一开始不认为是防火墙的问题,所以百思不得其解,各种问题试过都不行之后,后来想想tornado返回数据同样也是一条进站,所以还需要添加一条
```bash
iptables -A INPUT -p tcp -s 127.0.0.1 --dport 8888 -j ACCEPT
```
至此,通过nginx才能正常访问tornado
