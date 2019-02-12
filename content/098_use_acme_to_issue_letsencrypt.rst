通过 acme.sh 获取 Let's Encrypt 免费证书
========================================

:tags: SSL, HTTPS
:date: 2019-02-12
:category: Linux
:author: cold
:status: published
:slug: use-acme-sh-to-issue-letsencrypt-cert


配置 Nginx 正确处理 Webroot 验证
--------------------------------

在证书签发过程中 Let's Encrypt 会验证你拥有当前域名，最基本的方式在你的网站根目录创建一个文件，并通过域名在外部进行请求，如能请求到则认为你拥有该网站的控制权。假设你有一个域名 ``example.com``， 验证步骤大体如下：

1. 通过工具在网站根目录下创建 ``.well-known/acme-challenge/some-random-letters``
2. 工具将创建的路径告知 Let's Encrypt
3. Let's Encrypt 通过路名请求该文件，如 ``http://example.com/.well-known/acme-challenge/some-random-letters``
4. 若能请求到则确认拥有该网站的控制权颁发证书，否则拒绝颁发

为了简化多个域名颁发证书需指定不同的 Webroot，我们可以建立可以将所有域名的验证统一放在一个目录下，并新增一个配置片段供需要启用 HTTPS 的网站引用，新增 ``/etc/nginx/snippets/letsencrypt-acme-challenge.conf`` ，并填充如下内容

.. code-block:: nginx

   #############################################################################
   # Configuration file for Let's Encrypt ACME Challenge location
   #############################################################################
   #
   # This config enables to access /.well-known/acme-challenge/xxxxxxxxxxx
   # on all our sites (HTTP), including all subdomains.
   # This is required by ACME Challenge (webroot authentication).
   # You can check that this location is working by placing ping.txt here:
   # /var/www/letsencrypt/.well-known/acme-challenge/ping.txt
   # And pointing your browser to:
   # http://xxx.domain.tld/.well-known/acme-challenge/ping.txt
   #
   # Sources:
   # https://community.letsencrypt.org/t/howto-easy-cert-generation-and-renewal-with-nginx/3491
   #
   #############################################################################

   # Rule for legitimate ACME Challenge requests (like /.well-known/acme-challenge/xxxxxxxxx)
   # We use ^~ here, so that we don't check other regexes (for speed-up). We actually MUST cancel
   # other regex checks, because in our other config files have regex rule that denies access to files with dotted names.
   location ^~ /.well-known/acme-challenge/ {

       # Set correct content type. According to this:
       # https://community.letsencrypt.org/t/using-the-webroot-domain-verification-method/1445/29
       # Current specification requires "text/plain" or no content header at all.
       # It seems that "text/plain" is a safe option.
       default_type "text/plain";

       # This directory must be the same as in /etc/letsencrypt/cli.ini
       # as "webroot-path" parameter. Also don't forget to set "authenticator" parameter
       # there to "webroot".
       # Do NOT use alias, use root! Target directory is located here:
       # /var/www/common/letsencrypt/.well-known/acme-challenge/
       root         /var/www/letsencrypt;
   }

   # Hide /acme-challenge subdirectory and return 404 on all requests.
   # It is somewhat more secure than letting Nginx return 403.
   # Ending slash is important!
   location = /.well-known/acme-challenge/ {
       return 404;
   }


在需要启用验证的网站的 Nginx 配置中增加如下内容即可

.. code-block:: nginx

   server {
      ...

      include /etc/nginx/snippets/letsencrypt-acme-challenge.conf;
   }

创建相应目录重启 Nginx

.. code-block:: nginx

   sudo mkdir -p /var/www/letsencrypt/
   sudo chown $(whoami).$(whoami) /var/www/letsencrypt/
   sudo /etc/init.d/nginx configtest && sudo /etc/init.d/nginx reload

获取证书
---------

1. 安装 [acme.sh](https://github.com/Neilpang/acme.sh)

   .. code-block:: shell

      curl https://get.acme.sh | sh

2. 获取证书

   .. code-block:: shell

      acme.sh -d example.com -d www.example.com -w /var/www/letsencrypt

安装证书
--------

获取的证书因为在用户家目录下，所以不能直接使用，需要通过如下命令安装到系统中

.. code-block:: shell

   sudo mkdir -p /etc/nginx/certs/example.com
   sudo chown root.$(whoami) /etc/nginx/certs/example.com
   sudo chmod g+w /etc/nginx/certs/example.com
   acme.sh --install-cert -d example.com \
       --cert-file /etc/nginx/certs/example.com/cert.pem \
       --key-file /etc/nginx/certs/example.com/key.pem \
       --fullchain-file /etc/nginx/certs/example.com/fullchain.pem \
       --reloadcmd "service nginx reload"

安装后可以配置 Nginx 启用 HTTPS

.. code-block:: shell

   server {
     listen  80;
     server_name     www.example.com;
     server_name     example.com;
     return         301 https://$server_name$request_uri;
   }

   server{
           listen 443 ssl;
           server_name     www.example.com;
           server_name     example.com;

           ssl_certificate /etc/nginx/certs/example.com/fullchain.pem;
           ssl_certificate_key /etc/nginx/certs/example.com/key.pem;
           ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
           ssl_prefer_server_ciphers on;
           ssl_dhparam /etc/ssl/certs/dhparam.pem;
           ssl_ciphers 'ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:DHE-DSS-AES128-GCM-SHA256:kEDH+AESGCM:ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES128-SHA:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA:ECDHE-ECDSA-AES256-SHA:DHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA256:DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA:DHE-RSA-AES256-SHA:AES128-GCM-SHA256:AES256-GCM-SHA384:AES128-SHA256:AES256-SHA256:AES128-SHA:AES256-SHA:AES:CAMELLIA:DES-CBC3-SHA:!aNULL:!eNULL:!EXPORT:!DES:!RC4:!MD5:!PSK:!aECDH:!EDH-DSS-DES-CBC3-SHA:!EDH-RSA-DES-CBC3-SHA:!KRB5-DES-CBC3-SHA';
           ssl_session_timeout 1d;
           ssl_session_cache shared:SSL:50m;
           ssl_stapling on;
           ssl_stapling_verify on;
           add_header Strict-Transport-Security max-age=15768000;

           access_log      /var/log/nginx/www.example.com.log;

           location / {
                   root    /var/www/html;
                   index   index.html index.htm;
           }

           include /etc/nginx/snippets/letsencrypt-acme-challenge.conf;
   }



参考
-----

- https://community.letsencrypt.org/t/how-to-nginx-configuration-to-enable-acme-challenge-support-on-all-http-virtual-hosts/5622
