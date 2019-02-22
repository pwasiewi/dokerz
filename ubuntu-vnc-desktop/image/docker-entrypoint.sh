#!/bin/bash

# change password of a guest user
# PASS=`pwgen -c -n -1 10`
export PASS=packer && echo "guest:$PASS" | sudo chpasswd

cd /usr/lib/web && sudo ./run.py > /var/log/web.log 2>&1 &
sudo nginx -c /etc/nginx/nginx.conf
exec sudo /bin/tini -- /usr/bin/supervisord -n
