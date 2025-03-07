#!/bin/sh

set -x

RELX_CONFIG_PATH=/opt/emqdb/config/sys.config
VMARGS_PATH=/opt/emqdb/config/vm.args

# 确保日志目录权限
mkdir -p /opt/emqdb/log && chown -R emqdb:emqdb /opt/emqdb/log

export VMARGS_PATH RELX_CONFIG_PATH

# 用户ID/组ID定义
USER_ID=`stat -c '%u' /opt/emqdb/config/sys.config`
GROUP_ID=`stat -c '%g' /opt/emqdb/config/sys.config`
USER_ID=$([ "$USER_ID" = "0" ] && echo -n "1000" || echo -n "$USER_ID")
GROUP_ID=$([ "$GROUP_ID" = "0" ] && echo -n "1000" || echo -n "$GROUP_ID")

# 初始化时创建用户
if id "emqdb" &>/dev/null
then
    echo "found user emqdb"
else
    echo "create user emqdb"
    addgroup -S -g $GROUP_ID eadm
    adduser -S -D -u $USER_ID -G emqdb emqdb
fi

# 创建文件夹
mkdir -p /opt/emqdb/log && chown -R emqdb:emqdb /opt/emqdb

# 前台运行
exec /usr/bin/gosu emqdb /opt/emqdb/bin/emqdb foreground
