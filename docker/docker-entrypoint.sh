#!/bin/bash

set -x

RELX_CONFIG_PATH=/opt/emqdb/config/sys.config
VMARGS_PATH=/opt/emqdb/config/vm.args

export VMARGS_PATH RELX_CONFIG_PATH

# 用户ID/组ID定义
USER_ID=${USER_ID:-1001}
GROUP_ID=${GROUP_ID:-1001}
USERNAME="emqdb"
GROUPNAME="emqdb"

# 创建用户/组
if ! getent group "$GROUPNAME"; then
    groupadd -g "$GROUP_ID" "$GROUPNAME"
fi

if ! id "$USERNAME" &>/dev/null; then
    useradd -u "$USER_ID" -g "$GROUP_ID" -m -s /bin/bash "$USERNAME"
fi

# 创建文件夹
mkdir -p /opt/emqdb/log && chown -R emqdb:emqdb /opt/emqdb

# 前台运行
exec /usr/sbin/gosu emqdb /opt/emqdb/bin/emqdb foreground
