# emqx消息存入PostgreSQL/Oracle数据库

## 运行
```shell
docker run -itd \
-m 1G \
--memory-reservation 500M \
--memory-swappiness=0 \
-oom-kill-disable \
--cpu-shares=0 \
--restart=always \
- ./config/sys.config:/opt/emqdb/config/sys.config
- ./config/vm.args:/opt/emqdb/config/vm.args
- ./log/:/opt/emqdb/log/:rw
-p 8081:8091 \
--name eadm redgreat/emqdb
```
