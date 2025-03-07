#!/usr/bin/env bash
# docker build script

# 提前拉取镜像（反复打包测试时免除每次pull）
docker pull alpine:3.21
docker pull erlang:latest

# 清理镜像
docker stop emqdb
docker rm emqdb
docker rmi emqdb

# 清理build缓存
# docker builder prune -a

# 清理逻辑卷
# docker system prune -a

# 打包
# docker build --no-cache -t emqdb .
docker build -t emqdb .

# 运行
docker run -itd --name emqdb -p 8080:8090 emqdb

# 查看日志
docker logs -n 100 emqdb
