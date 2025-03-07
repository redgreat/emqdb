FROM --platform=$BUILDPLATFORM erlang:latest AS builder

WORKDIR /emqdbuild

COPY . .

# RUN echo "deb http://mirrors.aliyun.com/debian/ bookworm main non-free contrib" > /etc/apt/sources.list \
#    && echo "deb http://mirrors.aliyun.com/debian-security/ bookworm-security main" >> /etc/apt/sources.list

RUN apt-get update && apt-get install -y \
    build-essential \
    git \
    curl \
    bash \
    cmake \
    libssl-dev \
    zlib1g-dev
RUN rebar3 as prod release

FROM --platform=$BUILDPLATFORM alpine:3.21

ARG DOCKER_IMAGE_VERSION

ENV \
    DISABLE_IPV6=1 \
    TZ='Asia/Shanghai' \
    LANG='zh_CN.UTF-8'

WORKDIR /opt/emqdb

RUN apk add --no-cache --update \
    ncurses-libs \
    libgcc \
    libstdc++ \
    dumb-init \
    tzdata \
    && apk add --no-cache --repository https://dl-cdn.alpinelinux.org/alpine/edge/testing/ gosu

COPY --from=builder /emqdbuild/_build/prod/rel/emqdb /opt/emqdb/
COPY --from=builder /emqdbuild/docker/docker-entrypoint.sh /opt/emqdb/docker/docker-entrypoint.sh

RUN chmod +x /opt/emqdb/docker/docker-entrypoint.sh

VOLUME /opt/emqdb

EXPOSE 8091

LABEL \
    org.label-schema.name="emqdb" \
    org.label-schema.description="eqm消费存入postgres数据库" \
    org.label-schema.version="${DOCKER_IMAGE_VERSION:-unknown}" \
    org.label-schema.vcs-url="https://github.com/redgreat/emqdb" \
    org.label-schema.maintainer="wangcw <rubygreat@msn.com>" \
    org.label-schema.schema-version="1.0"

ENTRYPOINT ["/usr/bin/dumb-init", "-c", "--", "/opt/emqdb/docker/docker-entrypoint.sh"]
