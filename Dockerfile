FROM --platform=$BUILDPLATFORM erlang:latest AS builder

# 设置与GitHub Actions相同的构建路径
WORKDIR /home/runner/work/emqdb/emqdb

COPY . .

RUN apt-get update && apt-get install -y \
    build-essential \
    git \
    curl \
    cmake \
    libssl-dev \
    zlib1g-dev
RUN rm -rf _build && \
    rebar3 as prod release

FROM --platform=$BUILDPLATFORM debian:bookworm-slim

ARG DOCKER_IMAGE_VERSION

ENV \
    DISABLE_IPV6=1 \
    TZ='Asia/Shanghai' \
    LANG='zh_CN.UTF-8'

WORKDIR /opt/emqdb

RUN apt-get update && apt-get install -y \
    libncurses6 \
    libgcc-s1 \
    libstdc++6 \
    openssl \
    dumb-init \
    tzdata \
    gosu \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

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
