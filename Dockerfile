FROM ubuntu:18.04
MAINTAINER Matt Jones <jonesmr@gmail.com>

RUN apt-get update && apt-get install -y \
    ca-certificates \
    libatomic1 \
    libgmp10 \
    libffi6 \
    libmysqlclient-dev \
    libpcre3-dev \
    libssl1.1 \
    libssl1.0.0 \
    libssl-dev \
    netbase

RUN mkdir -p /deploy/bin && mkdir -p /deploy/static
ADD .cabal-sandbox/bin/WorkoutsMain /deploy/WorkoutsMain
ADD static /deploy/static/

EXPOSE 8000

ENTRYPOINT ["/deploy/WorkoutsMain"]
