FROM python:3.11-slim-bullseye as base


FROM base as builder

ENV PIP_DEFAULT_TIMEOUT=100 \
    PIP_DISABLE_PIP_VERSION_CHECK=1 \
    PIP_NO_CACHE_DIR=1 \
    POETRY_VERSION=1.5.1 \
    POETRY_VIRTUALENVS_IN_PROJECT=true \
    VENV_PATH="/code/.venv"

WORKDIR /code

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libtcmalloc-minimal4 \
    libarchive13 \
    libyaml-dev \
    libgmp10 \
    libossp-uuid16 \
    libssl1.1 \
    ca-certificates \
    libdb5.3 \
    libpcre2-8-0 \
    libedit2 \
    libgeos-3.9.0 \
    libspatialindex6 \
    unixodbc \
    odbc-postgresql \
    tdsodbc \
    libmariadbclient-dev-compat \
    libsqlite3-0 \
    libserd-0-0 \
    libraptor2-0 && \
    dpkgArch="$(dpkg --print-architecture)" && \
    rm -rf /var/lib/apt/lists/*

ENV LANG C.UTF-8

RUN apt-get update && \
    apt-get -y upgrade && \
    SWIPL_VER=9.0.4; \
    SWIPL_CHECKSUM=feb2815a51d34fa81cb34e8149830405935a7e1d1c1950461239750baa8b49f0; \
    BUILD_DEPS='make cmake ninja-build gcc g++ wget git autoconf libarchive-dev libgmp-dev libossp-uuid-dev libpcre2-dev libreadline-dev libedit-dev libssl-dev zlib1g-dev libdb-dev unixodbc-dev libsqlite3-dev libserd-dev libraptor2-dev libgeos++-dev libspatialindex-dev libgoogle-perftools-dev libgeos-dev libspatialindex-dev'; \
    apt-get update; apt-get install -y --no-install-recommends $BUILD_DEPS; rm -rf /var/lib/apt/lists/*; \
    mkdir /tmp/src && cd /tmp/src; \
    wget -q https://www.swi-prolog.org/download/stable/src/swipl-${SWIPL_VER}.tar.gz; \
    echo "$SWIPL_CHECKSUM  swipl-$SWIPL_VER.tar.gz" >> swipl-$SWIPL_VER.tar.gz-CHECKSUM; \
    sha256sum -c swipl-$SWIPL_VER.tar.gz-CHECKSUM; \
    tar -xvf swipl-${SWIPL_VER}.tar.gz; \
    mkdir swipl-$SWIPL_VER/build; \
    cd swipl-$SWIPL_VER/build; \
    cmake -DCMAKE_INSTALL_PREFIX=/usr/ -G Ninja ..; \
    ninja; \
    ninja install; \
    rm -rf /tmp/src; \
    apt-get purge -y --auto-remove $BUILD_DEPS

RUN pip install "poetry==$POETRY_VERSION"

COPY pyproject.toml poetry.lock ./
COPY . .

RUN poetry install -v

ENV PATH="$VENV_PATH/bin:$PATH"
