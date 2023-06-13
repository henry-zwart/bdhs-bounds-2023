FROM rust:latest as scryer_builder

RUN git clone https://github.com/mthom/scryer-prolog && \
    cd scryer-prolog && \
    cargo build --release

FROM python:3.11-slim-bullseye as final

COPY --from=swipl:latest /usr/lib/swipl/ /usr/lib/swipl/
COPY --from=scryer_builder /scryer-prolog/target/release/scryer-prolog /usr/bin

RUN ln -s "/usr/lib/swipl/bin/$(uname -m)-linux/swipl" /usr/bin/swipl && \
    apt-get update && \
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

ENV PIP_DEFAULT_TIMEOUT=100 \
    PIP_DISABLE_PIP_VERSION_CHECK=1 \
    PIP_NO_CACHE_DIR=1 \
    POETRY_VERSION=1.5.1 \
    POETRY_VIRTUALENVS_IN_PROJECT=true \
    VENV_PATH="/code/.venv" \
    RUST_BACKTRACE=1

WORKDIR /code

RUN pip install "poetry==$POETRY_VERSION"

COPY pyproject.toml poetry.lock ./
COPY . .

RUN poetry install -v

ENV PATH="$VENV_PATH/bin:$PATH"
