FROM rust:latest as scryer_builder

RUN git clone https://github.com/mthom/scryer-prolog && \
    cd scryer-prolog && \
    cargo build --release


# === Build rust bindings
FROM ghcr.io/pyo3/maturin as rust_binding_builder

WORKDIR /work
COPY Cargo.* ./
COPY src src
RUN maturin build --release -i 'python3.11'

# === Pull everything together
FROM python:3.11-slim-bullseye as final

WORKDIR /code

COPY --from=swipl:latest /usr/lib/swipl/ /usr/lib/swipl/
COPY --from=scryer_builder /scryer-prolog/target/release/scryer-prolog /usr/bin
COPY --from=rust_binding_builder /work/target/wheels/rust_bindings-0.1.0-cp311-cp311-manylinux_2_17_x86_64.manylinux2014_x86_64.whl .

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
    POETRY_VERSION=1.7 \
    POETRY_VIRTUALENVS_IN_PROJECT=true \
    VENV_PATH="/code/.venv" \
    RUST_BACKTRACE=1



RUN pip install "poetry==$POETRY_VERSION"

COPY pyproject.toml poetry.lock ./
COPY . .

RUN poetry install -v && \
    poetry run pip install rust_bindings-0.1.0-cp311-cp311-manylinux_2_17_x86_64.manylinux2014_x86_64.whl

ENV PATH="$VENV_PATH/bin:$PATH"
