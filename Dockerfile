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


FROM python:3.11-slim-bookworm as python-base

ENV PYTHONUNBUFFERED=1 \
    # prevents python creating .pyc files
    PYTHONDONTWRITEBYTECODE=1 \
    \
    # pip
    PIP_NO_CACHE_DIR=off \
    PIP_DISABLE_PIP_VERSION_CHECK=on \
    PIP_DEFAULT_TIMEOUT=100 \
    \
    # poetry
    # https://python-poetry.org/docs/configuration/#using-environment-variables
    POETRY_VERSION=1.8.2 \
    # make poetry install to this location
    POETRY_HOME="/opt/poetry" \
    # make poetry create the virtual environment in the project's root
    # it gets named `.venv`
    POETRY_VIRTUALENVS_IN_PROJECT=true \
    # do not ask any interactive question
    POETRY_NO_INTERACTION=1 \
    \
    # paths
    # this is where our requirements + virtual environment will live
    PYSETUP_PATH="/opt/pysetup" \
    VENV_PATH="/opt/pysetup/.venv" \
    # Rust error messages
    RUST_BACKTRACE=1

ENV PATH="$POETRY_HOME/bin:$VENV_PATH/bin:$PATH"


FROM python-base as python-builder

RUN apt-get update \
    && apt-get install --no-install-recommends -y \
        # deps for installing poetry
        curl \
        # deps for building python deps
        build-essential

RUN curl -sSL https://install.python-poetry.org | python3 -

# copy project requirement files here to ensure they will be cached.
WORKDIR $PYSETUP_PATH
COPY poetry.lock pyproject.toml README.md ./
COPY python ./python
COPY --from=rust_binding_builder /work/target/wheels/rust_bindings-0.1.0-cp311-cp311-manylinux_2_17_x86_64.manylinux2014_x86_64.whl .

RUN poetry install -v && \
    poetry run pip install rust_bindings-0.1.0-cp311-cp311-manylinux_2_17_x86_64.manylinux2014_x86_64.whl


FROM python-base as final

WORKDIR /code

COPY --from=swipl:9.2.2 /usr/lib/swipl/ /usr/lib/swipl/
COPY --from=scryer_builder /scryer-prolog/target/release/scryer-prolog /usr/bin
COPY --from=python-builder $PYSETUP_PATH $PYSETUP_PATH

RUN ln -s "/usr/lib/swipl/bin/$(uname -m)-linux/swipl" /usr/bin/swipl && \
    DEBIAN_FRONTEND=noninteractive && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
    libtcmalloc-minimal4 \
    libarchive13 \
    libyaml-0-2 \
    libgmp10 \
    libossp-uuid16 \
    libssl3 \
    ca-certificates \
    libdb5.3 \
    libpcre2-8-0 \
    libedit2 \
    libgeos3.11.1 \
    libspatialindex6 \
    unixodbc \
    odbc-postgresql \
    tdsodbc \
    libmariadbclient-dev-compat \
    libsqlite3-0 \
    libserd-0-0 \
    libpython3.11 \
    libraptor2-0 && \
    dpkgArch="$(dpkg --print-architecture)" && \
    rm -rf /var/lib/apt/lists/*