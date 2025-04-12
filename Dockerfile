FROM fedora:41

RUN dnf install -y \
    clang \
    cmake \
    curl \
    gcc-c++ \
    git \
    gzip \
    lld \
    llvm-devel \
    llvm-test \
    ninja-build \
    python3-lit \
    tar


WORKDIR /workdir

RUN curl https://sh.rustup.rs -sSf | sh -s -- --default-toolchain=1.84.1 -y

ADD ./ ./

# BUILD llvm plugin
RUN rm -rf build
RUN mkdir build && \
    cd build && \
    cmake .. -DCMAKE_BUILD_TYPE=Release && \
    cmake --build . && \
    cd ..

# BUILD rust macro
RUN . "$HOME/.cargo/env" && \
    cd rust_attribute && \
    cargo b

# BUILD rust macro
RUN . "$HOME/.cargo/env" && \
    cd firv2/rust-firv2 && \
    cargo b

# LLVM tests
RUN lit tests -v

# test ripgrep
RUN curl -OL https://github.com/BurntSushi/ripgrep/archive/refs/tags/14.1.1.tar.gz && \
    tar xf 14.1.1.tar.gz && \
    cp rust-toolchain.toml.in ripgrep-14.1.1/rust-toolchain.toml && \
    cp config.toml.in ripgrep-14.1.1/.cargo/config.toml

RUN . "$HOME/.cargo/env" && \
    cd ripgrep-14.1.1 && \
    cargo t 2>/dev/null && \
    cargo t -r 2>/dev/null 
