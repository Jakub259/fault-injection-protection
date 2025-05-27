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


WORKDIR /work

RUN curl https://sh.rustup.rs -sSf | sh -s -- --default-toolchain=1.84.1 -y

ADD ./ ./

# BUILD llvm plugin
RUN rm -rf build
RUN mkdir build && \
    cd build && \
    cmake .. -DCMAKE_BUILD_TYPE=Release && \
    cmake --build . && \
    cd ..

# LLVM tests
RUN lit tests -v

# BUILD rust cfip macro
RUN . "$HOME/.cargo/env" && \
    cd rust_attribute && \
    cargo b

# BUILD cfip example
RUN . "$HOME/.cargo/env" && \
    cd examples/cfip_example && \
    cargo r -- 1 2 3 && \
    cargo t && \
    cargo r -r -- 1 2 3 && \
    cargo t -r

# BUILD rust firv2 macro
RUN . "$HOME/.cargo/env" && \
cd firv2/rust-firv2 && \
cargo b

# BUILD firv2 example
RUN . "$HOME/.cargo/env" && \
    cd examples/firv2_example && \
    cargo r -- 1 2 3 && \
    cargo t && \
    cargo r -r -- 1 2 3 && \
    cargo t -r

# test ripgrep
RUN curl -OL https://github.com/BurntSushi/ripgrep/archive/refs/tags/14.1.1.tar.gz && \
    tar xf 14.1.1.tar.gz && \
    cp rust-toolchain.toml.in ripgrep-14.1.1/rust-toolchain.toml && \
    cp config.toml.in ripgrep-14.1.1/.cargo/config.toml

RUN . "$HOME/.cargo/env" && \
    cd ripgrep-14.1.1 && \
    cargo t 2>/dev/null && \
    cargo t -r 2>/dev/null 
