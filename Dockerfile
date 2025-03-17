FROM fedora:41

RUN dnf install -y \
    cmake \
    ninja-build \
    gcc-c++ \
    clang \
    lld \
    llvm-devel \
    llvm-test \
    python3-lit


WORKDIR workdir

RUN curl https://sh.rustup.rs -sSf | sh -s -- --default-toolchain=1.84.1 -y

ADD ./ ./

# BUILD llvm plugin
RUN rm -rf build
RUN mkdir build && \
    cd build && \
    cmake .. && \
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
