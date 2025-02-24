FROM fedora:41

RUN dnf install -y \
    cmake \
    rustup \
    ninja-build \
    gcc-c++ \
    llvm-devel


WORKDIR workdir

RUN curl https://sh.rustup.rs -sSf | sh -s -- --default-toolchain=1.84.1 -y

ADD ./ ./

RUN mkdir build && \
    cd build && \
    cmake .. && \
    cmake --build . && \
    cd ..

RUN . "$HOME/.cargo/env" && \
    cd rust_attribute && \
    cargo b