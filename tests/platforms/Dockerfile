FROM ubuntu
RUN apt-get update \
    && apt-get install -y wget xz-utils tar zsh libpcre3 libpcre3-dev gcc
RUN wget https://ziglang.org/builds/zig-linux-x86_64-0.10.0-dev.3685+dae7aeb33.tar.xz
RUN tar -xf zig-linux-x86_64-0.10.0-dev.3685+dae7aeb33.tar.xz
RUN zsh