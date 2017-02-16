FROM alpine:3.3

# Download the Erlang/OTP source
RUN mkdir /buildroot
WORKDIR /buildroot
ADD https://github.com/erlang/otp/archive/OTP-19.2.tar.gz .
RUN tar zxf OTP-19.2.tar.gz

# Install additional packages
RUN apk add --no-cache autoconf && \
    apk add --no-cache alpine-sdk && \
    apk add --no-cache openssl-dev

# Build Erlang/OTP
WORKDIR otp-OTP-19.2
RUN ./otp_build autoconf && \
    CFLAGS="-Os" ./configure --prefix=/buildroot/erlang/19.2 --without-termcap --disable-hipe && \
    make -j10

# Install Erlang/OTP
RUN mkdir -p /buildroot/erlang/19.2 && \
    make install

# Install Rebar3
RUN mkdir -p /buildroot/rebar3/bin
ADD https://s3.amazonaws.com/rebar3/rebar3 /buildroot/rebar3/bin/rebar3
RUN chmod a+x /buildroot/rebar3/bin/rebar3

# Setup Environment
ENV PATH=/buildroot/erlang/19.2/bin:/buildroot/rebar3/bin:$PATH

# Reset working directory
WORKDIR /buildroot

# Copy our Erlang test application
COPY dockerwatch dockerwatch

WORKDIR dockerwatch

CMD rebar3 as prod release -o /artifacts
