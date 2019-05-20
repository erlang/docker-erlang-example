# Build stage 0
FROM erlang:alpine

# Install some libs
RUN apk add --no-cache g++ && \
    apk add --no-cache make

# Install Rebar3
RUN mkdir -p /buildroot/rebar3/bin
ADD https://s3.amazonaws.com/rebar3/rebar3 /buildroot/rebar3/bin/rebar3
RUN chmod a+x /buildroot/rebar3/bin/rebar3

# Setup Environment
ENV PATH=/buildroot/rebar3/bin:$PATH

# Reset working directory
WORKDIR /buildroot

# Copy our Erlang test application
COPY dockerwatch dockerwatch

# And build the release
WORKDIR dockerwatch
RUN rebar3 as prod release


# Build stage 1
FROM alpine

# Install some libs
RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++

# Install the released application
COPY --from=0 /buildroot/dockerwatch/_build/prod/rel/dockerwatch /dockerwatch

# Expose relevant ports
EXPOSE 8080
EXPOSE 8443

CMD ["/dockerwatch/bin/dockerwatch", "foreground"]
