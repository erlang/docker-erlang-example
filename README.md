## Using Alpine Linux

For simplicity use the included script to do all the steps below

    $ ./create-image

#### 1. Build a container with an Erlang development environment

```Dockerfile
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
VOLUME /artifacts

CMD rebar3 as prod release -o /artifacts
```
    $ docker build -t build_erlang-dockerwatch --file=alpine/build/Dockerfile .

#### 2. Build and release the Erlang application to an exported Volume

    $ docker run --rm --volume="$PWD/alpine/run/artifacts:/artifacts" build_erlang-dockerwatch

This releases our Erlang application using rebar3 and the following rebar.config.

```erlang
{deps, [{lager,  "3.2.4"},   %% Logging
        {jsone,  "1.4.2"},   %% JSON Encode/Decode
        {cowboy, "1.1.2"}]}. %% HTTP Server

{relx, [{release, {"dockerwatch", "1.0.0"}, [dockerwatch]},
        {vm_args, "config/vm.args"},
        {sys_config, "config/sys.config"},
        {dev_mode, true},
        {include_erts, false},
        {extended_start_script, true}
    ]}.

{profiles, [{prod, [{relx, [{dev_mode, false},
                            {include_erts, true},
                            {include_src, false}]}]}
           ]}.
```

#### 3. Build a container with an Erlang runtime enviroment and your application

```Dockerfile
FROM alpine:3.3

# Install the released application
COPY artifacts/dockerwatch /dockerwatch

# Expose relevant ports
EXPOSE 9898

CMD ["/dockerwatch/bin/dockerwatch", "foreground"]
```

    $ docker build -t erlang-dockerwatch alpine/run/

What happened?

    $ docker images
    REPOSITORY                 TAG                 IMAGE ID            CREATED             SIZE
    erlang-dockerwatch         latest              ca683d321e7e        6 minutes ago       21.8 MB
    build_erlang-dockerwatch   latest              8059ec18fa6e        19 minutes ago      617 MB


## Generating Certificate

Generate certificates in subdirectory `ssl`.

    $ ./create-certs

For some more details of what this command does, wee [README-CERTS.md](README-CERTS.md)

## Running the Erlang Application

We start the image in docker container by issuing the following command.

    $ docker run -d -p 8443:8443 --volume="$PWD/ssl:/dockerwatch/lib/dockerwatch-1.0.0/priv/ssl" --log-driver=syslog erlang-dockerwatch
    870f979c5b4cdb7a1ba930b020043f50fa7457bf787237706fb27eefaf5fe61d

Let's parse some of the input.

 * `-p 8443:8443`, exposes port `8443` from the container to our localhost
 * `--volume="$PWD/ssl:/dockerwatch/lib/dockerwatch-1.0.0/priv/ssl"`, mounts our local directory with certificates in
   the container.
 * `--log-driver=syslog`, will log all data from stdout in the container to our local syslog.

Output in syslog.

    $ tail -n10 /var/log/syslog
    Feb 21 11:55:17 elxd1168lx9 kernel: [692274.686524] docker0: port 1(vethb1adc6d) entered forwarding state
    Feb 21 11:55:17 elxd1168lx9 870f979c5b4c[3837]: Exec: /dockerwatch/erts-8.2/bin/erlexec -noshell -noinput +Bd -boot /dockerwatch/releases/1.0.0/dockerwatch -mode embedded -boot_var ERTS_LIB_DIR /dockerwatch/lib -config /dockerwatch/releases/1.0.0/sys.config -args_file /dockerwatch/releases/1.0.0/vm.args -pa -- foreground
    Feb 21 11:55:17 elxd1168lx9 870f979c5b4c[3837]: Root: /dockerwatch
    Feb 21 11:55:17 elxd1168lx9 870f979c5b4c[3837]: /dockerwatch
    Feb 21 11:55:17 elxd1168lx9 870f979c5b4c[3837]: 10:55:17.463 [info] Application lager started on node dockerwatch@870f979c5b4c#015
    Feb 21 11:55:17 elxd1168lx9 870f979c5b4c[3837]: 10:55:17.464 [info] Application ranch started on node dockerwatch@870f979c5b4c#015
    Feb 21 11:55:17 elxd1168lx9 870f979c5b4c[3837]: 10:55:17.464 [info] Application cowboy started on node dockerwatch@870f979c5b4c#015
    ...

    $ docker ps
    CONTAINER ID        IMAGE                COMMAND                  CREATED             STATUS              PORTS                    NAMES
    870f979c5b4c        erlang-dockerwatch   "/dockerwatch/bin/..."   About a minute ago   Up About a minute  0.0.0.0:8443->8443/tcp   mystifying_thompson

Fetch container IP Address from container id:

    $ docker inspect --format='{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' 870f979c5b4c
    172.17.0.2

Test with curl:

    $ curl --cacert ssl/dockerwatch-ca.pem -i -H "Accept: application/json" https://localhost:8443
    HTTP/1.1 200 OK
    server: Cowboy
    date: Tue, 21 Feb 2017 10:57:20 GMT
    content-length: 17
    content-type: application/json
    vary: accept
    access-control-allow-origin: *
    
    {"hello":"world"}
