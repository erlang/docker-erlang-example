## Using Alpine Linux

In this example we create a docker image containing a small Erlang
application.

We use the following Dockerfile, containing two build stages:

```Dockerfile
# Build stage 0
FROM erlang:alpine

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
    apk add --no-cache ncurses-libs

# Install the released application
COPY --from=0 /buildroot/dockerwatch/_build/prod/rel/dockerwatch /dockerwatch

# Expose relevant ports
EXPOSE 8080
EXPOSE 8443

CMD ["/dockerwatch/bin/dockerwatch", "foreground"]
```

The image is built with the following command:

    $ docker build -t erlang-dockerwatch .

Notice that if you need a proxy to access the internet, you must
forward your proxy settings to docker, for example by giving the option
`--build-arg https_proxy=$HTTP_PROXY` to `docker build`.

This is what happens:

#### 1. Build stage 0: build

This step starts from the official erlang docker image based on alpine
linux. So with the base image, a full Erlang/OTP installation already
exists.

To be able to build our Erlang application, we now install rebar3.

Our Erlang application is found in the `dockerwatch` directory on our
local filesystem, and we use the `COPY` command to import this
complete directory into the current working directory in the image.

Finally, we release our erlang application using rebar3 and the
following rebar.config.

```erlang
{deps, [{jsone,  "1.4.7"},   %% JSON Encode/Decode
        {cowboy, "2.5.0"}]}. %% HTTP Server

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

#### 2. Build stage 1: create a minimal docker image

Now that our application is released, we can discard all extras that
was needed for compilation. That is, we no longer need rebar3 or the
full Erlang/OTP installation, since our release already contains the
required parts of Erlang/OTP, including the runtime system. So, we
start a new build stage from the apline linux docker image.

Using the `COPY` command with option `--from=0`, we specify that we
want to copy artifacts from build stage 0 into the current build
stage. We use this to copy our released Erlang application into the
final image.

We expose the relevant ports (needed by our application), and specify
the command to execute when running the image.



## What happened?

    $ docker images
	REPOSITORY           TAG                 IMAGE ID            CREATED             SIZE
	erlang-dockerwatch   latest              fcdd1aaa2ee7        16 seconds ago      26MB
	<none>               <none>              c36b2d950fae        21 seconds ago      106MB


## Generating Certificate

Generate certificates in subdirectory `ssl`.

    $ ./create-certs

For some more details of what this command does, see [README-CERTS.md](README-CERTS.md)

## Running the Erlang Application

We start the image in a docker container by issuing the following command.

    $ docker run -d -p 8443:8443 --volume="$PWD/ssl:/etc/ssl/certs" --log-driver=syslog erlang-dockerwatch
    870f979c5b4cdb7a1ba930b020043f50fa7457bf787237706fb27eefaf5fe61d

Let's parse some of the input.

 * `-d`, starts the container in the background and prints the container ID.
 * `-p 8443:8443`, exposes port `8443` from the container to our localhost.
 * `--volume="$PWD/ssl:/etc/ssl/certs"`, mounts our local directory
   (`$PWD/ssl`) with certificates to `/etc/ssl/certs` the container.
 * `--log-driver=syslog`, will log all data from stdout in the container to our local syslog.

In `/var/log/syslog` we can see these entries:

	Nov  7 14:55:42 elxa19vlx02 NetworkManager[1738]: <info>  [1541598942.5025] device (veth2644103): link connected
	Nov  7 14:55:42 elxa19vlx02 NetworkManager[1738]: <info>  [1541598942.5026] device (docker0): link connected
	Nov  7 14:55:43 elxa19vlx02 da217065cb2d[1334]: Exec: /dockerwatch/erts-10.1.1/bin/erlexec -noshell -noinput +Bd -boot /dockerwatch/releases/1.0.0/dockerwatch -mode embedded -boot_var ERTS_LIB_DIR /dockerwatch/lib -config /dockerwatch/releases/1.0.0/sys.config -args_file /dockerwatch/releases/1.0.0/vm.args -pa -- foreground
	Nov  7 14:55:43 elxa19vlx02 da217065cb2d[1334]: Root: /dockerwatch
	Nov  7 14:55:43 elxa19vlx02 da217065cb2d[1334]: /dockerwatch

And here is our docker container:

    $ docker ps
	CONTAINER ID        IMAGE                COMMAND                  CREATED             STATUS              PORTS                              NAMES
	870f979c5b4c        erlang-dockerwatch   "/dockerwatch/bin/do…"   3 minutes ago       Up 2 minutes        8080/tcp, 0.0.0.0:8443->8443/tcp   nifty_heisenberg

Fetch container IP Address from container id:

    $ docker inspect --format='{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' 870f979c5b4c
    172.17.0.2

Create a counter called `cnt` using https with curl:

    $ curl --cacert ssl/dockerwatch-ca.pem -i -H "Content-Type: application/json" -X POST -d "" https://localhost:8443/cnt
    HTTP/1.1 204 No Content
    server: Cowboy
    date: Wed, 22 Feb 2017 13:12:54 GMT
    content-length: 0
    content-type: text/html
    vary: accept

Read all counters using https with curl as json:

    curl --cacert ssl/dockerwatch-ca.pem -H "Accept: application/json" https://localhost:8443
    ["cnt"]

Read the counter `cnt` using https with curl as json:

    curl --cacert ssl/dockerwatch-ca.pem -H "Accept: application/json" https://localhost:8443/cnt
    {"cnt":0}

Increment the counter `cnt` using http with curl:

    curl -H "Content-Type: application/json" -X POST -d '{}' http://172.17.0.2:8080/cnt

Read the counter `cnt` using http with curl as text:

    curl -H "Accept: text/plain" http://172.17.0.2:8080/cnt
    1

Increment the counter `cnt` by 20 using http with curl:

    curl -H "Content-Type: application/json" -X POST -d '{"value":20}' http://172.17.0.2:8080/cnt

Read the counter `cnt` using http with curl as text:

    curl -H "Accept: text/plain" http://172.17.0.2:8080/cnt
    21
