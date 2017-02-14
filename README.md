## Setup

Modify docker deamon startup options

    $ vim /etc/default/docker

    # Use DOCKER_OPTS to modify the daemon startup options.
    #DOCKER_OPTS="--dns 8.8.8.8 --dns 8.8.4.4"
    DOCKER_OPTS="-H tcp://0.0.0.0:2375"

And export `DOCKER_HOST`

    $ export DOCKER_HOST="tcp://0.0.0.0:2375"

## Using Alpine Linux

For simplicity use the included script to do all the steps below

    $ ./create-image

Build a container with an Erlang development environment

    $ docker build -t build_erlang-dockerwatch --file=alpine/build/Dockerfile .

Build and release the Erlang application to an exported Volume

    $ docker run --rm --volume="$PWD/alpine/run/artifacts:/artifacts" build_erlang-dockerwatch

Build a container with an Erlang runtime enviroment and your application

```Dockerfile
FROM alpine:3.3
# Install the released application
COPY artifacts/dockerwatch /dockerwatch
# Add a non-root user to run the application
RUN addgroup -S dockerwatch && adduser -S -g dockerwatch dockerwatch
RUN chown -R dockerwatch /dockerwatch
USER dockerwatch
# Expose relevant ports
EXPOSE 9898
CMD ["/dockerwatch/bin/dockerwatch", "console"]
```

    $ docker build -t erlang-dockerwatch alpine/run/

What happened?

    $ docker images
    REPOSITORY                 TAG                 IMAGE ID            CREATED             SIZE
    erlang-dockerwatch         latest              cdce0db9fedc        4 seconds ago       19.3 MB
    build_erlang-dockerwatch   latest              3d35f954648f        3 minutes ago       619 MB

## Running the Erlang Application

    $ docker run -d erlang-dockerwatch
    35eacf260f3fbf1d943784ea4e1974487ea62aca3a428d5221dbcd2d4594b251

    $ docker ps
    CONTAINER ID        IMAGE                COMMAND                  CREATED             STATUS              PORTS           NAMES
    35eacf260f3f        erlang-dockerwatch   "/dockerwatch/bin/..."   13 seconds ago      Up 12 seconds       9898/tcp        ecstatic_tesla

Fetch container IP Address from container id:

    $ docker inspect --format='{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' 35eacf260f3f
    172.17.0.2

Test with curl:

    $ curl -i -H "Accept: application/json" http://172.17.0.2:9898
    HTTP/1.1 200 OK
    server: Cowboy
    date: Thu, 16 Feb 2017 15:20:58 GMT
    content-length: 17
    content-type: application/json
    vary: accept
    access-control-allow-origin: *

    {"hello":"world"}
