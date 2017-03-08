## Docker Cheatsheet

* Remove all containers that are not running:

        $ docker rm $(docker ps -aq -f status=exited)

* Remove dangling images:

        $ docker rmi $(docker images -f dangling=true -q)

* Attach to running docker:

        $ docker exec -i -t NameOrId /bin/sh

## Core generation

  * `/proc/sys/core_pattern` is clearly persisted on the host. Taking note of
    its content before starting any endeavour is therefore highly encouraged.
  * dockers `--privileged` is necessary for a gdb session to catch the stack,
    without privileges, gdb just complains about No stack. Google still is
    hardly knowledgeable about this phenomenon...
  * setting ulimit on docker run works perfectly, for future googlers (syntax hard to find),
    a docker-compose example:

    ulimits:
          core: -1
