## Using Logstash to collect log events

This example runs a Logstash instance which takes `gelf` messages as
input on UDP port `44622`.

Build the image with

	docker build -t logstash logstash/

Run it with

	docker run --rm -p 44622:44622/udp logstash

### Configuration

The Logstash pipeline configuration is specified in
`logstash/pipeline/logstash.conf`.


```
input {
    gelf {
        use_udp => true
        port => 44622
    }
}
filter {
    # If a log message can be parsed as json, do so, and populate the
    # log event with fields found.
    json {
        skip_on_invalid_json => "true"
        source => "message"
    }
    # Convert the level field to an integer
    mutate {
        convert => {
            "level" => "integer"
        }
    }
}
output {
    stdout {
       }
    file {
        path => "/usr/share/logstash/logs/output.log"
    }
}
```

### Run the example

Build the docker-erlang-example image:

	docker build -t dockerwatch .

To forward log events (single line printouts to STDOUT) from the
docker-erlang-example image into the Logstash container, use the
`gelf` log driver and specify the UDP port number, for example:

	docker run -p 8443:8443 --volume="$PWD/ssl:/etc/ssl/certs" --log-driver=gelf --log-opt gelf-address=udp://0.0.0.0:44622 dockerwatch

In general, a single line printout from Erlang's Logger can for
instance be:

    2018-11-12T15:32:20.284863+00:00 notice: Hello world

When forwarded to Logstash as is, this will result in the following
log event:

```
{
          "image_id" => "sha256:63bb61c199d0d649d178cfdbedfc88e6253f24e534f09fd15c3ef79302931ed0",
           "command" => "/dockerwatch/bin/dockerwatch console",
           "version" => "1.1",
       "source_host" => "172.17.0.1",
      "container_id" => "a83600195dd0867c5996ea625d7f7c5fc1b87a97043c649a7cb0ed52058d75f1",
               "tag" => "a83600195dd0",
             "level" => 6,
        "image_name" => "erlang-dockerwatch",
        "@timestamp" => 2018-11-12T15:32:20.463Z,
    "container_name" => "epic_chebyshev",
           "message" => "2018-11-12T15:32:20.284863+00:00 notice: Hello world\r",
              "host" => "elxa19vlx02",
           "created" => "2018-11-12T15:12:55.198308117Z",
          "@version" => "1"
}
```

Notice that the message itself includes a timestamp, but it is not
exactly the same as the `@timestamp` field of the log event. Also, the
`level` field of the log event is `6` (info), while the level of the
actual message was `notice` (5). To overcome this, and popluate the
Logstash log event with the real values from a logger event, we can
format the log event as json and let Logstash parse it (enabled by the
json filter specified in logstash.conf). The following sys.config can
be used:

```
[{kernel, [{logger,
            [%% Set formatter template to print jason
             {handler,default,logger_std_h,
              #{formatter=>
                    {logger_formatter,
                     #{template=>
                           ["{ \"@timestamp\": \"",time,
                            "\", \"level\": \"",{level_int,[level_int],[level]},
                            "\", \"message\": \"",msg,"\" }\n"]}}}},

             %% Add a primary filter to insert 'level_int' field in metadata
             {filters,log,
              [{level_int,{fun dockerwatch_filter:add_level_int/2,[]}}]},

             %% Set log level 'debug' for module dockerwatch - to get
             %% some log printouts when sending requests
             {module_level,debug,[dockerwatch]}
            ]}
          ]}
].
```

The log printout from Logger will now be:

    { "@timestamp": "2018-11-12T15:41:05.531172+00:00", "level": "5", "message": "Hello world" }

And the Logstash log event will be:

```
{
          "image_id" => "sha256:63bb61c199d0d649d178cfdbedfc88e6253f24e534f09fd15c3ef79302931ed0",
           "command" => "/dockerwatch/bin/dockerwatch console",
           "version" => "1.1",
       "source_host" => "172.17.0.1",
      "container_id" => "a83600195dd0867c5996ea625d7f7c5fc1b87a97043c649a7cb0ed52058d75f1",
               "tag" => "a83600195dd0",
             "level" => 5,
        "image_name" => "erlang-dockerwatch",
        "@timestamp" => 2018-11-12T15:41:05.531Z,
    "container_name" => "epic_chebyshev",
           "message" => "Hello world",
              "host" => "elxa19vlx02",
           "created" => "2018-11-12T15:12:55.198308117Z",
          "@version" => "1"
}
```

So, instead of including all information in the `message` field, the
log event is populated with the actual timestamp, level and message as
specified by the logging client.

NOTE: A currently necessary trick to get the level correct is to
include a `level_int` field in the logger metadata, allowing the level
to be represented as an integer. This is done by the primary logger
filter specified in sys.config.

The docker-erlang-example is modified to issue a log event for each
REST request it receives. Using the curl example lines from the simple
example will therefore trigger a few events which can be seen on
stdout in the logstash container.

Logstash also prints all events to a file (see output plugin file in
logstash.conf). This is done mainly for the Travis build - which
checks the number of lines in this file.
