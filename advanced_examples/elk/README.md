## Using Logstash, Elasticsearch and Kibana, a.k.a. the ELK stack

This example runs four docker containers:

- dockerwatch (our example application)
- logstash (log pipeline)
- elasticsearch (search and analytics engine)
- kibana (analytics and visualization platform, web interface)

As in the
[Logstash example](https://github.com/erlang/docker-erlang-example/tree/master/advanced_examples/logstash),
the dockerwatch container is started with a logging driver that sends
everything printed on standard out on to a UDP port in the logstash
container. Logstash forwards each log event over http to the
elasticsearch container, and kibana collects log events from
elasticsearch.

This setup, using Logstash, Elasticsearch and Kibana, is a quite
common open source solution for collecting and visualizing log events
from any number of sources. It is known as the Elastic Stack or the
ELK stack, https://www.elastic.co/elk-stack.

In the example, we use
[`docker-compose`](https://docs.docker.com/compose/) to define and run
the containers. This also simplifies addressing between the
containers.


### Prerequisites

Install [`docker-compose`](https://docs.docker.com/compose/install/).



### Run the example

First, generate certificates in subdirectory `ssl`:

	./create-certs

Then start it all by running

	docker-compose up

The startup takes a few minutes, but finally you can point your
browser at `http://localhost:5601` to connect to the Kibana web interface.

To generate some log events, use curl towards the dockerwatch
application. Each request generates `notice` level event through the
Erlang Logger. For example:

Create a counter called `cnt`:

	# curl --cacert ssl/dockerwatch-ca.pem -i -H "Content-Type: application/json" -X POST -d "" https://localhost:8443/cnt
	HTTP/1.1 204 No Content
	content-type: text/html
	date: Fri, 23 Nov 2018 13:48:03 GMT
	server: Cowboy
	vary: accept

Increment the counter `cnt`:

    # curl --cacert ssl/dockerwatch-ca.pem -H "Content-Type: application/json" -X POST -d '{}' https://localhost:8443/cnt

Read the counter `cnt` as json:

	# curl --cacert ssl/dockerwatch-ca.pem -H "Accept: application/json" https://localhost:8443/cnt
	{"cnt":1}

For further example requests, see the
[simple docker example](http://github.com/erlang/docker-erlang-example/).

In the Kibana web interface, you need to create an index pattern for
the logstash events. This you can do under the Management tab. Then
you can see all events in the Discovery tab.


### docker-compose configuration

docker-compose.yml:
```
version: "2"
services:
  dockerwatch:
    build: .
    image: dockerwatch
    init: true
    ports:
      - "8443:8443"
    logging:
      driver: "gelf"
      options:
        gelf-address: "udp://localhost:44622"
    volumes:
      - "./ssl:/etc/ssl/certs"
  logstash:
    image: docker.elastic.co/logstash/logstash:6.4.3
    ports:
      - "44622:44622/udp"
      - "9600:9600"
    volumes:
      - "./logstash/logstash.yml:/usr/share/logstash/config/logstash.yml"
      - "./logstash/logstash.conf:/usr/share/logstash/pipeline/logstash.conf"
  elasticsearch:
    image: docker.elastic.co/elasticsearch/elasticsearch:6.4.3
    ports:
      - "9200:9200"
    volumes:
      - "./elasticsearch/elasticsearch.yml:/usr/share/elasticsearch/config/elasticsearch.yml"
  kibana:
    image: docker.elastic.co/kibana/kibana:6.4.3
    ports:
      - "5601:5601"
    volumes:
      - "./kibana/kibana.yml:/usr/share/kibana/config/kibana.yml"
```


### Logstash configuration

The Logstash pipeline configuration is specified in
`logstash/pipeline/logstash.conf`. This is the same as in the
[Logstash example](https://github.com/erlang/docker-erlang-example/tree/master/advanced_examples/logstash),
except it has the additional `elasticsearch` output plugin.


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
    file {
        path => "/usr/share/logstash/logs/output.log"
    }
    elasticsearch {
        hosts => ["elasticsearch:9200"]
        http_compression => true
    }
}
```



### Equivalent without using `docker-compose`

The equivalent, not using docker-compose, would be something like the
following (after modifying logstash.conf to use $ELASTICSERACHIP
instead of the name 'elasticsearch' for the elasticsearch host):

Start elasticsearch:

	docker run --rm -p 9200:9200 --volume="$PWD/elasticsearch/elasticsearch.yml:/usr/share/elasticsearch/config/elasticsearch.yml" docker.elastic.co/elasticsearch/elasticsearch:6.4.3

Get IP address of elasticsearch container:

	EIP=$(docker inspect --format='{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' <elasticsearch container id>)

Start logstash with ($ELASTICSEARCHIP is used in logstash.conf)

	docker run --rm --env ELASTICSEARCHIP=$EIP -p 44622:44622/udp -p 9600:9600 --volume="$PWD/logstash/logstash.yml:/usr/share/logstash/config/logstash.yml" --volume="$PWD/logstash/logstash.conf:/usr/share/logstash/pipeline/logstash.conf" docker.elastic.co/logstash/logstash:6.4.3

Start Kibana (configuration found in kibana/config/kibana.yml, $ELASTICSEACH_URL is an env var which Kibana reads):

	docker run --rm --env ELASTICSEARCH_URL=http://$EIP:9200 -p 5601:5601 --volume="$PWD/kibana/kibana.yml:/usr/share/kibana/config/kibana.yml" docker.elastic.co/kibana/kibana:6.4.3

Start dockerwatch application

	docker run -p 8443:8443 --init --volume="$PWD/ssl:/etc/ssl/certs" --log-driver=gelf --log-opt gelf-address=udp://0.0.0.0:44622 dockerwatch
