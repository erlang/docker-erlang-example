#!/bin/bash

set -x

./create-certs
docker build -t logstash logstash/
docker run --name logstash -d -p 9600:9600 -p 44622:44622/udp logstash
docker build -t dockerwatch .
docker run --name dockerwatch -d -p 8443:8443 --init --volume="$PWD/ssl:/etc/ssl/certs" --log-driver=gelf --log-opt gelf-address=udp://0.0.0.0:44622 dockerwatch
IP=$(docker inspect --format='{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' dockerwatch)
# Wait for logstash to finis startup
until curl -s 'localhost:9600/_node'; do sleep 5; echo "waiting for logstash to finish startup"; done
# Create counter via http
curl -H 'Content-Type: application/json' -X POST -d '' http://$IP:8080/cnt
# Increment counter via http
curl -H 'Content-Type: application/json' -X POST -d '{}' http://$IP:8080/cnt
# Read all counters via https
curl --cacert ssl/dockerwatch-ca.pem -H 'Accept: application/json' https://localhost:8443/
# Read the counter `cnt`  as json using https
curl --cacert ssl/dockerwatch-ca.pem -H 'Accept: application/json' https://localhost:8443/cnt
# Increment the counter `cnt` by 20 using http
curl -H 'Content-Type: application/json' -X POST -d '{\"value\":20}' http://$IP:8080/cnt
# Read the counter `cnt` as text using http
curl -H 'Accept: text/plain' http://$IP:8080/cnt
# Check that there are 6 lines in the log (one for each curl command above)
sleep 10
docker exec logstash cat /usr/share/logstash/logs/output.log
test "$(docker exec logstash cat /usr/share/logstash/logs/output.log | wc -l)" = "7"
