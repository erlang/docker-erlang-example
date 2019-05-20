FROM docker.elastic.co/logstash/logstash:6.4.3

# Replace logstash config files
RUN rm -f /usr/share/logstash/pipeline/logstash.conf
ADD pipeline/ /usr/share/logstash/pipeline/
ADD config/ /usr/share/logstash/config/

EXPOSE 9600
EXPOSE 44622/udp
