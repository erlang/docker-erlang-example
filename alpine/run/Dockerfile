FROM alpine:3.3

# Install the released application
COPY artifacts/dockerwatch /dockerwatch

# Expose relevant ports
EXPOSE 8080
EXPOSE 8443

CMD ["/dockerwatch/bin/dockerwatch", "foreground"]
