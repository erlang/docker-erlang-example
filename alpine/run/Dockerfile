FROM alpine

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs

# Install the released application
COPY artifacts/dockerwatch /dockerwatch

# Expose relevant ports
EXPOSE 8080
EXPOSE 8443

CMD ["/dockerwatch/bin/dockerwatch", "foreground"]
