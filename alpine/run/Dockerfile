FROM alpine:3.3

RUN addgroup -S dockerwatch && adduser -S -g dockerwatch dockerwatch

COPY artifacts/dockerwatch /dockerwatch

RUN chown -R dockerwatch /dockerwatch

USER dockerwatch

EXPOSE 9898

CMD ["/dockerwatch/bin/dockerwatch", "console"]
