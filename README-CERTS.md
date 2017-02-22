## Generating Certificate

Generate certificates in subdirectory `ssl`.

### Root CA

    $ openssl genrsa -out dockerwatch-ca.key 4096

    $ openssl req -x509 -new -nodes -key dockerwatch-ca.key -sha256 -days 1024 -out dockerwatch-ca.pem

### Server Certificate

    $ openssl genrsa -out dockerwatch-server.key 4096

Certificate signing request

    $ openssl req -new -key dockerwatch-server.key -out dockerwatch-server.csr

The most important field: `Common Name (eg, YOUR name) []: localhost`. We use localhost in this example.

### Sign it

    $ openssl x509 -req -in dockerwatch-server.csr -CA dockerwatch-ca.pem -CAkey dockerwatch-ca.key -CAcreateserial -out dockerwatch-server.pem -days 500 -sha256
