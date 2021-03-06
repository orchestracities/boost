DAPS mTLS
=========
> ...or yet another way to lose your sanity

This dir contains dev certificates and keys we use for testing mutual
TLS between the adapter and the mock DAPS service.

### Adapter goodies

* `adapter.key-pair.pem`: 2048-bit RSA pub/pvt key pair without key
  encryption so it isn't password protected.
* `adapter.pub.pem`: pub key extracted from above pair.
* `adapter.minikube.crt.pem`: self-signed, 10-year-valid X509 certificate
  with above key and host of `orionadapterservice.istio-system`, i.e. where
  the adapter gets deployed in the local Minikube cluster.
* `adapter.localhost.crt.pem`: self-signed, 10-year-valid X509 certificate
  with above key and host of `localhost`. Comes in handy for running the
  adapter locally outside of Minikube's cluster.

### Mock DAPS service goodies

* `mockdaps.key-pair.pem`: 2048-bit RSA pub/pvt key pair without key
  encryption so it isn't password protected.
* `mockdaps.pub.pem`: pub key extracted from above pair.
* `mockdaps.minikube.crt.pem`: self-signed, 10-year-valid X509 certificate
  with above key and host of `mockdaps.default`, i.e. where mock
  DAPS gets deployed in the local Minikube cluster.
* `mockdaps.localhost.crt.pem`: self-signed, 10-year-valid X509 certificate
  with above key and host of `localhost`. Comes in handy for running mock
  DAPS locally outside of Minikube's cluster.


### How stuff got generated

Generate 2048-bit RSA pub/pvt key pair for the adapter without encrypting
the pvt key so it isn't password protected.

    $ openssl genrsa -out adapter.key-pair.pem 2048

View/extract pub key:

    $ openssl rsa -in adapter.key-pair.pem -pubout | less
    $ openssl rsa -in adapter.key-pair.pem -pubout -out adapter.pub.pem

Generate self-signed, 10-year-valid adapter certificate tied to hostname
of `orionadapterservice.istio-system`:

    $ openssl req -key adapter.key-pair.pem -new -x509 -days 3650 \
        -subj "/C=ZA/ST=Western Cape/L=Cape Town/O=Rusks Ltd/OU=Ma/CN=orionadapterservice.istio-system" \
        -out adapter.minikube.crt.pem

View plain-text cert:

    $ openssl x509 -text -noout -in adapter.minikube.crt.pem | less

As you're at it, generate another one for `localhost`:

    $ openssl req -key adapter.key-pair.pem -new -x509 -days 3650 \
        -subj "/C=ZA/ST=Western Cape/L=Cape Town/O=Rusks Ltd/OU=Ma/CN=localhost" \
        -out adapter.localhost.crt.pem

Generate mock DAPS service stuff in the same way:

    $ openssl genrsa -out mockdaps.key-pair.pem 2048
    $ openssl rsa -in mockdaps.key-pair.pem -pubout -out mockdaps.pub.pem
    $ openssl req -key mockdaps.key-pair.pem -new -x509 -days 3650 \
        -subj "/C=ZA/ST=Western Cape/L=Cape Town/O=Rusks Ltd/OU=Ma/CN=mockdaps.default" \
        -out mockdaps.minikube.crt.pem
    $ openssl req -key mockdaps.key-pair.pem -new -x509 -days 3650 \
        -subj "/C=ZA/ST=Western Cape/L=Cape Town/O=Rusks Ltd/OU=Ma/CN=localhost" \
        -out mockdaps.localhost.crt.pem
