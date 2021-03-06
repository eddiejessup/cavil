FROM haskell:8.10.2-buster

WORKDIR /opt/cavil

RUN apt-get update
RUN apt-get install -y libpq-dev

RUN cabal update

# Get a layer with most dependencies installed.
# Pick dependencies that:
#   - capture a lot of our dependencies, to minimise the remaining dependencies
#   - we are unlikely to drop, to minimise the chance we must rebuild from scratch.
RUN cabal new-install --lib \
    servant-server \
    generic-optics \
    postgresql-simple \
    protolude \
    uuid \
    optics \
    data-default

RUN mkdir /opt/cavil/app /opt/cavil/src /opt/cavil/test
COPY app /opt/cavil/app/
COPY src /opt/cavil/src/
COPY test /opt/cavil/test/
COPY cavil.cabal /opt/cavil/
RUN cabal new-install cavil:exe:app

EXPOSE 80
ENV LISTEN_PORT 80

ENTRYPOINT app
