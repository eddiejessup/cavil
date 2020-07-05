FROM haskell:8.8.3-buster

WORKDIR /opt/cavil

RUN apt-get update
RUN apt-get install -y libpq-dev

RUN cabal update

# Get a layer with most dependencies installed.
# Pick dependencies that:
#   - capture a lot of our dependencies, to minimise the remaining dependencies
#   - we are unlikely to drop, to minimise the chance we must rebuild from scratch.
RUN cabal new-install servant-server
RUN cabal new-install generic-optics
RUN cabal new-install postgresql-simple
RUN cabal new-install --lib protolude uuid optics

RUN mkdir /opt/cavil/app /opt/cavil/src
COPY app /opt/cavil/app/
COPY src /opt/cavil/src/
COPY cavil.cabal Setup.hs /opt/cavil/
RUN cabal new-install cavil:app

EXPOSE 8000
ENV LISTEN_PORT 8000

ENTRYPOINT app
