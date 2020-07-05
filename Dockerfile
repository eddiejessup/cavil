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

COPY . /opt/cavil/
RUN cabal new-install cavil:app

EXPOSE 8000

ENTRYPOINT app \
    --pg-host ${PG_HOST} \
    --pg-port ${PG_PORT} \
    --pg-user ${PG_USER} \
    --pg-password ${PG_PASSWORD} \
    --pg-db ${PG_DB} \
    --listen-port 8000 \
    --client-username ${CLIENT_USERNAME} \
    --client-password ${CLIENT_PASSWORD}
