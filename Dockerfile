FROM haskell:8.2.1

WORKDIR /opt/server

RUN cabal update

COPY ./HaskellWorkshop.cabal /opt/server/HaskellWorkshop.cabal

RUN cabal install --only-dependencies -j4

COPY . /opt/server
RUN cabal install

CMD ["cabal", "run"]

EXPOSE 8080