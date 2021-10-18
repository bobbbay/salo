FROM johannesloetzsch/nix-flake:latest
LABEL org.opencontainers.image.source https://github.com/semc-labs/salo

RUN mkdir /source/
COPY . /source/
WORKDIR /source/

RUN mkdir /out/

CMD nix run . && \
    nix build . && cp -L result /out/docker-image.tar.gz
