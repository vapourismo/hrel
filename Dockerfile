FROM base/archlinux

# Setup
RUN pacman -Sy --noconfirm postgresql-libs ghc ghc-static ghc-libs git wget tar && \
    mkdir build build/hrel

# Cabal
RUN wget 'https://www.haskell.org/cabal/release/cabal-install-2.0.0.1/cabal-install-2.0.0.1-x86_64-unknown-linux.tar.gz' -O cabal.tar.gz && \
    tar xf cabal.tar.gz && \
    ./cabal update && \
    ./cabal install -j cabal-install alex happy && \
    rm cabal

# Environment
ENV PATH=$PATH:/root/.local/bin:/root/.cabal/bin

# Build
ADD src build/hrel/src
ADD lib build/hrel/lib
ADD Setup.hs stack.yaml hrel.cabal build/hrel/
RUN cd build/hrel && \
    cabal install -j && \
    cd ../.. && \
    rm -rf build && \
    pacman -Rs --noconfirm git wget tar
