FROM base/archlinux

# Setup
RUN pacman -Sy --noconfirm postgresql-libs git sed tar make stack grep gawk && \
    mkdir build build/hrel && \
    stack setup --resolver ghc-8.0.2

ENV PATH=$PATH:/root/.local/bin

# Build
ADD src build/hrel/src
ADD lib build/hrel/lib
ADD Setup.hs stack.yaml hrel.cabal build/hrel/
RUN git clone https://github.com/vapourismo/pg-store.git build/pg-store && \
    cd build/hrel && \
    stack install && \
    cd ../.. && \
    rm -rf build && \
    pacman -Rs --noconfirm git sed tar make stack grep gawk
