FROM base/archlinux

# Setup system
RUN pacman -Sy --noconfirm postgresql-libs git sed tar make stack ghc grep gawk && \
    useradd -d /hrel -m hrel

# Switch to user
USER hrel
WORKDIR /hrel

# Prepare build environment
RUN mkdir build build/app && \
    git clone https://github.com/vapourismo/pg-store.git build/pg-store

# Copy source tree
ADD src build/app/src
ADD lib build/app/lib
ADD hrel.cabal Setup.hs stack.yaml build/app/

# Build the application
RUN cd build/app && stack setup && stack build && stack install

# Clean up system
USER root
RUN rm -rf build
RUN pacman -Rs --noconfirm git sed tar make stack grep gawk

# Setup entrypoint
USER hrel
ENV PATH=/hrel/.local/bin:$PATH