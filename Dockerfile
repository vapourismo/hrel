FROM base/archlinux

# Setup system
RUN pacman -Sy --noconfirm postgresql-libs git sed tar make stack ghc grep awk && \
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
RUN rm -rf build
USER root
RUN pacman -Rs git sed tar make stack grep awk

# Setup entrypoint
USER hrel
ENV PATH=/hrel/.local/bin:$PATH