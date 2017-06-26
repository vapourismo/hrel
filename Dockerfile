FROM node

RUN apt-get update -y && apt-get upgrade -y && useradd -d /hrel -m hrel

USER hrel

WORKDIR /hrel
ADD server server
ADD package.json package.json

RUN npm install
