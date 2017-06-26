FROM node

RUN apt-get update -y && apt-get upgrade -y && useradd -d /hrel -m hrel

ADD . /hrel
RUN chown -R hrel:hrel /hrel

USER hrel
WORKDIR /hrel

RUN cd client && npm install && npm run build && cd .. && npm install

CMD node server/server.js
