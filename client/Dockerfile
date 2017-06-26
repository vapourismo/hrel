FROM base/archlinux

RUN pacman -Sy --noconfirm nginx npm && \
    useradd -d /hrel -m hrel

WORKDIR /hrel

ADD . build
RUN chown -R hrel:hrel . && chmod 0755 .

USER hrel
RUN cd build && npm install && npm run build && mv out/* .. && cd .. && rm -rf build

USER root
RUN pacman -Rs --noconfirm npm

ADD nginx.conf /etc/nginx/nginx.conf
CMD nginx -g "daemon off;"
