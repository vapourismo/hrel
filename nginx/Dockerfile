FROM base/archlinux

# Setup system
RUN pacman -Sy --noconfirm nginx ruby && \
	gem install -n /usr/bin sass && \
    mkdir /hrel

# Provide nginx configuration
ADD nginx.conf /etc/nginx/nginx.conf
ADD index.scss /hrel/index.scss

# Final configuration and clean-up
RUN sass /hrel/index.scss /hrel/index.css && \
	rm -f /hrel/index.scss && \
    chown -R http:http /hrel && \
	pacman -Rs --noconfirm ruby

# Setup command
CMD nginx -g "daemon off;"