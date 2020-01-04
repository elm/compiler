# Lightweight GNU/Linux distribution for a lightweight image
FROM alpine:latest

# Installing the necessary tools to download the Elm binary from GitHub
RUN apk add --update gzip curl

# Adding a group for our unprivileged user
RUN addgroup -g 1000 -S user

# Adding our unprivileged user (to prevent creating root-owned files in projects)
RUN adduser -h /home/user -g "" -s /bin/sh -G user -S -D -u 1000 user

# Fetching the Elm binary archive
RUN curl -Lo elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz

# Unzipping the archive containing the Elm binary
RUN gunzip elm.gz

# Setting the proper permission in order to allow all users to execute the binary
RUN chmod +x elm

# Moving the binary to call it without its full path
RUN mv elm /usr/local/bin
