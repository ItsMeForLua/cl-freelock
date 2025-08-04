FROM debian:bullseye-slim

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    curl \
    git \
    sudo \
    unzip \
    ca-certificates && \
    rm -rf /var/lib/apt/lists/*

# IMPORTANT
# Create a non-root user builder and give it passwordless sudo.
RUN useradd -m -s /bin/bash builder && \
    echo "builder ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

# Switch to the non-root user.
USER builder
WORKDIR /home/builder/app

RUN curl -L https://raw.githubusercontent.com/roswell/roswell/master/scripts/install-for-ci.sh | sh
ENV PATH="/home/builder/.roswell/bin:${PATH}"

# We split these into separate RUN commands to leverage Docker's layer caching.
RUN ros setup
RUN ros install sbcl-bin
RUN ros install qlot

COPY --chown=builder:builder . .

# Install dependencies to warm up the cache.
RUN make deps

# Define the default command to run the test suite.
# This will be executed when the container is ran without arguments.
CMD ["make", "test"]
