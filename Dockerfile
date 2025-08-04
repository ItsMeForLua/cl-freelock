FROM debian:bullseye-slim

# Set non-interactive frontend for package managers to avoid prompts during build.
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
# Create a non-root user 'builder' and give it passwordless sudo.
RUN useradd -m -s /bin/bash builder && \
    echo "builder ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

# Switch to the non-root user.
USER builder
WORKDIR /home/builder/app

# Combine Roswell installation and setup into a single RUN layer.
# Use full paths to ros binary to avoid PATH issues within this layer.
RUN curl -L https://raw.githubusercontent.com/roswell/roswell/master/scripts/install-for-ci.sh | sh && \
    /home/builder/.roswell/bin/ros setup && \
    /home/builder/.roswell/bin/ros install sbcl-bin && \
    /home/builder/.roswell/bin/ros install qlot

# Set the PATH for all subsequent layers and the final container environment.
ENV PATH="/home/builder/.roswell/bin:${PATH}"

# Copy the project files into the image.
COPY --chown=builder:builder . .

# Install dependencies to warm up the cache.
RUN make deps

# Define the default command to run the test suite.
# This will be executed when the container is run without arguments.
CMD ["make", "test"]