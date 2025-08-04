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

# Combine Roswell installation and setup into a single RUN layer to ensure
# the 'ros' command is available in the PATH for subsequent commands.
RUN curl -L https://raw.githubusercontent.com/roswell/roswell/master/scripts/install-for-ci.sh | sh && \
    export PATH="/home/builder/.roswell/bin:${PATH}" && \
    ros setup && \
    ros install sbcl-bin && \
    ros install qlot

# Set the PATH for subsequent layers and the final container.
ENV PATH="/home/builder/.roswell/bin:${PATH}"

# Copy the project files into the image.
COPY --chown=builder:builder . .

# Install dependencies to warm up the cache.
RUN make deps

# Define the default command to run the test suite.
# This will be executed when the container is run without arguments.
CMD ["make", "test"]
