FROM debian:bullseye-slim

# Set non-interactive frontend for package managers to avoid prompts during build.
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    curl \
    git \
    sudo \
    unzip \
    ca-certificates \
    build-essential \
    libcurl4-openssl-dev \
    automake \
    autoconf \
    && rm -rf /var/lib/apt/lists/*

# Create a non-root user 'builder' and give it passwordless sudo.
RUN useradd -m -s /bin/bash builder && \
    echo "builder ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

# Switch to the non-root user.
USER builder
WORKDIR /home/builder

# Install Roswell from source -- more reliable approach
RUN git clone https://github.com/roswell/roswell.git && \
    cd roswell && \
    sh bootstrap && \
    ./configure --prefix=/home/builder/.local && \
    make && \
    make install

WORKDIR /home/builder/app

# With the new makefile logic, if we don't specify where the path is...
#, docker will not be able to find qlot installed via roswell.
ENV PATH="/home/builder/.local/bin:/home/builder/.roswell/bin:${PATH}"

RUN ros setup && \
    ros install sbcl-bin && \
    ros install qlot

# Copy the project files into the image.
COPY --chown=builder:builder . .

# Install dependencies to warm up the cache.
RUN make deps

# Define the default command to run the test suite.
CMD ["make", "test"]
