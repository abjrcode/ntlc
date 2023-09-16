FROM rust:1.72.0-slim

WORKDIR /usr/src/ntlcc

# Install baseline dependencies
RUN apt update && \
  apt install -y curl wget gnupg lsb-release software-properties-common ca-certificates

# Add NodeJS sources
RUN mkdir -p /etc/apt/keyrings && \
    curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg
RUN export NODE_MAJOR=18; echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_$NODE_MAJOR.x nodistro main" | tee /etc/apt/sources.list.d/nodesource.list

# Install NodeJS
RUN apt-get update && \
    apt-get install nodejs -y

# Add LLVM toolchain sources
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN add-apt-repository 'deb http://apt.llvm.org/bookworm/ llvm-toolchain-bookworm-16 main' && \
    add-apt-repository 'deb-src http://apt.llvm.org/bookworm/ llvm-toolchain-bookworm-16 main'

# Install LLVM Dependencies
RUN apt update && apt install -y libz-dev

# Install LLVM toolchain
RUN apt install -y clang-16 lldb-16 lld-16 libpolly-16-dev

# Set clang as default compiler
RUN ln -s /usr/bin/clang-16 /usr/bin/clang

# Install LLVM
RUN wget https://apt.llvm.org/llvm.sh && \
  chmod +x llvm.sh && \
  ./llvm.sh 16

# Copy source code
COPY . .

# NPM install
RUN npm install

# Build
RUN cargo build
