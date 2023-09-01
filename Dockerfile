FROM rust:1.72.0-alpine3.18

WORKDIR /usr/src/ntlcc

# Install NodeJS
RUN apk add nodejs npm

# Install LLVM
RUN apk add musl-dev clang-dev llvm16-static llvm16-dev

# Copy source code
COPY . .

# DOES NOT WORK UNTIL https://gitlab.com/taricorp/llvm-sys.rs/-/merge_requests/32 IS MERGED

# NPM install
# RUN npm install

# Build
# RUN cargo build
