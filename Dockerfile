# Stage 1: Build Cronicle
FROM cronicle/base-alpine AS cronicle-build
RUN apk add --no-cache git npm python3 alpine-sdk && \ 
git clone --depth=1 https://github.com/cronicle-edge/cronicle-edge \ 
/build/cronicle-edge

WORKDIR /build/cronicle-edge

# Ensure the bundle script is executable and build Cronicle
RUN chmod +x bundle && ./bundle /dist --s3 --tools

# Stage 2: Final Image Based on R2U for R Dependencies
FROM eddelbuettel/r2u:24.04

# Install system dependencies first (as root)
COPY dependencies.txt .
RUN echo "Updating deps... 2/14/2025" && \ 
apt-get update && \ 
apt-get install -y $(cat dependencies.txt) && \ 
rm -rf /var/lib/apt/lists/*

# Install R dependencies (as root)
RUN apt-get update && apt-get install -y r-base r-base-dev
COPY dependencies_r.txt install_r_deps.sh .
RUN chmod +x install_r_deps.sh && ./install_r_deps.sh

# Install Node.js (as root)
ENV NODE_VERSION="22.x"
RUN curl -fsSL https://deb.nodesource.com/setup_${NODE_VERSION} | bash -
RUN apt-get install -y nodejs

# Create cronicle directory and set ownership
WORKDIR /opt/cronicle
RUN chown -R ubuntu:ubuntu /opt/cronicle

# Copy Cronicle from the build stage
COPY --from=cronicle-build /dist .

# Fix ownership after copying
RUN chown -R ubuntu:ubuntu /opt/cronicle

# Define build-time arguments
ARG AWS_SECRET_ACCESS_KEY

# Set environment variables for Cronicle
ENV PATH="/opt/cronicle/bin:/opt/cronicle/minio-binaries:${PATH}" \
  CRONICLE_foreground=1 \
  CRONICLE_echo=1 \
  TZ=America/New_York \
  LANG="en_US.UTF-8" \
  LANGUAGE="en_US.UTF-8" \
  LC_ALL="en_US.UTF-8" \
  CRONICLE_Storage__engine=S3 \
  CRONICLE_Storage__S3__params__Bucket=cronicle \
  CRONICLE_Storage__AWS__endpoint=https://s3-gate.mortality.watch \
  CRONICLE_Storage__AWS__forcePathStyle=true \
  CRONICLE_Storage__AWS__region=us-east-1 \
  CRONICLE_Storage__AWS__credentials__secretAccessKey="${AWS_SECRET_ACCESS_KEY}" \
  CRONICLE_Storage__AWS__credentials__accessKeyId=minio

# Protect sensitive folders (as root, then fix ownership)
RUN mkdir -p /opt/cronicle/data /opt/cronicle/conf && \ 
chmod 0700 /opt/cronicle/data /opt/cronicle/conf && \ 
chown -R ubuntu:ubuntu /opt/cronicle/data /opt/cronicle/conf

# SSL configuration
COPY openssl.cnf .
RUN chown ubuntu:ubuntu /opt/cronicle/openssl.cnf
ENV OPENSSL_CONF=/opt/cronicle/openssl.cnf

# Install MinIO Client
RUN mkdir -p minio-binaries && \ 
curl -fsSL https://dl.min.io/client/mc/release/linux-amd64/mc \ 
-o minio-binaries/mc && chmod +x minio-binaries/mc && \ 
chown -R ubuntu:ubuntu minio-binaries

# Switch to ubuntu user for MinIO setup
USER ubuntu
RUN minio-binaries/mc alias set minio https://s3-gate.mortality.watch \ 
minio $AWS_SECRET_ACCESS_KEY

# Expose Cronicle Manager Port
EXPOSE 3012

# Set default command
ENTRYPOINT ["/usr/bin/tini", "-s", "--"]
CMD ["manager"]
