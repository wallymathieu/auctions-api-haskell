# Build stage
FROM haskell:9.4.8 AS builder

WORKDIR /build

# Copy cabal files
COPY auction-site.cabal .

# Update cabal and install dependencies
RUN cabal update && \
    cabal build --only-dependencies -j4

# Copy source code
COPY app ./app
COPY src ./src
COPY test ./test

# Build the application
RUN cabal build auction-site-exe -j4

# Extract the built executable
RUN cp $(cabal exec which auction-site-exe) /build/auction-site-exe

# Runtime stage
FROM debian:bookworm-slim

WORKDIR /app

# Install runtime dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    ca-certificates \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

# Copy the executable from builder
COPY --from=builder /build/auction-site-exe /app/auction-site-exe

# Create tmp directory for events
RUN mkdir -p /app/tmp

# Expose the port the app runs on
EXPOSE 8080

# Set environment variables
ENV PATH="/app:$PATH"

# Run the application
CMD ["/app/auction-site-exe"]
