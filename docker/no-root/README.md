# TerminusDB Non-Root Docker Image

This directory contains the Dockerfile for building a non-root variant of the TerminusDB Docker image.

## What's Different

The non-root image is based on the standard TerminusDB image but configured to run as a non-root user (UID 1000):

- Changes ownership of `/app/terminusdb/storage/db` to user 1000
- Runs the container as user 1000 instead of root

## Published Images

The non-root images are automatically published to Docker Hub with the same tags as the standard images, but with a `-noroot` suffix:

- `terminusdb/terminusdb-server:dev-noroot` (from main branch)
- `terminusdb/terminusdb-server:v11.0.4-noroot` (from version tags)
- `terminusdb/terminusdb-server:latest-noroot` (from latest release)
- `terminusdb/terminusdb-server:v11-noroot` (major version, from releases)

## Multi-Architecture Support

Both **amd64** and **arm64** architectures are built and published as a multi-arch image, just like the standard image.

## Usage

Use the non-root image the same way as the standard image:

```bash
docker run -it -p 6363:6363 terminusdb/terminusdb-server:dev-noroot
```

## Build Process

The image is built automatically by the GitHub Actions workflow (`.github/workflows/docker-images-publish.yml`) after the standard image is published.
