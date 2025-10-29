# Release Steps

This document describes the steps for creating a new TerminusDB release.

## Overview

TerminusDB uses an automated release workflow with GitHub Actions that handles:
- Version bumping via `VERSION` file
- Multi-architecture Docker images (AMD64, ARM64) with noroot variants
- Snap package publishing
- Automated artifact management

<!-- [[[cog
import cog
last_released_version = f'`v{LAST_RELEASED_VERSION}`'
last_released_version_link = f'[{last_released_version}](https://github.com/terminusdb/terminusdb/releases/tag/v{LAST_RELEASED_VERSION})'
changes_since_last_released_version_link = f'[`{LAST_RELEASED_VERSION}...main`](https://github.com/terminusdb/terminusdb/compare/v{LAST_RELEASED_VERSION}...main)'
current_repo_version = f'`v{CURRENT_REPO_VERSION}`'
current_repo_version_link = f'[{current_repo_version}](https://github.com/terminusdb/terminusdb/releases/tag/v{CURRENT_REPO_VERSION})'
]]] -->
<!-- [[[end]]] -->

**Last released version:** <!--
[[[cog cog.out(last_released_version_link) ]]] -->
[`v11.1.17`](https://github.com/terminusdb/terminusdb/releases/tag/v11.1.17)
<!-- [[[end]]] -->

**Current repository version:** <!--
[[[cog cog.out(current_repo_version) ]]] -->
`v11.2-rc2`
<!-- [[[end]]] -->

**Changes since last release:** <!--
[[[cog cog.out(changes_since_last_released_version_link) ]]] -->
[`11.1.17...main`](https://github.com/terminusdb/terminusdb/compare/v11.1.17...main)
<!-- [[[end]]] -->

---

## Release Process

### 1. Prepare Release Notes

**Before creating the release**, update release notes on `main`:

1. Review changes since last release (see link above)
2. Create a PR updating [`RELEASE_NOTES.md`](./RELEASE_NOTES.md):
   - Add a new section at the top for the intended upcoming version
   - Use standard subsection headers: `Enhancements`, `Bug fixes`, `Maintenance and bug fixes`, `Other`
   - Include issue/PR references (e.g., `#1234`)
3. Get the PR reviewed, approved, and merged

### 2. Bump Version

Use the **Bump Version** GitHub Action to update the `VERSION` file and related references:

#### Option A: Manual Version (Recommended for Releases)

Go to [Actions → Bump Version](https://github.com/terminusdb/terminusdb/actions/workflows/bump-version.yml) and click **Run workflow**:
- **Branch:** `main`
- **New version:** e.g., `11.2.0` or `11.2.0-dev`

This will:
- Update `VERSION` file
- Update `docs/release-steps.md` (this file) via cogapp
- Update `src/config/terminus_config.pl`
- Update `distribution/snap/snapcraft.yaml`
- Create a PR with the changes

#### Option B: Automatic Patch Bump (not currently used)

When called as `workflow_call` (from other workflows), the action automatically bumps the patch version while preserving any suffix (e.g., `11.1.5-dev` → `11.1.6-dev`).

**Review and merge** the version bump PR before proceeding.

### 3. Create Release Tag

After the version bump PR is merged:

```bash
git checkout main
git pull
git tag v<VERSION>  # e.g., v11.2.0
git push origin v<VERSION>
```

**Important:** Pushing a tag triggers the automated build and publish workflows.

### 4. Automated Build & Publish

Pushing a version tag (`v*.*.*`) automatically triggers:

#### Docker Images
- Builds AMD64 image via `.github/workflows/docker-amd64-build.yml`
- Builds ARM64 image via `.github/workflows/docker-arm64-build.yml` (BuildJet runner)
- Runs integration tests on both architectures
- Publishes multi-arch manifests via `.github/workflows/docker-images-publish.yml`:
  - `terminusdb/terminusdb-server:v<VERSION>`
  - `terminusdb/terminusdb-server:v<MAJOR>` (e.g., `v11`)
  - `terminusdb/terminusdb-server:latest` (for non-rc/beta releases)
  - Corresponding `-noroot` variants for all tags
- Triggers enterprise build (internal)

#### Snap Package
- Builds snap via `.github/workflows/snap-build.yml`
- Publishes to Snap Store via `.github/workflows/snap-publish.yml`:
  - **stable** channel for production releases
  - **edge** channel for `-dev` versions

### 5. Create GitHub Release

After automated builds complete (monitor in Actions tab):

1. Go to [Releases](https://github.com/terminusdb/terminusdb/releases)
2. Click **Draft a new release**
3. Select the tag you created
4. **Release title:** `TerminusDB v<VERSION>`
5. **Description:** Copy relevant section from `RELEASE_NOTES.md`
6. Check **Set as a pre-release** for RC/beta versions
7. Click **Publish release**

### 6. Verify Release

Confirm successful publication:

- **Docker Hub:** https://hub.docker.com/r/terminusdb/terminusdb-server/tags
  - Verify version tag exists
  - Check multi-arch manifest (AMD64, ARM64)
  - Verify `-noroot` variants
- **Snap Store:** https://snapcraft.io/terminusdb
  - Verify version in appropriate channel
- **GitHub Release:** Verify release notes are correct

### 7. Post-Release (Optional)

For major releases, consider:
- Announcement blog post
- Update documentation site
- Notify community channels

---

## Build System Architecture

### Key Workflows

| Workflow | Trigger | Purpose |
|----------|---------|---------|
| `bump-version.yml` | Manual or workflow_call | Update VERSION and generated files |
| `docker-amd64-build.yml` | PR, manual, workflow_call | Build and test AMD64 image |
| `docker-arm64-build.yml` | PR, manual, workflow_call | Build and test ARM64 image |
| `docker-images-publish.yml` | Tag push, manual | Publish multi-arch Docker images |
| `snap-build.yml` | PR, manual, workflow_call | Build snap package |
| `snap-publish.yml` | Workflow_call, manual | Publish snap to store |
| `native-build.yml` | Push to main, PR | Run unit and integration tests |
| `code-lint.yml` | PR | Run linting checks |

### Docker Image Variants

All Docker images are built for both AMD64 and ARM64:

- **Standard:** `terminusdb/terminusdb-server:<tag>`
  - Runs as root
  - Full permissions
  
- **No-root:** `terminusdb/terminusdb-server:<tag>-noroot`
  - Runs as non-privileged user (`terminusdb:terminusdb`)
  - Enhanced security for production deployments
  - Built from `docker/no-root/Dockerfile`

### Version File System

The `VERSION` file is the single source of truth. Cogapp (`cog -r`) propagates versions to:
- `docs/release-steps.md` (this file)
- `src/config/terminus_config.pl`
- `distribution/snap/snapcraft.yaml`

---

## Development Workflow

For local testing before release:

```bash
# Clean build
make clean && make dev

# Restart test server
./tests/terminusdb-test-server.sh restart --clean

# Run tests (see PREPARE_PR.md for full checklist)
swipl -g run_tests -t halt src/interactive.pl
npx mocha tests/test/*.js
make lint
```

See [`PREPARE_PR.md`](../PREPARE_PR.md) for complete pre-release testing checklist.

---

## Troubleshooting

**Build fails on ARM64:**
- Check BuildJet runner availability
- Verify GitHub Actions cache settings
- See `.github/workflows/docker-arm64-build.yml`

**Snap publish fails:**
- Verify `SNAP_STORE_LOGIN` secret is valid
- Check version format matches snap naming conventions
- Review snap build logs

**Docker manifest creation fails:**
- Ensure both AMD64 and ARM64 builds succeeded
- Check artifact upload/download between jobs
- Verify image tags are correctly formatted

**Version bump PR not created:**
- Check cogapp installation in workflow
- Verify VERSION file format (no trailing newlines except one)
- Review bump-version.yml workflow logs

---

The versions in this file are auto-generated via cogapp when the version bump workflow runs.
