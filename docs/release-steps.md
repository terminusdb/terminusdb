# Release Steps

This document describes the steps for creating a new TerminusDB release.

## Overview

TerminusDB uses an automated release workflow with GitHub Actions that handles:
- Version bumping via `VERSION` file
- Multi-architecture Docker images (AMD64, ARM64) with noroot variants
- Snap package publishing
- Automated artifact management
- The repository is normally in vX.X.X-dev (and then in either no suffic or -rcX)

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
[`v11.2.0-dev`](https://github.com/terminusdb/terminusdb/releases/tag/v11.2.0-dev)
<!-- [[[end]]] -->

**Current repository version:** <!--
[[[cog cog.out(current_repo_version) ]]] -->
`v12.0.0`
<!-- [[[end]]] -->

**Changes since last release:** <!--
[[[cog cog.out(changes_since_last_released_version_link) ]]] -->
[`11.2.0-dev...main`](https://github.com/terminusdb/terminusdb/compare/v11.2.0-dev...main)
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

#### Option A: Manual Version (what is uese)

Go to [Actions → Bump Version](https://github.com/terminusdb/terminusdb/actions/workflows/bump-version.yml) and click **Run workflow**:
- **Branch:** `main`
- **New version:** e.g., `11.2.0`, or like `11.2.0-rc3`

The workflow accepts any suffix after the version number: `-rc1`, `-rc2`, `-alpha`, `-beta`, `-dev`, or no suffix etc.

This will:
- Update `VERSION` file
- Update `docs/release-steps.md` (this file) via cogapp
- Update `src/config/terminus_config.pl`
- Update `distribution/snap/snapcraft.yaml`
- Create a PR with the changes

You may need to close and reopen the PR if the version bump verification build is stuck.

#### Option B: Automatic Patch Bump (not used yet)

When called as `workflow_call` (from other workflows), the action automatically bumps the patch version while preserving any suffix (e.g., `11.1.5-dev` → `11.1.6`, `11.1.6` → `11.2.0-rc1`, `11.2.0-rc1` → `11.2.0`).

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
  - **Always:** `terminusdb/terminusdb-server:v<VERSION>` (e.g., `v11.2.0-rc3`)
  - **Production only:** `terminusdb/terminusdb-server:v<MAJOR>` (e.g., `v11`)
  - **Production only:** `terminusdb/terminusdb-server:latest`
  - Corresponding `-noroot` variants for all tags
- **Pre-release versions** (`-rc`, `-beta`, `-alpha`, `-dev`) get **only** version-specific tags, not `latest` or major version tags
- Triggers enterprise build (internal)

**Docker Tag Examples:**

| Release Version | Tags Created |
|-----------------|--------------|
| `v11.2.0` | `v11.2.0`, `v11`, `latest`, `v11.2.0-noroot`, `v11-noroot`, `latest-noroot` |
| `v11.2.0-rc3` | `v11.2.0-rc3`, `v11.2.0-rc3-noroot` (no `latest` or `v11`) |
| `v11.2.0-beta` | `v11.2.0-beta`, `v11.2.0-beta-noroot` (no `latest` or `v11`) |

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
6. **Set as a pre-release:** Check this for versions with `-rc`, `-beta`, `-alpha` suffixes
7. Click **Publish release**

> **Note:** RC versions (e.g., `11.2.0-rc3`) are fully supported and should be marked as pre-releases in GitHub.

### 6. Verify Release

Confirm successful publication:

- **Docker Hub:** https://hub.docker.com/r/terminusdb/terminusdb-server/tags
  - Verify version tag exists
  - Check multi-arch manifest (AMD64, ARM64)
  - Verify `-noroot` variants
- **Snap Store:** https://snapcraft.io/terminusdb
  - Verify version in appropriate channel
- **GitHub Release:** Verify release notes are correct

### 7. Bump to development version again

Go to [Actions → Bump Version](https://github.com/terminusdb/terminusdb/actions/workflows/bump-version.yml) and click **Run workflow**:
- **Branch:** `main`
- **New version:** e.g., `11.2.0-dev`

Then merge the new development version PR (to ensure build images are correctly produced). You may need to close and reopen the PR if the version bump verification build is stuck.

### 8. Post-Release (Optional)

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
- Verify the workflow ran on `main` branch (PR creation only runs on main)
- Check that branch `version-bump-<VERSION>` was created and pushed
- Review workflow logs for `gh pr create` output
- Ensure `GH_TOKEN` has PR creation permissions
- If branch exists but no PR, manually create: `gh pr create --base main --head version-bump-<VERSION>`

**Cogapp failures:**
- Check cogapp installation in workflow
- Verify VERSION file format (no trailing newlines except one)
- Ensure all cogapp template files have valid syntax

**Version bump PR created but checks don't run:**
- GitHub Actions don't auto-trigger on PRs created by `GITHUB_TOKEN` (security policy)
- **Solution**: Close the PR and reopen it manually
- Checks will automatically run after reopening
- Alternative: Go to Actions tab and manually approve pending workflow runs if they are visible
- This is expected behavior to prevent infinite workflow loops

---

The versions in this file are auto-generated via cogapp when the version bump workflow runs.
