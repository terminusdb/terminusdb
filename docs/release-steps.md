# Release Steps

This document describes the steps we go through we making a new release.

<!-- [[[cog
import cog
last_released_version = f'`v{LAST_RELEASED_VERSION}`'
last_released_version_link = f'[{last_released_version}](https://github.com/terminusdb/terminusdb/releases/tag/v{LAST_RELEASED_VERSION})'
changes_since_last_released_version_link = f'[`{LAST_RELEASED_VERSION}...main`](https://github.com/terminusdb/terminusdb/compare/v{LAST_RELEASED_VERSION}...main)'
current_repo_version = f'`v{CURRENT_REPO_VERSION}`'
current_repo_version_link = f'[{current_repo_version}](https://github.com/terminusdb/terminusdb/releases/tag/v{CURRENT_REPO_VERSION})'
]]] -->
<!-- [[[end]]] -->

The last released version of TerminusDB is: <!--
[[[cog cog.out(last_released_version_link) ]]] -->
[`v10.1.6`](https://github.com/terminusdb/terminusdb/releases/tag/v10.1.6)
<!-- [[[end]]] -->

The current repository version is: <!--
[[[cog cog.out(current_repo_version) ]]] -->
`v10.1.7`
<!-- [[[end]]] -->

1. Look at the changes to `main` since the last released version: <!--
   [[[cog cog.out(changes_since_last_released_version_link) ]]] -->
   [`10.1.6...main`](https://github.com/terminusdb/terminusdb/compare/v10.1.6...main)
   <!-- [[[end]]] -->

2. (_PR_) Update [`RELEASE_NOTES.md`](./RELEASE_NOTES.md) in `main`:
   1. Add a new section for <!--
      [[[cog cog.out(current_repo_version) ]]] -->
      `v10.1.7`
      <!-- [[[end]]] -->
      at the beginning of the file.
   2. Use the following subsection headers:
      - New
      - Bug fixes
      - Enhancements
      - Other
   3. Add a bullet point for each item under the above subsections. If no items
      can be identified for that subsection, omit that subsection.

3. Review the PR for release notes.
   1. Approve.
   2. Merge.

4. Create a new tag on `main`:
   <!-- [[[cog
   cog.out(f"""
   ```
   git checkout main
   git pull
   git tag v{CURRENT_REPO_VERSION}
   git push origin v{CURRENT_REPO_VERSION}
   ```
   """)
   ]]] -->

   ```
   git checkout main
   git pull
   git tag v10.1.7
   git push origin v10.1.7
   ```
   <!-- [[[end]]] -->

5. Create a new GitHub release for the tag: <!--
   [[[cog cog.out(current_repo_version_link) ]]] -->
   [`v10.1.7`](https://github.com/terminusdb/terminusdb/releases/tag/v10.1.7)
   <!-- [[[end]]] -->

6. (_PR_) Update the versions in [`ci.yml`](../.github/workflows/ci.yml):
   1. Change the value for `LAST_RELEASED_VERSION` to: <!--
      [[[cog cog.out(current_repo_version) ]]] -->
      `v10.1.7`
      <!-- [[[end]]] -->
   2. Change the value for `CURRENT_REPO_VERSION` to the next planned version.

7. Review the PR for correct version.
   1. Wait for CI to pass.
   2. Approve.
   3. Merge.
