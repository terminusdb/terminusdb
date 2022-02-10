# Release Steps

This document describes the steps we go through we making a new release.

1. Note the current release branch: `v10.0`.

2. Choose the version number for the new release. Save it (mentally) as `$VERSION`.

3. Look at the changes between `main` and the current release branch:

   https://github.com/terminusdb/terminusdb/compare/v10.0...main

4. (_PR_) Update [`RELEASE_NOTES.md`](./RELEASE_NOTES.md) in `main`:
   1. Add a new section for `$VERSION` at the beginning of the file.
   2. Use the following subsection headers:
      - New
      - Bug fixes
      - Enhancements
      - Other
   3. Add a bullet point for each item under the above subsections. If no items
      can be identified for that subsection, omit that subsection.

5. Review the PR for release notes.
   1. Approve.
   2. Merge.

6. (_PR_) Merge from `main` to the current release branch:

   https://github.com/terminusdb/terminusdb/compare/v10.0...main

7. Review the PR for unexpected commits.
   1. Wait for CI to pass.
   2. Approve.
   3. Merge.

8. Create a new tag for `$VERSION` on the current release branch:

   ```
   git checkout v10.0
   git pull
   git tag $VERSION
   git push origin $VERSION
   ```

9. Create a new release for this tag on GitHub:

   https://github.com/terminusdb/terminusdb/tags

10. (_PR_) Merge from the current release branch to `main`:

    https://github.com/terminusdb/terminusdb/compare/main...v10.0

11. Review the PR for unexpected commits.
    1. Approve.
    2. Merge.

12. (_PR_) Update the version number in [`terminus_config.pl`](../src/config/terminus_config.pl).

13. Review the PR for correct version.
    1. Wait for CI to pass.
    2. Approve.
    3. Merge.
