# Release Steps

This document describes the steps we go through we making a new release.

1. Note the current release branch: `v10.0`

2. Choose the version number for the new release.
   * Save it (mentally) as `$VERSION`.

3. Look at the changes between `main` and the current release branch:

   https://github.com/terminusdb/terminusdb/compare/v10.0...main

4. Update [`RELEASE_NOTES.md`](./RELEASE_NOTES.md):
   1. Add a section for `$VERSION` at the beginning of the file.
   2. Use the following subsection headers:
      - New
      - Bug fixes
      - Enhancements
      - Other
   3. Add a bullet point for each item under the above subsections. If no items
      can be identified for that subsection, don't use that subsection.
