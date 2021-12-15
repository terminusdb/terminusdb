# Release Steps

This document describes the steps we go through we making a release.

1. Choose the version number for the release and mentally store it in
   `$VERSION`.
2. Update [`RELEASE_NOTES.md`](./RELEASE_NOTES.md):
   1. Add a section for `$VERSION` at the beginning of the file.
   2. Use the following subsection headers:
      - New
      - Bug fixes
      - Enhancements
      - Other
   3. Add a bullet point for each item under the above subsections. If no items
      can be identified for that subsection, don't use that subsection.
