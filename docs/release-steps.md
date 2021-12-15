# Release Steps

This document describes the steps we go through we making a new release.

1. Identify the previous version released.
   * Save it as `$LAST_VERSION`.

2. Choose the version number for the new release.
   * Save it as `$VERSION`.

3. Look at the changes to the `main` branch since the last version.
   * Copy the following URL, paste it into your browser, and replace
     `$LAST_VERSION` with its value.

     ```
     https://github.com/terminusdb/terminusdb/compare/$LAST_VERSION...main
     ```

4. Update [`RELEASE_NOTES.md`](./RELEASE_NOTES.md):
   1. Add a section for `$VERSION` at the beginning of the file.
   2. Use the following subsection headers:
      - New
      - Bug fixes
      - Enhancements
      - Other
   3. Add a bullet point for each item under the above subsections. If no items
      can be identified for that subsection, don't use that subsection.
