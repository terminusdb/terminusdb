# Development guides

These are tips and tricks for working with the source code of TerminusDB.

## Local `terminus_store_prolog`

For normal usage, you can install the latest released
[`terminus_store_prolog`][] from GitHub:


```
pack_install(terminus_store_prolog).
```

However, it is sometimes useful to use a local build of `terminus_store_prolog`.
For example, suppose you are working on a change to [`terminus-store`][] and
need to test it before release.

To do this, it appears that we need to tell SWI Prolog where the
`terminus_store_prolog` directory is and convince it that the directory is a
pack. We do this with a symbolic link in the local pack directory. For example:

```sh
# Make the directory if it does not exist.
mkdir -p $HOME/.local/share/swi-prolog/pack

# Create a symlink to the source repository directory in the pack directory.
ln -s <path-to>/terminus_store_prolog $HOME/.local/share/swi-prolog/pack/
```

The above works on Linux and macOS. We haven't tried it on Windows.

[`terminus_store_prolog`]: https://github.com/terminusdb/terminus_store_prolog
[`terminus-store`]: https://github.com/terminusdb/terminusdb-store
