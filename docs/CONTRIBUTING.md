# How to contribute

Thanks for taking the time to contribute to TerminusDB!

## Testing

Before submitting a change, please run `make ; ./terminusdb test` to make sure
that all tests pass.  Failure should result in a big fail message, and
success with a final `true`. API tests will require that the admin
password is `root` or that the environment variable
`TERMINUS_ADMIN_PASSWD` is set prior to invocation of `terminusdb`.

## Submitting changes

Please send a [GitHub Pull Request](https://github.com/terminusdb/terminusdb/pull/new/main) to the main branch.

Please write clear log messages with your commits. Small changes can be a one line message, 
but big changes should have a descriptive paragraph with a newline after the title in the message.

It should look something like this: 

    $ git commit -m "My change title
    > 
    > This is a paragraph describing my change."

## Development environment

One of the easier ways to set up a development environment is by forking the git repository, cloning it and checking out the `dev` branch.
Docker is a prerequisite for setting it up this way, an alternative is following the instructions in [BUILD.md](BUILD.md).

1. Make a fork on GitHub
2. Clone the repository with `git clone git@github.com:[your_username]/terminusdb.git`
3. Go to the directory `cd terminusdb`.
4. Run `docker run -it --mount type=bind,source="$(pwd)",target=/app/terminusdb -p 6363:6363 --rm  terminusdb/terminusdb:dev`
   inside the terminusdb directory. It will mount the current sources to the Docker container.
5. Run `make.` inside the swipl console after you changed the code.


## Coding conventions

We have a house style for prolog, especially for conditionals. We will be releasing a prolog mode for Emacs soon that 
helps with the indentation. Until then, try to copy what you see.

### Veterans of the Tabs / Spaces War

No tabs please! 4 spaces for indentation. This is non-negotiable.
