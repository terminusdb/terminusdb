# TerminusDB build instructions

These instructions give the general steps we use for building TerminusDB from
[source][].

We strive to be as accurate as possible; however, we cannot account for every
possible system configuration. Use these instructions as a guideline, and let us
know with an [issue][] or on [Discord][] if you have any problems.

[source]: https://github.com/terminusdb/terminusdb
[issue]: https://github.com/terminusdb/terminusdb/issues
[Discord]: https://discord.gg/yTJKAma

## Tested systems

We have tested the steps below with these systems and install tools:

* Debian and Ubuntu using `apt-get`
* Fedora using `dnf`
* Arch Linux using `pacman`
* Other Linux systems using `snap`
* macOS using `brew`

## Steps

The instructions are broken into steps describing the operations for each
system.

### 1. Install tool dependencies

#### 1.1 Install common tools

> :left_speech_bubble: TerminusDB uses a few common tools for building and
> running.

The main tools used are GNU `make`, `clang`, and `git`. These should be in your
path (e.g. `which make`) after installation. We provide our preferred steps
below.

**Using `apt-get`**

```sh
sudo apt-get install findutils make clang git
```

**Using `dnf`**

```sh
sudo dnf install findutils make clang git
```

**Using `pacman`**

```sh
sudo pacman -S findutils make clang git
```

#### 1.2 Install Rust

> :left_speech_bubble: TerminusDB uses the systems programming language [Rust][]
> for the underlying [triple][] store, [`terminusdb-store`][store], as well as
> for other functionality.

[Rust]: https://www.rust-lang.org/
[triple]: https://en.wikipedia.org/wiki/Semantic_triple
[store]: https://github.com/terminusdb/terminusdb-store

Install the Rust tools by following the [official instructions][rust_install].

[rust_install]: https://www.rust-lang.org/tools/install

The main tools used are [`cargo`][cargo] and [`rustc`][rustc]. These should be
in your path (e.g. `which cargo`) after installation.

[cargo]: https://doc.rust-lang.org/cargo/
[rustc]: https://doc.rust-lang.org/rustc/

#### 1.3 Install SWI-Prolog

> :left_speech_bubble: Much of TerminusDB is written in the logic programming
> language [Prolog][prolog]. Our development environment is
> [SWI-Prolog][swi_prolog], a well-maintained implementation with many
> [swi_prolog_features][]. We test daily with the latest
> [`stable`][swi_prolog_stable] release. (We cannot guarantee that it will work
> with a `devel` release or an older `stable` release.)

[prolog]: https://en.wikipedia.org/wiki/Prolog
[swi_prolog]: https://www.swi-prolog.org/
[swi_prolog_features]: https://www.swi-prolog.org/features.html
[swi_prolog_stable]: https://www.swi-prolog.org/download/stable

Install the `stable` SWI-Prolog environment following the [official
instructions][swi_prolog_install], or use our preferred steps below.

The main tool used is `swipl`. This should be in your path (e.g. `which swipl`)
after installation.

[swi_prolog_install]: https://www.swi-prolog.org/Download.html

**Using `apt-get`**

Install `apt-add-repository` if needed:

```sh
sudo apt-get install software-properties-common
```

Add the SWI-Prolog PPA for the `stable` release:

```sh
sudo apt-add-repository ppa:swi-prolog/stable
sudo apt-get update
```

Install `swi-prolog`:

```sh
sudo apt-get install swi-prolog
```

**Using `dnf`**

```sh
sudo dnf install pl pl-devel
```

**Using `snap`**

```sh
snap install swi-prolog
```

**Using `pacman`**

```sh
sudo pacman -S swi-prolog
```

**Using `brew`**

```sh
brew install swi-prolog
```

### 2. Install library dependencies

> :left_speech_bubble: TerminusDB requires two libraries to be installed as
> [packs][] with SWI-Prolog. These are [`tus`][tus] (resumable file uploads over
> HTTP) and [`terminus_store_prolog`][terminus_store_prolog] (Prolog binding to
> `terminusdb-store`).

[packs]: https://www.swi-prolog.org/pack/list
[tus]: https://github.com/terminusdb/tus
[terminus_store_prolog]: https://github.com/terminusdb/terminus_store_prolog

> :memo: All of the following commands assume your current working directory is
> the top-level directory of this repository.

Install the library dependencies with:

```sh
make install-deps
```

For documentation on managing packs in general, see
[`prolog_pack.pl`][prolog_pack].

[prolog_pack]: https://www.swi-prolog.org/pldoc/doc/_SWI_/library/prolog_pack.pl

### 3. Build

Build the `terminusdb` executable with:

```sh
make
```

### 4. Test

Run the unit tests with:

```sh
make test
```

For running the integration tests, see the [`README.md`][integration_tests] in
the top-level `tests/` directory.

[integration_tests]: ../tests/README.md

### 5. Initialize the store

> :left_speech_bubble: Before TerminusDB can be used, the store must be
> initialized. Initialization creates a subdirectory `storage/`, the location of
> the system database and all of the databases you will create, in the current
> working directory.

Initialize the store with default settings:

```sh
./terminusdb store init
```

> :memo: The default initial store has one user, `admin`, whose password is
> `root`.

To initialize the store with the `admin` password set to `my_password`, use:

```sh
./terminusdb store init --key "my_password"
```

To wipe your `storage/` directory and start over, use this (along with `--key`
if you want to also set the `admin` password):

```sh
./terminusdb store init --force
```

### 5. Start and stop the server

Start the TerminusDB server with:

```
./terminusdb serve
```

It will start up with a message like this:

```prolog
% Started server at http://localhost:6363/
```

To stop the server, type `^C^C` (`Control-C` twice). You will get a SWI-Prolog
prompt like this:

```
Action (h for help) ?
```

Type `e` to exit.

## Notes

### Manage multiple versions of SWI-Prolog

If you want to install multiple versions of SWI-Prolog and easily switch between
them, you might try [`swivm`][swivm].

[swivm]: https://github.com/fnogatz/swivm
