# Build From Source

[Debian / Ubuntu](#debian-or-ubuntu)

[Fedora / Red Hat](#fedora-or-red-hat)

[Arch Linux](#arch-linux)

[Mac OS](#mac-os)

## Debian or Ubuntu

The following directions should work on debian or ubuntu.

### Rust

Install Rust by following the following instructions on the official
Rust installation guide.

https://www.rust-lang.org/tools/install

### swi-prolog

To use TerminusDB Server, you will need the swi-prolog installation of
prolog. The TerminusDB team tends to use the latest stable for local
development and packaging, but likely, other versions of swi-prolog since
8.0.3 will work as well.
Currently we don't have an officially supported version of swi-prolog,
but latest stable is likely to work.

To install swi-prolog in Debian variants simply use the apt package
manager:

```
apt install swi-prolog
```

the swi-prolog project website also contains downloadable packages for
swi-prolog for different operating systems:
[stable](https://www.swi-prolog.org/download/stable)
[devel](https://www.swi-prolog.org/download/devel).

If you want to experiment with multiple builds of swi-prolog, you may
be interested in [swivm](https://github.com/fnogatz/swivm). This tool
lets you build and install multiple versions of swi-prolog and switch
between them.

Once installed, you will have to install one library dependency for
our storage backend: [terminus_store_prolog](https://github.com/terminusdb/terminus_store_prolog).

This can be done by typing:

```
$ swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 8.1.10-28-g8a26a53c1)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

1 ?- pack_install(terminus_store_prolog).
% Contacting server ....
  ?- pack_install(tus).
```

### TerminusDB Server

The TerminusDB Server source tree should then be cloned from GitHub and compiled:

```
git clone https://github.com/terminusdb/terminusdb
cd terminusdb
make
```

This will create a binary called `terminusdb`.
You need to set the admin user password which is used as a
super-user API key for access. This can be done with the
`terminusdb` binary. The script should also be used to
configure the server name, as shown in the example. It will also
create the system database.

```
./terminusdb store init --key "my_password_here"
```

At this point you can enter the terminusDB directory and start the server:

```
./terminusdb serve
```

Now you are ready to interact with the HTTP server.

## Fedora or Red Hat

These instructions have been tested on Fedora 30 and might result in different results depending on your
Fedora / Red Hat release.


### Rust

Install Rust by following the following instructions on the official
Rust installation guide.

https://www.rust-lang.org/tools/install

### SWIPL

SWI-Prolog is needed to run terminusdb-server. Install SWI-PROLOG with:

```
sudo dnf install pl pl-devel
```

### SWIPL libraries

Run SWIPL and install the required dependencies, note that you need to have
rust installed to compile the dependencies:

```
$ swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 8.1.10-28-g8a26a53c1)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

1 ?- pack_install(terminus_store_prolog).
% Contacting server ....
  ?- pack_install(tus).
```


### TerminusDB Server

The TerminusDB Server source tree should then be cloned from GitHub and compiled:

```
git clone https://github.com/terminusdb/terminusdb
cd terminusdb
make
```

This will create a binary called `terminusdb`.
You need to set the admin user password which is used as a
super-user API key for access. This can be done with the
`terminusdb` binary. The script should also be used to
configure the server name, as shown in the example. It will also
create the system database.

```
./terminusdb store init --key "my_password_here"
```

At this point you can enter the terminusDB directory and start the server:

```
./terminusdb serve
```

Now you are ready to interact with the HTTP server.


## Arch Linux


### Rust

Install Rust by following the following instructions on the official
Rust installation guide.

https://www.rust-lang.org/tools/install

### Library dependencies

1. Install all dependencies of all the required libraries:

```
sudo pacman -S git swi-prolog make automake autoconf libtool zlib pkgconf gcc
```

### SWIPL libraries

Run SWIPL and install the required dependencies, note that you need to have
rust installed to compile the dependencies:

```
$ swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 8.1.10-28-g8a26a53c1)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

1 ?- pack_install(terminus_store_prolog).
% Contacting server ....
  ?- pack_install(tus).
```


### TerminusDB Server

The TerminusDB Server source tree should then be cloned from GitHub and compiled:

```
git clone https://github.com/terminusdb/terminusdb
cd terminusdb
make
```

This will create a binary called `terminusdb`.
You need to set the admin user password which is used as a
super-user API key for access. This can be done with the
`terminusdb` binary. The script should also be used to
configure the server name, as shown in the example. It will also
create the system database.

```
./terminusdb store init --key "my_password_here"
```

At this point you can enter the terminusDB directory and start the server:

```
./terminusdb serve
```

Now you are ready to interact with the HTTP server.

## Mac OS

### Installing SWI Prolog and Rust

First of all, SWI Prolog and Rust should be installed from [homebrew](https://brew.sh/) by executing in a terminal:

```
brew install swi-prolog
brew install rust
```

### Installing and compiling the storage back-end

Run SWI-Prolog by running `swipl` inside a terminal.

In the SWI-Prolog prompt, enter the following:

```
pack_install(terminus_store_prolog).
pack_install(tus).
```

Press Y if it asks for confirmation, be sure to check the GitHub URLs to check whether you trust the package.

If the compilation has finished, press Ctrl+D to exit the SWI Prolog interactive prompt.

### Running TerminusDB-server

Clone the Git repository and execute the `terminusdb` binary file, the `terminusdb store init` command  will initialize
the system database:

```
git clone https://github.com/terminusdb/terminusdb.git
cd terminusdb
make
./terminusdb store init --key root
./terminusdb serve
```

It should now start on https://127.0.0.1:6363.
