# Build From Source

[Debian / Ubuntu](#debian-or-ubuntu)

[Fedora / Red Hat](#fedora-or-red-hat)

## Debian or Ubuntu

The following directions should work on debian or ubuntu.

### SWIPL

To use Terminus Server, you will need the SWIPL installation of
prolog. To install this in Debian variants simply use the apt package
manager:

```
apt install swi-prolog
```
Once installed, you will have to install two library dependencies from SWIPL. 

This can be done by typing: 

```
$ swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 8.1.10-28-g8a26a53c1)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

1 ?- pack_install('https://github.com/GavinMendelGleason/hdt.git').
% Contacting server ....
2 ?- pack_install(mavis). 
% Contacting server ....
```

### HDT Library

You will also need to install `hdt-cpp`. You can git clone the source tree from this repository:

```
git clone https://github.com/rdfhdt/hdt-cpp
```

... and follow the directions contained in the repostiory for the
pre-requisites and building the code.


### Rapper

Rapper is a dependency as well. Install it with:

```
sudo apt install raptor2-utils
```

### Terminus Server

The Terminus Server source tree should then be cloned from GitHub:

```
git clone https://github.com/terminusdb/terminus-server
cd terminus-server
git submodule init
git submodule update
```

You need to set the admin user password which is used as a
super-user API key for access. This can be done with the
`initialize_database` script. The script should also be used to
configure the server name, as shown in the example.

```
utils/initialize_database -k "my_password_here" -s "my_server_name_here"
```

At this point you can enter the terminusDB directory and start the server:

```
./start.pl
```

Now you are ready to interact with the HTTP server.

## Fedora or Red Hat

These instructions have been tested on Fedora 30 and might result in different results depending on your
Fedora / Red Hat release.

### SWIPL

SWI-Prolog is needed to run terminus-server. Install SWI-PROLOG with:

```
sudo dnf install pl pl-devel
```

### HDT Library

The HDT library is used as storage engine until we have implemented terminus-store to be used by terminus-server.

Instructions on how to install HDT can be found on the [hdt-cpp GitHub Page](https://github.com/rdfhdt/hdt-cpp).

A quick overview:

1. Install the dependencies for HDT: `sudo dnf install git gcc-c++ zlib-devel make serd serd-devel autoconf libtool gzip pkgconf-pkg-config`.
2. Clone the latest release: `git clone --branch v1.3.3 https://github.com/rdfhdt/hdt-cpp.git`
3. Build and install hdt-cpp:

```
cd hdt-cpp
./autogen.sh
./configure
make -j4
sudo make install
# Add the library path to the ld paths
echo "/usr/local/lib/" | sudo tee /etc/ld.so.conf.d/local-lib.conf
sudo ldconfig
```

### Rapper

Rapper is a dependency as well. Install it with:

```
sudo dnf install raptor2 
```

### SWIPL libraries

Run SWIPL and install the required dependencies:

```
$ swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 8.1.10-28-g8a26a53c1)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

1 ?- pack_install('https://github.com/GavinMendelGleason/hdt.git').
% Contacting server ....
2 ?- pack_install(mavis).
% Contacting server ....
```


### Terminus Server

The Terminus Server source tree should then be cloned from GitHub: 

```
git clone https://github.com/terminusdb/terminus-server
cd terminus-server
git submodule init
git submodule update
```

You need to set the admin user password which is used as a
super-user API key for access. This can be done with the
`initialize_database` script. The script should also be used to
configure the server name, as shown in the example.

```
utils/initialize_database -k "my_password_here" -s "my_server_name_here"
```

At this point you can enter the terminusDB directory and start the server: 

```
./start.pl
```

Now you are ready to interact with the HTTP server. 
