# Debian and Ubuntu

Debian and Ubuntu users can also install our deb repo. It needs the JFrog GPG key to be
imported first.

```
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 729A94F1038CF4CD
sudo apt-add-repository "https://terminusdb.jfrog.io/artifactory/terminusdb-deb main"
sudo apt update
sudo apt install terminusdb
```
