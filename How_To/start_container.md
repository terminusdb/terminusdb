# Starting a container

## TerminusDB Bootstrap (recommended)

TerminusDB bootstrap is wrapper script around the Docker image which already sets sane defaults to make
the experience as easy as possible. For instance, it automatically persists the data in a volume.

Additional instructions on how to run TerminusDB-bootstrap can be found on the [repo](https://github.com/terminusdb/terminusdb-bootstrap)

## Running it manually with Docker (advanced users)

The TerminusDB Docker image can be run with:

```
sudo docker run -p 6363:6363 terminusdb/terminusdb-server:latest
```

Note that using a specific version instead of latest is recommended for production usage. Another thing that you might want to do is setting up a volume in /app/terminusdb/storage.
