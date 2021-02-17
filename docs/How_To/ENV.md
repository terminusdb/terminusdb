# Bootstrap

Bootstrap is a shell script that makes use of Docker to run TerminusDB.
It is an officially supported way of running TerminusDB.

## ENV variables

TerminusDB is configurable from ENV variables that can be set by either setting ENV
variables locally, or by creating a file called `ENV` that will be read on the start
of the script.

The file `ENV.example` contains an example `ENV` file.

| ENV name                      | Default value                                                               | Purpose                                                       |
|-------------------------------|-----------------------------------------------------------------------------|---------------------------------------------------------------|
| `TERMINUSDB_DOCKER`           | sudo docker                                                                 | Default docker command                                        |
| `TERMINUSDB_CONTAINER`        | terminusdb-server                                                           | Name of the running container                                 |
| `TERMINUSDB_REPOSITORY`       | terminusdb/terminusdb-server                                                | Docker image                                                  |
| `TERMINUSDB_NETWORK`          | bridge                                                                      | Docker network mode                                           |
| `TERMINUSDB_TAG`              | The latest version tag of terminusdb-server                                 | TerminusDB docker image version                               |
| `TERMINUSDB_STORAGE`          | terminusdb_storage_local                                                    | Storage volume name                                           |
| `TERMINUSDB_PORT`             | 6363                                                                        | Port to run TerminusDB                                        |
| `TERMINUSDB_LOCAL`            |                                                                             | Local folder to mount inside container                        |
| `TERMINUSDB_SERVER`           | 127.0.0.1                                                                   | Server on which TerminusDB will run                           |
| `TERMINUSDB_PASS`             | root                                                                        | Password for accessing TerminusDB                             |
| `TERMINUSDB_AUTOLOGIN`        | false                                                                       | Whether the administration console should automatically login |
| `TERMINUSDB_CONSOLE`          | http://127.0.0.1/console                                                    | URL for browser top open console                              |
| `TERMINUSDB_CONSOLE_BASE_URL` | https://unpkg.com/@terminusdb/terminusdb-console@SOME_VERSION/console/dist/ | URL to hosted console                                         |
| `TERMINUSDB_HTTPS_ENABLED`    | false                                                                       | Enable HTTPS                                                  |
| `TERMINUSDB_SSL_CERT`         | A self signed cert                                                          | Path to SSL cert inside terminusdb-server container           |
| `TERMINUSDB_SSL_CERT_KEY`     | A self-created private key                                                  | Path to private key for SSL cert inside container             |
