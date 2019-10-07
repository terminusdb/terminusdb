# Run terminus-server from Docker

1. Install Docker as described on the [Docker installation page](https://docs.docker.com/install/). Choose your
   operating system on the left sidebar.
2. Open a command terminal (shell on Unix, Powershell or Cmd on Windows) and run `docker run -it --name terminus-server -d --mount type=volume,source=terminus_storage,dst=/app/terminusdb/storage --mount type=volume,source=terminus_config,dst=/app/terminusdb/config -p 6363:6363  --rm  terminusdb/terminus-server:latest`

This will create two volumes: one to persist your database data and one to persist your configuration.

You can configure the following environment variables to taste using the `-e ENV_NAME=value` in the docker command above:

#### Env variables

| Env variable   | Description                      | Default   |
|----------------|----------------------------------|-----------|
| SERVER_NAME    | Location of the server           | localhost |
| SERVER_PORT    | Port of the server               | 6363      |
| WORKERS_AMOUNT | Amount of workers for the server | 8         |
| ADMIN_PASS     | Administrator password           | root      |


3. You should see a hash appearing in the terminal.
4. Verify that the server is running by checking the status of the container with `docker ps`. It should
   list the container with the status `Running`.
5. Open the URL in your browser: http://localhost:6363/dashboard
6. Profit!
