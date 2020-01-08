# Run terminus-server from Docker

1. Install Docker as described on the [Docker installation page](https://docs.docker.com/install/). Choose your
   operating system on the left sidebar.
2. Open a command terminal (shell on Unix, Powershell or Cmd on Windows) and run: 
```docker pull terminusdb/terminus-server:latest```
(this will pull the latest version of the server and upgrade to the latest)

Next run:
```docker run -it --name terminus-server -d -v terminus_storage:/app/terminusdb/storage -v terminus_config:/app/terminusdb/config -p 6363:6363  --rm  terminusdb/terminus-server:latest```

# Upgrading from an old version of Terminus DB
1. Open a command terminal (shell on Unix, Powershell or Cmd on Windows) and run: 
```docker pull terminusdb/terminus-server:latest```
(this will pull the latest version of the server and upgrade to the latest)
2. If you have existing install with a docker volume, you either need to delete it, or use a different location. For example, in this script we use "newstorage" and "newconfig" to ensure that we use a fresh file storage area. 

Next run:
```docker run -it --name terminus-server -d -v terminus_storage:/app/terminusdb/newstorage -v terminus_config:/app/terminusdb/newconfig -p 6363:6363  --rm  terminusdb/terminus-server:latest```


3. This will create two volumes: one to persist your database data and one to persist your configuration.

You can configure the following environment variables to taste using the `-e ENV_NAME=value` in the docker command above:

#### Env variables

| Env variable | Description                      | Default   |
|--------------|----------------------------------|-----------|
| SERVER_NAME  | Location of the server           | localhost |
| SERVER_PORT  | Port of the server               | 6363      |
| WORKERS      | Amount of workers for the server | 8         |
| ADMIN_PASS   | Administrator password           | root      |


4. You should see a hash appearing in the terminal.
5. Verify that the server is running by checking the status of the container with `docker ps`. It should
   list the container with the status `Running`.
6. Open the URL in your browser: http://localhost:6363/console
7. Profit!
