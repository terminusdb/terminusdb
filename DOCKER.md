# Run terminus-server from Docker
​
1. Install Docker as described on the [Docker installation page](https://docs.docker.com/install/). Choose your
   operating system on the left sidebar.
2. Run `docker run --name terminus-server -p 6363:6363 -e SERVER_NAME=localhost -e DB_PASS=root -it terminusdb/terminus-server:latest`

You can configure the following environment variables to taste using the `-e ENV_NAME=value` in the docker command above:

#### Env variables

| Env variable   | Description                      | Default   |
|----------------|----------------------------------|-----------|
| SERVER_NAME    | Location of the server           | localhost |
| SERVER_PORT    | Port of the server               | 6363      |
| WORKERS_AMOUNT | Amount of workers for the server | 8         |
| ADMIN_PASS     | Administrator password           | root      |


3. You should see the following in a terminal:
​
```
Successfully updated admin key!!
% Started server at http://localhost:6363/
​
 ** Syncing 'http://localhost:6363/terminus/document' in collection 'http://localhost:6363/terminus'
​
Predicate Bitmap in 14 us
Count predicates in 3 us
Count Objects in 3 us Max was: 2
Bitmap in 2 us
Bitmap bits: 32 Ones: 31
Object references in 9 us
Sort lists in 3 us
Index generated in 61 us
​
 ** Syncing 'http://localhost:6363/terminus/inference' in collection 'http://localhost:6363/terminus'
​
Predicate Bitmap in 9 us
Count predicates in 2 us
Count Objects in 1 us Max was: 1
Bitmap in 2 us
Bitmap bits: 5 Ones: 5
Object references in 5 us
Sort lists in 1 us
Index generated in 39 us
​
 ** Syncing 'http://localhost:6363/terminus/schema' in collection 'http://localhost:6363/terminus'
​
Predicate Bitmap in 32 us
Count predicates in 10 us
Count Objects in 4 us Max was: 12
Bitmap in 3 us
Bitmap bits: 194 Ones: 127
Object references in 24 us
Sort lists in 23 us
Index generated in 123 us
Welcome to SWI-Prolog (threaded, 64 bits, version 8.0.3)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.
​
For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).
```
​
The server can be stopped by pressing Ctrl+C.
​
​
4. Open the URL in your browser: http://localhost:6363/dashboard
5. Profit!
