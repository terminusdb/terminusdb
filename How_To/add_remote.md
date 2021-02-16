# Adding a new remote besides TerminusDB-Hub

## Using the CLI
It is possible to insert a new remote for a particular database. For instance, when you want to host a remote TerminusDB to
push and pull from in a closed-off environment. Be sure that the remote you are adding is reachable.

```bash
./terminusdb remote add admin/your_database_name NameOfYourRemote 'https://URL_OF_YOUR_REMOTE:6363'
```
