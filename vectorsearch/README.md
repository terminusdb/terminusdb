# TerminusDB 12.1 release candidate with vector search

This folder has a docker compose setup for the **TerminusDB 12.1 release candidate** (`terminusdb/terminusdb-server:12.1-rc`).

> This is a release candidate, not a final release. Please do not use it for production data yet.

What you get:

- TerminusDB 12.1-rc with the built-in vectorlink vector search engine.
- Local embeddings with [Ollama](https://ollama.com) (`nomic-embed-v2`, 768 dimensions). No API key is needed, and a CPU is enough (no GPU).

## Quick start

```shell
cd vectorsearch
cp .env.example .env
# set TERMINUSDB_ADMIN_PASS in .env
docker compose up -d
docker compose logs
```

Then open `localhost:6363` and log in as `admin` with the password from `.env`.

On the first start, the `embeddings-init` service downloads the embedding model. This can take a few minutes.

## Use OpenAI instead

You can use OpenAI (or any OpenAI-compatible endpoint) for embeddings. See the comments in [docker-compose.yml](./docker-compose.yml) under `terminusdb-server`. Set `OPENAI_KEY` in `.env` and you can remove the `embeddings` services.

## Note: embedding settings are locked

The embedding provider, model and dimension are saved in the vector store the first time it starts. If you change them later, the engine will not start on that store. To switch, delete the vector store and start again (the steps are in the comments in `docker-compose.yml`). All documents are embedded again on reindex.

## Problems?

This is a release candidate, so some things may not work yet. Please report problems in [GitHub issues](https://github.com/terminusdb/terminusdb/issues).
