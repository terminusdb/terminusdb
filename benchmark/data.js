window.BENCHMARK_DATA = {
  "lastUpdate": 1634720287832,
  "repoUrl": "https://github.com/terminusdb/terminusdb",
  "entries": {
    "Benchmark": [
      {
        "commit": {
          "author": {
            "email": "sean.leather@gmail.com",
            "name": "Sean Leather",
            "username": "spl"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0b17b0af9f934cf6d213cadbee004fb2c3283eb8",
          "message": "Merge pull request #700 from terminusdb/benchmark_improvement\n\nci: add benchmarking with external machine",
          "timestamp": "2021-10-19T16:38:50+02:00",
          "tree_id": "3178258d02e97713821b4e04d68d533c7b46d169",
          "url": "https://github.com/terminusdb/terminusdb/commit/0b17b0af9f934cf6d213cadbee004fb2c3283eb8"
        },
        "date": 1634654585608,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 195.845,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 656.702,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 666.986,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 346.012,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 381.52,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 143.719,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 44.881,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 42.789,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "sean@terminusdb.com",
            "name": "Sean Leather",
            "username": "spl"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "6f3af50a7e1727ae672e7cb38330f77ada7c4262",
          "message": "Merge pull request #712 from terminusdb/ci_latest_docker_fix\n\nci: push docker latest tag when a version is pushed",
          "timestamp": "2021-10-20T10:41:41+02:00",
          "tree_id": "f8793d4ed5d2fd8f9e6576d1db68c6e8bba3c764",
          "url": "https://github.com/terminusdb/terminusdb/commit/6f3af50a7e1727ae672e7cb38330f77ada7c4262"
        },
        "date": 1634719507140,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 223.063,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 718.368,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 730.374,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 367.997,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 437.527,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 162.827,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 47.555,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 39.668,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "gavin@terminusdb.com",
            "name": "Gavin Mendel-Gleason",
            "username": "GavinMendelGleason"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "a87342f57234b7d9499037501f2ea0e9955cce0d",
          "message": "Merge pull request #713 from terminusdb/change-pull-request-template\n\nUpdate pull_request_template.md",
          "timestamp": "2021-10-20T10:54:39+02:00",
          "tree_id": "134f62f3c380c191411101baacd6ab6ed36cd582",
          "url": "https://github.com/terminusdb/terminusdb/commit/a87342f57234b7d9499037501f2ea0e9955cce0d"
        },
        "date": 1634720287112,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 238.056,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 826.029,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 797.022,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 392.154,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 438.962,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 173.627,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 90.519,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 56.819,
            "unit": "ms"
          }
        ]
      }
    ]
  }
}