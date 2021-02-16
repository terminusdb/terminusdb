window.BENCHMARK_DATA = {
  "lastUpdate": 1613477982553,
  "repoUrl": "https://github.com/terminusdb/terminusdb",
  "entries": {
    "Benchmark": [
      {
        "commit": {
          "author": {
            "email": "robin@datachemist.com",
            "name": "rrooij",
            "username": "rrooij"
          },
          "committer": {
            "email": "robin@datachemist.com",
            "name": "rrooij",
            "username": "rrooij"
          },
          "distinct": true,
          "id": "a6e8c333cfac2e80b9c7a737ff3351002fb72698",
          "message": "ci benchmark: don't run serve, the system db needs to be intialized",
          "timestamp": "2021-02-16T12:15:37+01:00",
          "tree_id": "519db5dfd97eb3ffa9c9cb4c1428e26cdc9b9272",
          "url": "https://github.com/terminusdb/terminusdb/commit/a6e8c333cfac2e80b9c7a737ff3351002fb72698"
        },
        "date": 1613474250685,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 5.1299485846389645,
            "unit": "iter/sec",
            "range": "stddev: 0.021680743871788484",
            "extra": "mean: 194.9337275999966 msec\nrounds: 30"
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
            "email": "gavin@terminusdb.com",
            "name": "Gavin Mendel-Gleason",
            "username": "GavinMendelGleason"
          },
          "distinct": true,
          "id": "175cdfbc8ab96f817f2f8355806f9142c0c93764",
          "message": "Add tests to path patterns",
          "timestamp": "2021-02-16T13:17:51+01:00",
          "tree_id": "6fab27f0199602ea2637f9f69a56b3b6fee6d148",
          "url": "https://github.com/terminusdb/terminusdb/commit/175cdfbc8ab96f817f2f8355806f9142c0c93764"
        },
        "date": 1613477982076,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.640238714315271,
            "unit": "iter/sec",
            "range": "stddev: 0.01595998109358533",
            "extra": "mean: 150.59699553333274 msec\nrounds: 30"
          }
        ]
      }
    ]
  }
}