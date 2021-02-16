window.BENCHMARK_DATA = {
  "lastUpdate": 1613488955622,
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
          "id": "822b7f978e95c2b5e9c9c482b1b04e998f6a6c1e",
          "message": "Fix pull error handling",
          "timestamp": "2021-02-16T15:38:35+01:00",
          "tree_id": "e1aa25a861d37dfe635be77d23999c9bc5e6dd11",
          "url": "https://github.com/terminusdb/terminusdb/commit/822b7f978e95c2b5e9c9c482b1b04e998f6a6c1e"
        },
        "date": 1613486425645,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.068699411775536,
            "unit": "iter/sec",
            "range": "stddev: 0.019191559877066767",
            "extra": "mean: 164.77995236666817 msec\nrounds: 30"
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
          "id": "f7082a4f428fe12fa7e98fc6f744516f03a8e787",
          "message": "Fix TUSless server for communicating push with TUS client",
          "timestamp": "2021-02-16T16:20:28+01:00",
          "tree_id": "0382de08cdabe7a876516ebec412237268e140ff",
          "url": "https://github.com/terminusdb/terminusdb/commit/f7082a4f428fe12fa7e98fc6f744516f03a8e787"
        },
        "date": 1613488954739,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.706213568726697,
            "unit": "iter/sec",
            "range": "stddev: 0.014265952881263021",
            "extra": "mean: 149.11544193333364 msec\nrounds: 30"
          }
        ]
      }
    ]
  }
}