window.BENCHMARK_DATA = {
  "lastUpdate": 1617889559508,
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
      },
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
          "id": "c8804b94ed8b64d7842123d492ff3ad2f6ceb9aa",
          "message": "api_init: remove index_template_path predicate",
          "timestamp": "2021-02-17T10:46:07+01:00",
          "tree_id": "e66653f06ae1743b3d04820a38ff5839d17b4cc9",
          "url": "https://github.com/terminusdb/terminusdb/commit/c8804b94ed8b64d7842123d492ff3ad2f6ceb9aa"
        },
        "date": 1613555269102,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.407189194324177,
            "unit": "iter/sec",
            "range": "stddev: 0.014559031497899385",
            "extra": "mean: 156.07467949999858 msec\nrounds: 30"
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
          "id": "7fdcc5f434d7fef95351cb1edba59b3f645ef0f0",
          "message": "Add triple bulk load test",
          "timestamp": "2021-02-17T12:20:46+01:00",
          "tree_id": "2fe248540a4a523f4e31d5811891a3999ea47e88",
          "url": "https://github.com/terminusdb/terminusdb/commit/7fdcc5f434d7fef95351cb1edba59b3f645ef0f0"
        },
        "date": 1613560949005,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.637714369681782,
            "unit": "iter/sec",
            "range": "stddev: 0.011995352328486739",
            "extra": "mean: 150.654268066666 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 6.226694345720579,
            "unit": "iter/sec",
            "range": "stddev: 0.005552891099514212",
            "extra": "mean: 160.5988578333334 msec\nrounds: 30"
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
          "id": "72654e6a690786b1d3cf1182900dceb49c6bcab4",
          "message": "Update benchmark to add schema controlled graph",
          "timestamp": "2021-02-17T15:44:29+01:00",
          "tree_id": "e1be9ed6ed66ceb8d78060630593f3a9ca69e851",
          "url": "https://github.com/terminusdb/terminusdb/commit/72654e6a690786b1d3cf1182900dceb49c6bcab4"
        },
        "date": 1613573357277,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 5.049957365625562,
            "unit": "iter/sec",
            "range": "stddev: 0.018111487392988644",
            "extra": "mean: 198.0214737666652 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 4.732488470677264,
            "unit": "iter/sec",
            "range": "stddev: 0.010822429510690516",
            "extra": "mean: 211.30532196666726 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 13.450081113404194,
            "unit": "iter/sec",
            "range": "stddev: 0.03323820721119932",
            "extra": "mean: 74.34899399999988 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 13.515643743347008,
            "unit": "iter/sec",
            "range": "stddev: 0.038470669929384686",
            "extra": "mean: 73.98833669999951 msec\nrounds: 30"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "matthijs@terminusdb.com",
            "name": "Matthijs van Otterdijk",
            "username": "matko"
          },
          "committer": {
            "email": "matthijs@terminusdb.com",
            "name": "Matthijs van Otterdijk",
            "username": "matko"
          },
          "distinct": true,
          "id": "1a86f58eed3db74d300c03e786ef3b8700436cb7",
          "message": "reimplement storage size calculation to use layer_stack_names",
          "timestamp": "2021-02-17T16:40:59+01:00",
          "tree_id": "a8acf278785e88cfc8bda03c2552d5163c1c3db2",
          "url": "https://github.com/terminusdb/terminusdb/commit/1a86f58eed3db74d300c03e786ef3b8700436cb7"
        },
        "date": 1613576572306,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.645070694886924,
            "unit": "iter/sec",
            "range": "stddev: 0.019128392007431095",
            "extra": "mean: 150.48748853333555 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 6.256154187998323,
            "unit": "iter/sec",
            "range": "stddev: 0.01908597945531987",
            "extra": "mean: 159.84260776666588 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 16.977622038604096,
            "unit": "iter/sec",
            "range": "stddev: 0.021880644309108303",
            "extra": "mean: 58.90106386666976 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 16.443835610821672,
            "unit": "iter/sec",
            "range": "stddev: 0.02327162672620002",
            "extra": "mean: 60.813062333334265 msec\nrounds: 30"
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
          "id": "c35d28417a6b1a8f36f382158287430d1290e824",
          "message": "Add typecast in the JSON WOQL- fixes path range bug",
          "timestamp": "2021-02-18T11:59:55+01:00",
          "tree_id": "0ca657b9156b11156f92510eedcd2c1deabc2518",
          "url": "https://github.com/terminusdb/terminusdb/commit/c35d28417a6b1a8f36f382158287430d1290e824"
        },
        "date": 1613646100369,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 5.9131576787152005,
            "unit": "iter/sec",
            "range": "stddev: 0.017738299092059737",
            "extra": "mean: 169.11438089999962 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.70689715650441,
            "unit": "iter/sec",
            "range": "stddev: 0.009728044134235137",
            "extra": "mean: 175.22656753333195 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 14.251484121054071,
            "unit": "iter/sec",
            "range": "stddev: 0.023521101575488684",
            "extra": "mean: 70.16813066666334 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 14.313917822755018,
            "unit": "iter/sec",
            "range": "stddev: 0.020578023530708266",
            "extra": "mean: 69.86207496666546 msec\nrounds: 30"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "matthijs@terminusdb.com",
            "name": "Matthijs van Otterdijk",
            "username": "matko"
          },
          "committer": {
            "email": "matthijs@terminusdb.com",
            "name": "Matthijs van Otterdijk",
            "username": "matko"
          },
          "distinct": true,
          "id": "05f0f29a9639a1f1730b94e1a936f99c6f4f7fef",
          "message": "update store version",
          "timestamp": "2021-02-18T15:25:41+01:00",
          "tree_id": "419d0a075de657a4997dafc059f8b5a96b689676",
          "url": "https://github.com/terminusdb/terminusdb/commit/05f0f29a9639a1f1730b94e1a936f99c6f4f7fef"
        },
        "date": 1613658454995,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 5.903423801401539,
            "unit": "iter/sec",
            "range": "stddev: 0.041607030345239264",
            "extra": "mean: 169.39322563333312 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.811513294757913,
            "unit": "iter/sec",
            "range": "stddev: 0.032631311635791645",
            "extra": "mean: 172.07222099999626 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 15.354033936342848,
            "unit": "iter/sec",
            "range": "stddev: 0.023128309811561828",
            "extra": "mean: 65.12946396666545 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 15.409093171636638,
            "unit": "iter/sec",
            "range": "stddev: 0.01693364033905362",
            "extra": "mean: 64.89674563333097 msec\nrounds: 30"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "matthijs@terminusdb.com",
            "name": "Matthijs van Otterdijk",
            "username": "matko"
          },
          "committer": {
            "email": "matthijs@terminusdb.com",
            "name": "Matthijs van Otterdijk",
            "username": "matko"
          },
          "distinct": true,
          "id": "2b4906e59d3e6ca9745f6f0fff2be47aec847490",
          "message": "raise store dependency to 0.16.3",
          "timestamp": "2021-02-18T16:26:50+01:00",
          "tree_id": "d2f8af45a36db7d14330dbcb811a92f8b7b280ab",
          "url": "https://github.com/terminusdb/terminusdb/commit/2b4906e59d3e6ca9745f6f0fff2be47aec847490"
        },
        "date": 1613662132474,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 5.965352268828636,
            "unit": "iter/sec",
            "range": "stddev: 0.015807059948159907",
            "extra": "mean: 167.6346936333336 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.707558193699745,
            "unit": "iter/sec",
            "range": "stddev: 0.021812682863798172",
            "extra": "mean: 175.20627316666597 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 14.776438936199298,
            "unit": "iter/sec",
            "range": "stddev: 0.02576880687974948",
            "extra": "mean: 67.67530419999919 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 14.244859709254868,
            "unit": "iter/sec",
            "range": "stddev: 0.026056237151397574",
            "extra": "mean: 70.20076156666542 msec\nrounds: 30"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "matthijs@terminusdb.com",
            "name": "Matthijs van Otterdijk",
            "username": "matko"
          },
          "committer": {
            "email": "matthijs@terminusdb.com",
            "name": "Matthijs van Otterdijk",
            "username": "matko"
          },
          "distinct": true,
          "id": "61533032a76fa99adc73214194a011290d1a87e2",
          "message": "updated release notes",
          "timestamp": "2021-02-18T16:50:56+01:00",
          "tree_id": "83ea05495bacd245a9554400795b9e596c7d73d3",
          "url": "https://github.com/terminusdb/terminusdb/commit/61533032a76fa99adc73214194a011290d1a87e2"
        },
        "date": 1613663565180,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.13834079063099,
            "unit": "iter/sec",
            "range": "stddev: 0.014053240613747214",
            "extra": "mean: 162.91047273333373 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.828035761016265,
            "unit": "iter/sec",
            "range": "stddev: 0.009019507170420363",
            "extra": "mean: 171.5843966999998 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 15.428861363325765,
            "unit": "iter/sec",
            "range": "stddev: 0.021332791905790377",
            "extra": "mean: 64.813596833334 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 15.508387170475164,
            "unit": "iter/sec",
            "range": "stddev: 0.02349149583118867",
            "extra": "mean: 64.48123773333427 msec\nrounds: 30"
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
          "id": "88ec4a8ead8616405f361341e5e5041fab21e337",
          "message": "Add how to for loading turtle",
          "timestamp": "2021-02-19T11:47:21+01:00",
          "tree_id": "8fc18ceb3b29ad86495a390658ead455ec1af27b",
          "url": "https://github.com/terminusdb/terminusdb/commit/88ec4a8ead8616405f361341e5e5041fab21e337"
        },
        "date": 1613731754134,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.671171444861324,
            "unit": "iter/sec",
            "range": "stddev: 0.017208460474894955",
            "extra": "mean: 149.89871093333704 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 6.417290366575659,
            "unit": "iter/sec",
            "range": "stddev: 0.009685775476418064",
            "extra": "mean: 155.82900926666525 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 15.959948139871813,
            "unit": "iter/sec",
            "range": "stddev: 0.02109073363403188",
            "extra": "mean: 62.656845200001506 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 15.70232289958391,
            "unit": "iter/sec",
            "range": "stddev: 0.022176171761717035",
            "extra": "mean: 63.68484500000307 msec\nrounds: 30"
          }
        ]
      },
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
          "id": "e397b93b94bb5a728872e417a3ef19004cdfd801",
          "message": "AppImage: allow env variables to override",
          "timestamp": "2021-02-19T12:43:38+01:00",
          "tree_id": "b46cf200a612a9e0a18e83cfe60504f6d08dcaf2",
          "url": "https://github.com/terminusdb/terminusdb/commit/e397b93b94bb5a728872e417a3ef19004cdfd801"
        },
        "date": 1613735119851,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 7.97537055635232,
            "unit": "iter/sec",
            "range": "stddev: 0.012618934792383024",
            "extra": "mean: 125.3860235 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 7.654608041126054,
            "unit": "iter/sec",
            "range": "stddev: 0.005863666566270735",
            "extra": "mean: 130.64026199999813 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 18.446230451380963,
            "unit": "iter/sec",
            "range": "stddev: 0.017941600066333694",
            "extra": "mean: 54.2116180666677 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 18.40777823369122,
            "unit": "iter/sec",
            "range": "stddev: 0.016421214525814732",
            "extra": "mean: 54.32486133333185 msec\nrounds: 30"
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
          "id": "858e55cd5ea753f92f594157a21d53be86350ceb",
          "message": "Attempt to fix autoload ssl bug",
          "timestamp": "2021-02-19T15:27:55+01:00",
          "tree_id": "d9ddd226e35df9ee7a2ce2ecf52b5f7994bcd0ba",
          "url": "https://github.com/terminusdb/terminusdb/commit/858e55cd5ea753f92f594157a21d53be86350ceb"
        },
        "date": 1613744968762,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 8.047491550329124,
            "unit": "iter/sec",
            "range": "stddev: 0.010773635675690143",
            "extra": "mean: 124.26232369999848 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 7.793642431736424,
            "unit": "iter/sec",
            "range": "stddev: 0.005207647482953389",
            "extra": "mean: 128.30970996666574 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 20.64749039117945,
            "unit": "iter/sec",
            "range": "stddev: 0.017639630401716056",
            "extra": "mean: 48.4320360999997 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 20.34938914901789,
            "unit": "iter/sec",
            "range": "stddev: 0.018186448624173266",
            "extra": "mean: 49.14152423333367 msec\nrounds: 30"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "kitty@datachemist.com",
            "name": "Kitty Jose",
            "username": "KittyJose"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e396ccc4369c0b1dcdca9620ba43a55a6c37bf82",
          "message": "Adding release notes",
          "timestamp": "2021-02-19T14:53:35Z",
          "tree_id": "55c042020a9a28b13d4fa03c3577780ab41dfd33",
          "url": "https://github.com/terminusdb/terminusdb/commit/e396ccc4369c0b1dcdca9620ba43a55a6c37bf82"
        },
        "date": 1613746521130,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 5.921589133008501,
            "unit": "iter/sec",
            "range": "stddev: 0.014916403793552939",
            "extra": "mean: 168.87358739999976 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.616010937778813,
            "unit": "iter/sec",
            "range": "stddev: 0.011951728064447664",
            "extra": "mean: 178.06233126666768 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 14.458820218018474,
            "unit": "iter/sec",
            "range": "stddev: 0.025091295434677755",
            "extra": "mean: 69.16193610000127 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 14.439987119917571,
            "unit": "iter/sec",
            "range": "stddev: 0.026187057756498124",
            "extra": "mean: 69.25213933332847 msec\nrounds: 30"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "54106287+KittyJose@users.noreply.github.com",
            "name": "Kitty Jose",
            "username": "KittyJose"
          },
          "committer": {
            "email": "54106287+KittyJose@users.noreply.github.com",
            "name": "Kitty Jose",
            "username": "KittyJose"
          },
          "distinct": true,
          "id": "e9147a9f4688afdc68636fea909717d5820c8940",
          "message": "adding release notes",
          "timestamp": "2021-02-19T15:06:01Z",
          "tree_id": "ca17e18c41b9c6b699cd09a73826800bd0eb409c",
          "url": "https://github.com/terminusdb/terminusdb/commit/e9147a9f4688afdc68636fea909717d5820c8940"
        },
        "date": 1613747294983,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.205544040989296,
            "unit": "iter/sec",
            "range": "stddev: 0.018113645945351677",
            "extra": "mean: 161.14622560000052 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.926622612267271,
            "unit": "iter/sec",
            "range": "stddev: 0.01620436362335331",
            "extra": "mean: 168.73016309999923 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 14.542075989345735,
            "unit": "iter/sec",
            "range": "stddev: 0.02747864611311701",
            "extra": "mean: 68.7659726666709 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 14.716132059568407,
            "unit": "iter/sec",
            "range": "stddev: 0.024330374529883292",
            "extra": "mean: 67.95263836666929 msec\nrounds: 30"
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
          "id": "fcdfeda45cb05d50ec1c38a269ae61105bcb74c4",
          "message": "Expand release notes",
          "timestamp": "2021-02-19T16:13:57+01:00",
          "tree_id": "50acdf611874799a0b99bab4f02f224a9aded86c",
          "url": "https://github.com/terminusdb/terminusdb/commit/fcdfeda45cb05d50ec1c38a269ae61105bcb74c4"
        },
        "date": 1613747801814,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 7.547394436968956,
            "unit": "iter/sec",
            "range": "stddev: 0.01348021097360346",
            "extra": "mean: 132.49605653333276 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 7.338035010491455,
            "unit": "iter/sec",
            "range": "stddev: 0.007722591712190472",
            "extra": "mean: 136.27626450000082 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 17.26771640852211,
            "unit": "iter/sec",
            "range": "stddev: 0.016864238816355497",
            "extra": "mean: 57.91153713333349 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 16.49899574160185,
            "unit": "iter/sec",
            "range": "stddev: 0.022571360061947644",
            "extra": "mean: 60.6097495666674 msec\nrounds: 30"
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
          "id": "2237d6ac4cb01466191b19d6cbd583c9009b4d74",
          "message": "Fix autoloading of mimepack",
          "timestamp": "2021-02-19T16:40:50+01:00",
          "tree_id": "d9783f47f7de46eb025f8a9867c7ad6e5bf3022a",
          "url": "https://github.com/terminusdb/terminusdb/commit/2237d6ac4cb01466191b19d6cbd583c9009b4d74"
        },
        "date": 1613749356898,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 7.7049157932668155,
            "unit": "iter/sec",
            "range": "stddev: 0.013389845857043932",
            "extra": "mean: 129.78727176666638 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 7.783942817898164,
            "unit": "iter/sec",
            "range": "stddev: 0.007105139158365481",
            "extra": "mean: 128.46959740000017 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 17.92677667878175,
            "unit": "iter/sec",
            "range": "stddev: 0.018856276390308128",
            "extra": "mean: 55.78247656666614 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 18.277190901205874,
            "unit": "iter/sec",
            "range": "stddev: 0.019203396470923867",
            "extra": "mean: 54.713002966666124 msec\nrounds: 30"
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
          "distinct": false,
          "id": "399be48d3a65635d8897433aff312f78dda189c2",
          "message": "Simplifying date marshalling",
          "timestamp": "2021-02-24T10:26:42+01:00",
          "tree_id": "384d3a934c58005a6b079df5261c5c4222ad8500",
          "url": "https://github.com/terminusdb/terminusdb/commit/399be48d3a65635d8897433aff312f78dda189c2"
        },
        "date": 1614158957634,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.830585238846521,
            "unit": "iter/sec",
            "range": "stddev: 0.014672146516917253",
            "extra": "mean: 146.40033980000075 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 6.727767194308595,
            "unit": "iter/sec",
            "range": "stddev: 0.006844170046198985",
            "extra": "mean: 148.63772349999823 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 15.415265485832478,
            "unit": "iter/sec",
            "range": "stddev: 0.021813452132594034",
            "extra": "mean: 64.87076080000426 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 15.066469179005226,
            "unit": "iter/sec",
            "range": "stddev: 0.0250506093258094",
            "extra": "mean: 66.37255139999733 msec\nrounds: 30"
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
          "id": "9a57bdfd92a91f47cec732b144f947c9df4a5653",
          "message": "Add a date time property",
          "timestamp": "2021-02-24T10:30:14+01:00",
          "tree_id": "95f8d0ac84093bba3a399f7b8aedacbe5c39abb6",
          "url": "https://github.com/terminusdb/terminusdb/commit/9a57bdfd92a91f47cec732b144f947c9df4a5653"
        },
        "date": 1614159135720,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.392506414187546,
            "unit": "iter/sec",
            "range": "stddev: 0.017303664613711002",
            "extra": "mean: 156.43316333333627 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 6.25975839735506,
            "unit": "iter/sec",
            "range": "stddev: 0.009452240729802445",
            "extra": "mean: 159.75057446666483 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 15.468080177507039,
            "unit": "iter/sec",
            "range": "stddev: 0.02180813388385356",
            "extra": "mean: 64.64926406666507 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 15.098640209164925,
            "unit": "iter/sec",
            "range": "stddev: 0.02387598045953358",
            "extra": "mean: 66.23112983333404 msec\nrounds: 30"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "matthijs@terminusdb.com",
            "name": "Matthijs van Otterdijk",
            "username": "matko"
          },
          "committer": {
            "email": "matthijs@terminusdb.com",
            "name": "Matthijs van Otterdijk",
            "username": "matko"
          },
          "distinct": true,
          "id": "7248eae776d7b32dd7386b21769182feefd29b70",
          "message": "raise store dependency to 0.16.4",
          "timestamp": "2021-02-25T15:08:48+01:00",
          "tree_id": "549a96b0d9ab28715070bde5b8cf899a109d7c1d",
          "url": "https://github.com/terminusdb/terminusdb/commit/7248eae776d7b32dd7386b21769182feefd29b70"
        },
        "date": 1614262243594,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 5.863721890417999,
            "unit": "iter/sec",
            "range": "stddev: 0.016136617399101636",
            "extra": "mean: 170.54014816666458 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.893237640166416,
            "unit": "iter/sec",
            "range": "stddev: 0.006811483747374047",
            "extra": "mean: 169.6860131999974 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 14.740542571070499,
            "unit": "iter/sec",
            "range": "stddev: 0.02293836574160839",
            "extra": "mean: 67.84010799999862 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 14.61238734197711,
            "unit": "iter/sec",
            "range": "stddev: 0.02464671940023105",
            "extra": "mean: 68.43508706666248 msec\nrounds: 30"
          }
        ]
      },
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
          "id": "3b164ce9e904168f73a647f45ce56180e8ab282b",
          "message": "increase package.json version to 4.2.0 as well",
          "timestamp": "2021-02-25T15:59:52+01:00",
          "tree_id": "d57b8fc060080f73af7a4d96e8d918d22d8005cc",
          "url": "https://github.com/terminusdb/terminusdb/commit/3b164ce9e904168f73a647f45ce56180e8ab282b"
        },
        "date": 1614265324711,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 7.740522675681922,
            "unit": "iter/sec",
            "range": "stddev: 0.012660156779665968",
            "extra": "mean: 129.19024230000105 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 7.490830990651685,
            "unit": "iter/sec",
            "range": "stddev: 0.006688022829022412",
            "extra": "mean: 133.4965374666666 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 20.120478042276794,
            "unit": "iter/sec",
            "range": "stddev: 0.017612311432712465",
            "extra": "mean: 49.700608400000114 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 19.31810961162503,
            "unit": "iter/sec",
            "range": "stddev: 0.015126277312465278",
            "extra": "mean: 51.76489936666637 msec\nrounds: 30"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "dk@trick.ca",
            "name": "Dmytri Kleiner",
            "username": "dmytri"
          },
          "committer": {
            "email": "dk@trick.ca",
            "name": "Dmytri Kleiner",
            "username": "dmytri"
          },
          "distinct": true,
          "id": "be8f77e59b864a0aaa78c72ff6ee4d4567e5f2f1",
          "message": "gittalk creds",
          "timestamp": "2021-02-26T15:50:02+01:00",
          "tree_id": "16634b94fdf976fab2459020f7acfd52174fde56",
          "url": "https://github.com/terminusdb/terminusdb/commit/be8f77e59b864a0aaa78c72ff6ee4d4567e5f2f1"
        },
        "date": 1614351389830,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 5.992616783308362,
            "unit": "iter/sec",
            "range": "stddev: 0.01377626868482685",
            "extra": "mean: 166.87200870000015 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.596513386934817,
            "unit": "iter/sec",
            "range": "stddev: 0.014788880786403072",
            "extra": "mean: 178.68267809999736 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 14.636858080030139,
            "unit": "iter/sec",
            "range": "stddev: 0.026344565218533577",
            "extra": "mean: 68.32067336666701 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 15.268046643717186,
            "unit": "iter/sec",
            "range": "stddev: 0.017418381990942106",
            "extra": "mean: 65.49626310000178 msec\nrounds: 30"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "matthijs@terminusdb.com",
            "name": "Matthijs van Otterdijk",
            "username": "matko"
          },
          "committer": {
            "email": "matthijs@terminusdb.com",
            "name": "Matthijs van Otterdijk",
            "username": "matko"
          },
          "distinct": true,
          "id": "75038799becd6975a61acd30c0256efa71f466f4",
          "message": "add a bunch of tests to test marshalling of data to store literals",
          "timestamp": "2021-02-26T17:13:57+01:00",
          "tree_id": "cffc7840cde009295a52ce7325eee5ae9c69e714",
          "url": "https://github.com/terminusdb/terminusdb/commit/75038799becd6975a61acd30c0256efa71f466f4"
        },
        "date": 1614356134192,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 8.459850186069763,
            "unit": "iter/sec",
            "range": "stddev: 0.011920379434636846",
            "extra": "mean: 118.205402933332 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 8.19675299662888,
            "unit": "iter/sec",
            "range": "stddev: 0.004035131224766856",
            "extra": "mean: 121.99952839999877 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 18.765533968277417,
            "unit": "iter/sec",
            "range": "stddev: 0.01585098247311204",
            "extra": "mean: 53.28918440000005 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 18.23491393006029,
            "unit": "iter/sec",
            "range": "stddev: 0.019399173084400066",
            "extra": "mean: 54.83985303333393 msec\nrounds: 30"
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
          "id": "0e786df116124ca36618e7a746ee8153f22dc1df",
          "message": "Add higher argument mapping utils.",
          "timestamp": "2021-03-03T13:57:30+01:00",
          "tree_id": "84ca5d657cf16a19fa374ab68b477cf4f87fd184",
          "url": "https://github.com/terminusdb/terminusdb/commit/0e786df116124ca36618e7a746ee8153f22dc1df"
        },
        "date": 1614776366119,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 7.21547369416678,
            "unit": "iter/sec",
            "range": "stddev: 0.013952970301921197",
            "extra": "mean: 138.59103953333403 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 7.165816915035489,
            "unit": "iter/sec",
            "range": "stddev: 0.0050879049596612635",
            "extra": "mean: 139.5514303333338 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 16.16838905515194,
            "unit": "iter/sec",
            "range": "stddev: 0.020215765644702694",
            "extra": "mean: 61.84908073333115 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 12.782898442609902,
            "unit": "iter/sec",
            "range": "stddev: 0.018082589005712357",
            "extra": "mean: 78.22951926666708 msec\nrounds: 30"
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
          "id": "e9aa857dae69b5686bc4e9b9ce998d0c68b0403c",
          "message": "Add fixes to typeof",
          "timestamp": "2021-03-04T22:44:43+01:00",
          "tree_id": "d65ac76b97de6853cf3ab2cb214ac6ce25e903f2",
          "url": "https://github.com/terminusdb/terminusdb/commit/e9aa857dae69b5686bc4e9b9ce998d0c68b0403c"
        },
        "date": 1614894395533,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.431058795365317,
            "unit": "iter/sec",
            "range": "stddev: 0.015080125770656486",
            "extra": "mean: 155.4953906999998 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 6.507577349095891,
            "unit": "iter/sec",
            "range": "stddev: 0.00953387960393686",
            "extra": "mean: 153.66701713333174 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 15.238379718810451,
            "unit": "iter/sec",
            "range": "stddev: 0.020769256622270336",
            "extra": "mean: 65.6237748666669 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 11.604126307870121,
            "unit": "iter/sec",
            "range": "stddev: 0.022602636616914334",
            "extra": "mean: 86.1762422666653 msec\nrounds: 30"
          }
        ]
      },
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
          "id": "2f45e170c90a5de0a80b5b62f7276d002b58aeac",
          "message": "ci: only run tests on mac but don't distribute desktop anymore",
          "timestamp": "2021-03-11T13:28:05+01:00",
          "tree_id": "f8942584f3d51fd1cb6f8d070767c5eb4a8c9d7c",
          "url": "https://github.com/terminusdb/terminusdb/commit/2f45e170c90a5de0a80b5b62f7276d002b58aeac"
        },
        "date": 1615465803504,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 5.717131204536627,
            "unit": "iter/sec",
            "range": "stddev: 0.017141872723843894",
            "extra": "mean: 174.91290023333477 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.7005346164871655,
            "unit": "iter/sec",
            "range": "stddev: 0.011452408457222406",
            "extra": "mean: 175.4221432333357 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 15.674835477944844,
            "unit": "iter/sec",
            "range": "stddev: 0.026039061875648348",
            "extra": "mean: 63.7965228666701 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 10.913888956296473,
            "unit": "iter/sec",
            "range": "stddev: 0.020701456772428756",
            "extra": "mean: 91.62636746666522 msec\nrounds: 30"
          }
        ]
      },
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
          "id": "8df30ceb2de3816d89b8133b08c09fb498ca973e",
          "message": "ci: remove desktop from swi prolog cache key",
          "timestamp": "2021-03-11T13:30:00+01:00",
          "tree_id": "dd7e165ecc26a648666df8951bde77ef1eaa8de3",
          "url": "https://github.com/terminusdb/terminusdb/commit/8df30ceb2de3816d89b8133b08c09fb498ca973e"
        },
        "date": 1615465914073,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.46846211889008,
            "unit": "iter/sec",
            "range": "stddev: 0.016527047947686304",
            "extra": "mean: 154.59625203333331 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 6.348790923294425,
            "unit": "iter/sec",
            "range": "stddev: 0.009078792698431433",
            "extra": "mean: 157.51030583333403 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 15.350847966898062,
            "unit": "iter/sec",
            "range": "stddev: 0.023105649928216343",
            "extra": "mean: 65.14298116666642 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 11.257516932529763,
            "unit": "iter/sec",
            "range": "stddev: 0.02145578621491349",
            "extra": "mean: 88.82953549999968 msec\nrounds: 30"
          }
        ]
      },
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
          "id": "230e5b4542550661920f5dcb2cf1c7853602d664",
          "message": "Remove desktop builds entirely\n\nThese will be moved to the hub-desktop client",
          "timestamp": "2021-03-11T13:41:05+01:00",
          "tree_id": "7f92e1b58078fc662175ecef7cd05a7a830a9df4",
          "url": "https://github.com/terminusdb/terminusdb/commit/230e5b4542550661920f5dcb2cf1c7853602d664"
        },
        "date": 1615466585915,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 5.350259581950435,
            "unit": "iter/sec",
            "range": "stddev: 0.018448471256604683",
            "extra": "mean: 186.90681913333455 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.633347158988158,
            "unit": "iter/sec",
            "range": "stddev: 0.011609356150763072",
            "extra": "mean: 177.51435723333202 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 15.037646967228858,
            "unit": "iter/sec",
            "range": "stddev: 0.029388429941143757",
            "extra": "mean: 66.4997656999977 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 10.141418113681645,
            "unit": "iter/sec",
            "range": "stddev: 0.025207848152709243",
            "extra": "mean: 98.60553906666307 msec\nrounds: 30"
          }
        ]
      },
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
          "id": "733c2fd1256ecde946e675caa0eaeb6b652fdc40",
          "message": "Revert \"ci: only run tests on mac but don't distribute desktop anymore\"\n\nThis reverts commit 2f45e170c90a5de0a80b5b62f7276d002b58aeac.",
          "timestamp": "2021-03-11T15:25:52+01:00",
          "tree_id": "d65ac76b97de6853cf3ab2cb214ac6ce25e903f2",
          "url": "https://github.com/terminusdb/terminusdb/commit/733c2fd1256ecde946e675caa0eaeb6b652fdc40"
        },
        "date": 1615472882102,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 7.867182843686954,
            "unit": "iter/sec",
            "range": "stddev: 0.013331017517695979",
            "extra": "mean: 127.11030363333339 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 7.905307169785398,
            "unit": "iter/sec",
            "range": "stddev: 0.008649878239196274",
            "extra": "mean: 126.49729839999962 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 18.717959088978155,
            "unit": "iter/sec",
            "range": "stddev: 0.01692677578831291",
            "extra": "mean: 53.424627933332644 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 14.353956006902242,
            "unit": "iter/sec",
            "range": "stddev: 0.014982683612479038",
            "extra": "mean: 69.66720529999813 msec\nrounds: 30"
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
          "id": "e28b1f46bad3446089e8ac40011313da7da47af3",
          "message": "All tests passing.",
          "timestamp": "2021-03-12T12:16:43+01:00",
          "tree_id": "f80f77af125f3c0d750ace9cfa6612dc2d4df249",
          "url": "https://github.com/terminusdb/terminusdb/commit/e28b1f46bad3446089e8ac40011313da7da47af3"
        },
        "date": 1615548404757,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.501301635785926,
            "unit": "iter/sec",
            "range": "stddev: 0.018001050481324513",
            "extra": "mean: 153.8153520666654 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 6.576371387884312,
            "unit": "iter/sec",
            "range": "stddev: 0.008951037094827393",
            "extra": "mean: 152.05953876666786 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 15.416012441930999,
            "unit": "iter/sec",
            "range": "stddev: 0.021308980448550688",
            "extra": "mean: 64.86761759999855 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 15.337250423806463,
            "unit": "iter/sec",
            "range": "stddev: 0.022269519627741086",
            "extra": "mean: 65.20073496666659 msec\nrounds: 30"
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
          "id": "1d71133af79ac35510953066b94d6210f34aee91",
          "message": "Fix terminusdb/terminusdb#239",
          "timestamp": "2021-03-12T14:10:44+01:00",
          "tree_id": "a72f5f22ba73616337b59741ed401e273945746c",
          "url": "https://github.com/terminusdb/terminusdb/commit/1d71133af79ac35510953066b94d6210f34aee91"
        },
        "date": 1615554810202,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 7.617276659776903,
            "unit": "iter/sec",
            "range": "stddev: 0.013774405049231145",
            "extra": "mean: 131.28051463333463 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 7.658853017167209,
            "unit": "iter/sec",
            "range": "stddev: 0.006440067196335664",
            "extra": "mean: 130.56785366666713 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 18.69985484324969,
            "unit": "iter/sec",
            "range": "stddev: 0.016313188186508183",
            "extra": "mean: 53.47635093333262 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 18.305491111531307,
            "unit": "iter/sec",
            "range": "stddev: 0.01969520431044741",
            "extra": "mean: 54.628416900001284 msec\nrounds: 30"
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
          "id": "08cb37962d9f149acdb56aaebea8b13420a148af",
          "message": "Fixing csv missing files.",
          "timestamp": "2021-03-15T17:04:07+01:00",
          "tree_id": "87baf63c6c424206c672e0d91af376f3272ff34c",
          "url": "https://github.com/terminusdb/terminusdb/commit/08cb37962d9f149acdb56aaebea8b13420a148af"
        },
        "date": 1615824473774,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.288330684757577,
            "unit": "iter/sec",
            "range": "stddev: 0.01743880167766556",
            "extra": "mean: 159.0247158000011 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 6.03841190327305,
            "unit": "iter/sec",
            "range": "stddev: 0.007339656497453308",
            "extra": "mean: 165.60645680000096 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 17.358047164657286,
            "unit": "iter/sec",
            "range": "stddev: 0.026182133097935757",
            "extra": "mean: 57.6101672333337 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 16.26937531837578,
            "unit": "iter/sec",
            "range": "stddev: 0.022735081207393707",
            "extra": "mean: 61.46517493333192 msec\nrounds: 30"
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
          "id": "77eb8baa7f134ee32367a04b6fa6b26f1fe1c41e",
          "message": "Add random IDGen to server",
          "timestamp": "2021-03-15T17:13:20+01:00",
          "tree_id": "5e74a6450db487daa2377fd0bde1890955bc9285",
          "url": "https://github.com/terminusdb/terminusdb/commit/77eb8baa7f134ee32367a04b6fa6b26f1fe1c41e"
        },
        "date": 1615824958123,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 5.611227960610683,
            "unit": "iter/sec",
            "range": "stddev: 0.018844203817637472",
            "extra": "mean: 178.21411053333284 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.3737354745109585,
            "unit": "iter/sec",
            "range": "stddev: 0.015387094131310005",
            "extra": "mean: 186.09029133333098 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 15.236157098305918,
            "unit": "iter/sec",
            "range": "stddev: 0.023768572576016205",
            "extra": "mean: 65.6333479333308 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 15.212812876027396,
            "unit": "iter/sec",
            "range": "stddev: 0.024670692778741503",
            "extra": "mean: 65.73406300000026 msec\nrounds: 30"
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
          "id": "3e424bdf365a162305350e965cd0d056969e302f",
          "message": "More datamarshalling fixes and more tests.",
          "timestamp": "2021-03-17T23:54:13+01:00",
          "tree_id": "a47b776632c7ef0c34870eb71883d4a4440cd88a",
          "url": "https://github.com/terminusdb/terminusdb/commit/3e424bdf365a162305350e965cd0d056969e302f"
        },
        "date": 1616021785139,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 5.606160576220271,
            "unit": "iter/sec",
            "range": "stddev: 0.0258598223756975",
            "extra": "mean: 178.37519749999916 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.694556583498197,
            "unit": "iter/sec",
            "range": "stddev: 0.01948028040594999",
            "extra": "mean: 175.60629793333175 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 11.802452871283986,
            "unit": "iter/sec",
            "range": "stddev: 0.07892255315385555",
            "extra": "mean: 84.72815023333453 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 14.093244516410119,
            "unit": "iter/sec",
            "range": "stddev: 0.021223395839931884",
            "extra": "mean: 70.95598169999846 msec\nrounds: 30"
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
          "id": "f770459a97182a9f15ae9b0d7a0e8d1d5a9accf8",
          "message": "more robust JSON construction - more tests",
          "timestamp": "2021-03-18T01:35:17+01:00",
          "tree_id": "0d87ad6e525a30d1a6dc6de808d94cf406c68c07",
          "url": "https://github.com/terminusdb/terminusdb/commit/f770459a97182a9f15ae9b0d7a0e8d1d5a9accf8"
        },
        "date": 1616027841361,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 8.460061818703856,
            "unit": "iter/sec",
            "range": "stddev: 0.011547548364360739",
            "extra": "mean: 118.2024459666664 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 8.229402896591438,
            "unit": "iter/sec",
            "range": "stddev: 0.007757628646170088",
            "extra": "mean: 121.51549906666901 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 19.34848190949824,
            "unit": "iter/sec",
            "range": "stddev: 0.019539773762301173",
            "extra": "mean: 51.68364136666952 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 18.546585088630554,
            "unit": "iter/sec",
            "range": "stddev: 0.01724252493883222",
            "extra": "mean: 53.91828173333219 msec\nrounds: 30"
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
          "id": "044e4e2b2d831691a430fb47559dd2bc23c053fa",
          "message": "Add date result to test, remove extraneous prints",
          "timestamp": "2021-03-18T02:17:44+01:00",
          "tree_id": "084b0519a067c54a0bd834dd18f65c852b4429ff",
          "url": "https://github.com/terminusdb/terminusdb/commit/044e4e2b2d831691a430fb47559dd2bc23c053fa"
        },
        "date": 1616073921419,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 5.667872340060457,
            "unit": "iter/sec",
            "range": "stddev: 0.01453222127905833",
            "extra": "mean: 176.43304930000122 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.569535551855858,
            "unit": "iter/sec",
            "range": "stddev: 0.010399379802572143",
            "extra": "mean: 179.54818506666754 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 14.162881028101998,
            "unit": "iter/sec",
            "range": "stddev: 0.024658256604483064",
            "extra": "mean: 70.60710303333053 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 13.751281810836604,
            "unit": "iter/sec",
            "range": "stddev: 0.029938036876287644",
            "extra": "mean: 72.72049353333425 msec\nrounds: 30"
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
          "id": "ea0f8b8d069ed04804a747afba984629113c982f",
          "message": "Adding release notes for v4.2.1",
          "timestamp": "2021-03-18T15:22:46+01:00",
          "tree_id": "6196e9e7b6d7b5800e86b5910271988dc8cc9fd4",
          "url": "https://github.com/terminusdb/terminusdb/commit/ea0f8b8d069ed04804a747afba984629113c982f"
        },
        "date": 1616077488127,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 5.785886730431418,
            "unit": "iter/sec",
            "range": "stddev: 0.01541717241263556",
            "extra": "mean: 172.83435480000077 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.592556222362888,
            "unit": "iter/sec",
            "range": "stddev: 0.01768183451368517",
            "extra": "mean: 178.8091098666674 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 15.39019774871333,
            "unit": "iter/sec",
            "range": "stddev: 0.022878827368017243",
            "extra": "mean: 64.97642306666289 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 14.538330305427497,
            "unit": "iter/sec",
            "range": "stddev: 0.030721631570006173",
            "extra": "mean: 68.78368966666528 msec\nrounds: 30"
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
          "id": "5cfbdfee3457021c793a55e5dcb85905b89aae07",
          "message": "Adding documentation",
          "timestamp": "2021-03-19T02:24:02+01:00",
          "tree_id": "e6b7069e24d0e1d2a013b1a55025a7d5292e8794",
          "url": "https://github.com/terminusdb/terminusdb/commit/5cfbdfee3457021c793a55e5dcb85905b89aae07"
        },
        "date": 1616117156463,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 5.92093482873747,
            "unit": "iter/sec",
            "range": "stddev: 0.017379579466427913",
            "extra": "mean: 168.89224910000092 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.761788568322847,
            "unit": "iter/sec",
            "range": "stddev: 0.020644503952837623",
            "extra": "mean: 173.55721893333583 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 14.450929992922974,
            "unit": "iter/sec",
            "range": "stddev: 0.027490131998372545",
            "extra": "mean: 69.19969860000208 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 14.283101083546423,
            "unit": "iter/sec",
            "range": "stddev: 0.033362930164657516",
            "extra": "mean: 70.01280703333825 msec\nrounds: 30"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "gavin@datachemist.com",
            "name": "Gavin Mendel-Gleason",
            "username": "GavinMendelGleason"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "631b50b6f206774dfdf04f4940f82f1715d924d0",
          "message": "Merge pull request #259 from rrooij/dev\n\nFix very slow query when using ./terminusdb list with many dbs",
          "timestamp": "2021-03-19T23:55:38+01:00",
          "tree_id": "2a16ab5e5a65d30d680cdefab211a13bb07b3408",
          "url": "https://github.com/terminusdb/terminusdb/commit/631b50b6f206774dfdf04f4940f82f1715d924d0"
        },
        "date": 1616194639270,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.03227531507222,
            "unit": "iter/sec",
            "range": "stddev: 0.012454662344719848",
            "extra": "mean: 165.77492700000013 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.871370767800115,
            "unit": "iter/sec",
            "range": "stddev: 0.010453491364765252",
            "extra": "mean: 170.31797846666734 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 16.337917273674996,
            "unit": "iter/sec",
            "range": "stddev: 0.026003177484749996",
            "extra": "mean: 61.20731199999909 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 16.07135864092781,
            "unit": "iter/sec",
            "range": "stddev: 0.021980568509993092",
            "extra": "mean: 62.222492966672384 msec\nrounds: 30"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "gavin@datachemist.com",
            "name": "Gavin Mendel-Gleason",
            "username": "GavinMendelGleason"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "2869cbe88815020067ef337b2ced108463ce7112",
          "message": "Merge pull request #273 from terminusdb/nonpersistent\n\nimplement support for starting TerminusDB in in-memory mode",
          "timestamp": "2021-03-23T13:43:09+01:00",
          "tree_id": "34318948dd78f477db04a2588c32248b65516607",
          "url": "https://github.com/terminusdb/terminusdb/commit/2869cbe88815020067ef337b2ced108463ce7112"
        },
        "date": 1616503488860,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.849053845892309,
            "unit": "iter/sec",
            "range": "stddev: 0.016770956682896743",
            "extra": "mean: 146.00556843333123 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 6.994453017093498,
            "unit": "iter/sec",
            "range": "stddev: 0.00466923249276947",
            "extra": "mean: 142.97043636666587 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 15.85378563367824,
            "unit": "iter/sec",
            "range": "stddev: 0.020588621294128792",
            "extra": "mean: 63.07641739999922 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 16.237464166859343,
            "unit": "iter/sec",
            "range": "stddev: 0.018396334671626286",
            "extra": "mean: 61.58597116666774 msec\nrounds: 30"
          }
        ]
      },
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
          "id": "9f1e4ef11f0539bba6af60df22b7175cc00f3b5f",
          "message": "ci: try to fix for pull requests",
          "timestamp": "2021-03-24T11:00:52+01:00",
          "tree_id": "b2644738655ce23a84e5fecfa19af261f977ee7d",
          "url": "https://github.com/terminusdb/terminusdb/commit/9f1e4ef11f0539bba6af60df22b7175cc00f3b5f"
        },
        "date": 1616580168035,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.41160791007379,
            "unit": "iter/sec",
            "range": "stddev: 0.01684861073039696",
            "extra": "mean: 155.96711683333285 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 6.448631448509153,
            "unit": "iter/sec",
            "range": "stddev: 0.008882776033407013",
            "extra": "mean: 155.07166256666568 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 15.2356005850395,
            "unit": "iter/sec",
            "range": "stddev: 0.0228836187757014",
            "extra": "mean: 65.63574533333092 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 15.038795829741604,
            "unit": "iter/sec",
            "range": "stddev: 0.02265179796555951",
            "extra": "mean: 66.49468556666895 msec\nrounds: 30"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "gavin@datachemist.com",
            "name": "Gavin Mendel-Gleason",
            "username": "GavinMendelGleason"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "4a7833b748bf8d5b69ce38f23dd66fc214f5f5d3",
          "message": "Merge pull request #260 from rrooij/dev\n\nRefactor and optimize list db",
          "timestamp": "2021-03-25T11:18:16+01:00",
          "tree_id": "5db31e55240d0fb38098c775c65cb71500819553",
          "url": "https://github.com/terminusdb/terminusdb/commit/4a7833b748bf8d5b69ce38f23dd66fc214f5f5d3"
        },
        "date": 1616667591026,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.560750225567315,
            "unit": "iter/sec",
            "range": "stddev: 0.031294536635979885",
            "extra": "mean: 152.42159289999933 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 6.669254838479165,
            "unit": "iter/sec",
            "range": "stddev: 0.00797906742047806",
            "extra": "mean: 149.94178873333271 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 16.530015753882743,
            "unit": "iter/sec",
            "range": "stddev: 0.02101992921286258",
            "extra": "mean: 60.49601009999701 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 16.86083831002268,
            "unit": "iter/sec",
            "range": "stddev: 0.01601113414943079",
            "extra": "mean: 59.309032066665665 msec\nrounds: 30"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "matthijs@terminusdb.com",
            "name": "Matthijs van Otterdijk",
            "username": "matko"
          },
          "committer": {
            "email": "matthijs@terminusdb.com",
            "name": "Matthijs van Otterdijk",
            "username": "matko"
          },
          "distinct": true,
          "id": "a4a72d315d02326bf6160bdde374086d2ed65fd1",
          "message": "add memory flag to release notes",
          "timestamp": "2021-03-25T15:08:36+01:00",
          "tree_id": "e779a570f3272f706ad99bf40b435866faca7533",
          "url": "https://github.com/terminusdb/terminusdb/commit/a4a72d315d02326bf6160bdde374086d2ed65fd1"
        },
        "date": 1616681421623,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.031830855691835,
            "unit": "iter/sec",
            "range": "stddev: 0.01752212533302523",
            "extra": "mean: 165.78714223333483 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.909325879556599,
            "unit": "iter/sec",
            "range": "stddev: 0.01747088572989102",
            "extra": "mean: 169.2240401666652 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 14.692397490470116,
            "unit": "iter/sec",
            "range": "stddev: 0.024134799864179973",
            "extra": "mean: 68.06241123333525 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 14.564228715417574,
            "unit": "iter/sec",
            "range": "stddev: 0.023370342631458936",
            "extra": "mean: 68.66137710000449 msec\nrounds: 30"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "matthijs@terminusdb.com",
            "name": "Matthijs van Otterdijk",
            "username": "matko"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "58543d85e1b43b4c89bb6f3ed88600cf97d2fdaf",
          "message": "Merge pull request #285 from terminusdb/error_handling\n\nAdding error handling for database_does_not_exist.",
          "timestamp": "2021-04-06T17:15:13+02:00",
          "tree_id": "4300cf7cde49cf06299db5d7708d5781210d7ab4",
          "url": "https://github.com/terminusdb/terminusdb/commit/58543d85e1b43b4c89bb6f3ed88600cf97d2fdaf"
        },
        "date": 1617722208897,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 5.775438614934784,
            "unit": "iter/sec",
            "range": "stddev: 0.01725620038270369",
            "extra": "mean: 173.14702253333394 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.646718728967836,
            "unit": "iter/sec",
            "range": "stddev: 0.009692956008557838",
            "extra": "mean: 177.09399883333484 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 14.996130450985097,
            "unit": "iter/sec",
            "range": "stddev: 0.02553568653540865",
            "extra": "mean: 66.68386909999906 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 14.444920211965977,
            "unit": "iter/sec",
            "range": "stddev: 0.025933399460621506",
            "extra": "mean: 69.22848900000247 msec\nrounds: 30"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "gavin@datachemist.com",
            "name": "Gavin Mendel-Gleason",
            "username": "GavinMendelGleason"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "3711fb85e63b211a8913124d705e59a17cdf9f4e",
          "message": "Merge pull request #293 from terminusdb/store_upgrade\n\nUpping terminus store prolog version",
          "timestamp": "2021-04-08T15:44:26+02:00",
          "tree_id": "acd3b8da6c81a60fc50f6e3113fd0565ffe54688",
          "url": "https://github.com/terminusdb/terminusdb/commit/3711fb85e63b211a8913124d705e59a17cdf9f4e"
        },
        "date": 1617889559000,
        "tool": "pytest",
        "benches": [
          {
            "name": "benchmarks.py::test_insert_triple_speed",
            "value": 6.242122585645874,
            "unit": "iter/sec",
            "range": "stddev: 0.015719005510055235",
            "extra": "mean: 160.2019163000032 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_bulk_update",
            "value": 5.934526514885939,
            "unit": "iter/sec",
            "range": "stddev: 0.01636987181645085",
            "extra": "mean: 168.5054397333365 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_create_schema",
            "value": 15.662052394406805,
            "unit": "iter/sec",
            "range": "stddev: 0.027763132836439434",
            "extra": "mean: 63.848592433333806 msec\nrounds: 30"
          },
          {
            "name": "benchmarks.py::test_schema_bulk_update",
            "value": 16.191207137639534,
            "unit": "iter/sec",
            "range": "stddev: 0.020369636184049186",
            "extra": "mean: 61.761917533332664 msec\nrounds: 30"
          }
        ]
      }
    ]
  }
}