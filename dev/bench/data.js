window.BENCHMARK_DATA = {
  "lastUpdate": 1614894396106,
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
      }
    ]
  }
}