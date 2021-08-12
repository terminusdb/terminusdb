window.BENCHMARK_DATA = {
  "lastUpdate": 1628768132569,
  "repoUrl": "https://github.com/terminusdb/terminusdb",
  "entries": {
    "Benchmark": [
      {
        "commit": {
          "author": {
            "email": "sean@terminusdb.com",
            "name": "Sean Leather",
            "username": "spl"
          },
          "committer": {
            "email": "sean@terminusdb.com",
            "name": "Sean Leather",
            "username": "spl"
          },
          "distinct": true,
          "id": "be6dcdb1a14d3e0a951fca500969d7b4480f3203",
          "message": "Use terminusdb-labs/benchmark-tracker in CI\n\nReplace Python benchmark with:\nhttps://github.com/terminusdb-labs/terminusdb-http-perf",
          "timestamp": "2021-08-06T14:45:22+02:00",
          "tree_id": "badec8bb8e82e405a3fa182f59e34cf261db2d17",
          "url": "https://github.com/terminusdb/terminusdb/commit/be6dcdb1a14d3e0a951fca500969d7b4480f3203"
        },
        "date": 1628254171068,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 123.237,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 372.157,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 367.09,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 203.454,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 214.558,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 335.838,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 52.094,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 40.99,
            "unit": "ms"
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
          "id": "7d163fb31f3d98c49d26323030260e7a8fbf397f",
          "message": "change create_db predicate argument order to match reality",
          "timestamp": "2021-08-09T13:37:01+02:00",
          "tree_id": "96b5044b6a58999c1be7574c53c26926263f5cbc",
          "url": "https://github.com/terminusdb/terminusdb/commit/7d163fb31f3d98c49d26323030260e7a8fbf397f"
        },
        "date": 1628509283542,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 112.986,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 357.816,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 317.284,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 188.51,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 190.852,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 314.788,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 49.97,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 41.795,
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
            "email": "gavin@terminusdb.com",
            "name": "Gavin Mendel-Gleason",
            "username": "GavinMendelGleason"
          },
          "distinct": true,
          "id": "7d8b3bf3edf05e9637b4727e0fae1461c54e53bf",
          "message": "Milisecond arithmetic was inverted",
          "timestamp": "2021-08-10T11:05:11+02:00",
          "tree_id": "d5b4a5c5e19eba94e28c7631b450534578d442f6",
          "url": "https://github.com/terminusdb/terminusdb/commit/7d8b3bf3edf05e9637b4727e0fae1461c54e53bf"
        },
        "date": 1628586512548,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 127.504,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 369.505,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 392.562,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 233.524,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 219.614,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 385.18,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 44.714,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 46.079,
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
            "email": "gavin@terminusdb.com",
            "name": "Gavin Mendel-Gleason",
            "username": "GavinMendelGleason"
          },
          "distinct": true,
          "id": "7c0999ccc3bead214b37612534a066f894e6859a",
          "message": "Checking in fix for nanoseconds in ISO 8601",
          "timestamp": "2021-08-10T11:17:41+02:00",
          "tree_id": "ace09adfaf986638a801576036f5d7d83b9de0d0",
          "url": "https://github.com/terminusdb/terminusdb/commit/7c0999ccc3bead214b37612534a066f894e6859a"
        },
        "date": 1628587277066,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 113.823,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 367.935,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 342.755,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 192.025,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 209.863,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 340.58,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 47.264,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 45.637,
            "unit": "ms"
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
          "id": "d24a70d0ad02d885aa23bfcdd47be6a2ed738c4d",
          "message": "when deleting documents, recognize lists of ids for deletion.\n\nFixes #387.",
          "timestamp": "2021-08-10T15:56:01+02:00",
          "tree_id": "6bcbe453c6cdedb951f9018b08a638767dcbeac7",
          "url": "https://github.com/terminusdb/terminusdb/commit/d24a70d0ad02d885aa23bfcdd47be6a2ed738c4d"
        },
        "date": 1628603984294,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 136.072,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 390.841,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 384.771,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 210.355,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 221.315,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 361.164,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 53.723,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 46.644,
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
            "email": "gavin@terminusdb.com",
            "name": "Gavin Mendel-Gleason",
            "username": "GavinMendelGleason"
          },
          "distinct": true,
          "id": "f8fd91c44ae581415f71fb418b4e1d33fb42b731",
          "message": "Additiional tests from frontend.",
          "timestamp": "2021-08-10T17:16:47+02:00",
          "tree_id": "c8598d1991f1d77bc6f64117e68bd5ea383c9e7c",
          "url": "https://github.com/terminusdb/terminusdb/commit/f8fd91c44ae581415f71fb418b4e1d33fb42b731"
        },
        "date": 1628608798472,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 124.145,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 397.675,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 414.335,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 265.288,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 316.024,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 348.412,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 46.452,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 40.579,
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
            "email": "gavin@terminusdb.com",
            "name": "Gavin Mendel-Gleason",
            "username": "GavinMendelGleason"
          },
          "distinct": true,
          "id": "013a606002aa70bd9467dcbc49d87ad099e29b52",
          "message": "Stop silently dying on bad fields",
          "timestamp": "2021-08-10T18:15:02+02:00",
          "tree_id": "9543aa3d2847108311d381ee6bd5b442fcf0c6ea",
          "url": "https://github.com/terminusdb/terminusdb/commit/013a606002aa70bd9467dcbc49d87ad099e29b52"
        },
        "date": 1628613227776,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 144.373,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 445.98,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 449.91,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 253.428,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 271.533,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 406.143,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 49.623,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 47.285,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "dani@terminusdb.com",
            "name": "AstroChelonian"
          },
          "committer": {
            "email": "dani@terminusdb.com",
            "name": "AstroChelonian"
          },
          "distinct": true,
          "id": "1487d0e5f0ca21e06669659bdc1ed40c5dde384a",
          "message": "bug fixed, tests added",
          "timestamp": "2021-08-10T17:38:36+01:00",
          "tree_id": "a136f7129334782ca18b1d64ec8cf1fa654f5eb4",
          "url": "https://github.com/terminusdb/terminusdb/commit/1487d0e5f0ca21e06669659bdc1ed40c5dde384a"
        },
        "date": 1628613813837,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 93.653,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 282.483,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 277.044,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 152.779,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 166.523,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 259.354,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 37.091,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 35.299,
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
            "email": "gavin@terminusdb.com",
            "name": "Gavin Mendel-Gleason",
            "username": "GavinMendelGleason"
          },
          "distinct": true,
          "id": "4be8175cd46e31a330f723d3e7bb936c11a55a41",
          "message": "Fixed typo and documentation now more picky.",
          "timestamp": "2021-08-11T10:13:47+02:00",
          "tree_id": "a782a3b3893ffa4584e6480a4106a75b23d110e1",
          "url": "https://github.com/terminusdb/terminusdb/commit/4be8175cd46e31a330f723d3e7bb936c11a55a41"
        },
        "date": 1628669881393,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 149.835,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 396.247,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 473.265,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 240.1,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 284.298,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 368.558,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 49.588,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 44.063,
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
            "email": "sean@terminusdb.com",
            "name": "Sean Leather",
            "username": "spl"
          },
          "distinct": true,
          "id": "ce777196997b5f9b758b32f3ce73e28e67575734",
          "message": "Add test replace_schema_document_lexical_key",
          "timestamp": "2021-08-11T12:38:14+02:00",
          "tree_id": "515d3592b744db927212271b8ef2b4ae14b5487f",
          "url": "https://github.com/terminusdb/terminusdb/commit/ce777196997b5f9b758b32f3ce73e28e67575734"
        },
        "date": 1628678436356,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 129.59,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 373.405,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 388.409,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 211.373,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 222.916,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 306.062,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 42.153,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 41.408,
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
            "email": "sean@terminusdb.com",
            "name": "Sean Leather",
            "username": "spl"
          },
          "distinct": true,
          "id": "abd7139156edc333c4f903e509d2673554446a96",
          "message": "Fix generic_exception_jsonld to return only a JSON object",
          "timestamp": "2021-08-11T13:37:52+02:00",
          "tree_id": "e76da1bc273e1c216cc9257e57fbba3e53d42850",
          "url": "https://github.com/terminusdb/terminusdb/commit/abd7139156edc333c4f903e509d2673554446a96"
        },
        "date": 1628682005765,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 132.608,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 403.779,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 411.857,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 222.909,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 239.599,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 385.684,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 50.623,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 47.157,
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
            "email": "sean@terminusdb.com",
            "name": "Sean Leather",
            "username": "spl"
          },
          "distinct": false,
          "id": "f8833762796b7ce84f4cc13e5e244c0941f24866",
          "message": "delete_schema_subdocument: fix the garbage leak",
          "timestamp": "2021-08-11T14:01:02+02:00",
          "tree_id": "35b89499be05efd4b815a0c769fd95be6549c871",
          "url": "https://github.com/terminusdb/terminusdb/commit/f8833762796b7ce84f4cc13e5e244c0941f24866"
        },
        "date": 1628683468380,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 152.865,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 448.234,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 555.793,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 269.616,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 266.902,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 399.862,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 48.179,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 42.822,
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
          "id": "f3454c5ac5f58807ea4015e954d180f3aede5f60",
          "message": "Merge pull request #396 from terminusdb/bundle_bug\n\nbug fixed, tests added",
          "timestamp": "2021-08-11T14:55:39+02:00",
          "tree_id": "d70257bd27a7f66a9ba25a9aab60d7498efe56a4",
          "url": "https://github.com/terminusdb/terminusdb/commit/f3454c5ac5f58807ea4015e954d180f3aede5f60"
        },
        "date": 1628686711346,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 90.771,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 271.71,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 277.643,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 155.59,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 171.854,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 254.843,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 38.73,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 33.047,
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
            "email": "sean@terminusdb.com",
            "name": "Sean Leather",
            "username": "spl"
          },
          "distinct": true,
          "id": "b9ce3e78d651197e7d9f9cfcac9d44a1b876a359",
          "message": "Add test.yml workflow for testing ubuntu, macos",
          "timestamp": "2021-08-12T10:37:35+02:00",
          "tree_id": "bc68176df986917deaa5fbd727113abc2d783b48",
          "url": "https://github.com/terminusdb/terminusdb/commit/b9ce3e78d651197e7d9f9cfcac9d44a1b876a359"
        },
        "date": 1628757603036,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 115.703,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 348.203,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 340.978,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 200.163,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 270.365,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 318.183,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 42.797,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 38.25,
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
            "email": "gavin@terminusdb.com",
            "name": "Gavin Mendel-Gleason",
            "username": "GavinMendelGleason"
          },
          "distinct": true,
          "id": "07157c8e3683ea087d7c2e17d347045a61748759",
          "message": "Adding requirement of valuehash or random for subdocument",
          "timestamp": "2021-08-12T10:40:24+02:00",
          "tree_id": "910bb36850713002d2b56d4530359a1d8e817044",
          "url": "https://github.com/terminusdb/terminusdb/commit/07157c8e3683ea087d7c2e17d347045a61748759"
        },
        "date": 1628757833013,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 95.492,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 287.422,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 284.534,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 169.388,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 175.777,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 287.897,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 44.382,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 38.752,
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
            "email": "sean@terminusdb.com",
            "name": "Sean Leather",
            "username": "spl"
          },
          "distinct": true,
          "id": "36575a3abb6d157212cbea849a589d884265d521",
          "message": "Remove singleton variables",
          "timestamp": "2021-08-12T12:02:51+02:00",
          "tree_id": "7f7956fefa033255f0d7196d482385de1029a37f",
          "url": "https://github.com/terminusdb/terminusdb/commit/36575a3abb6d157212cbea849a589d884265d521"
        },
        "date": 1628762667658,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 80.157,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 229.649,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 235.36,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 131.614,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 150.357,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 233.621,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 36.026,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 32.774,
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
            "email": "gavin@terminusdb.com",
            "name": "Gavin Mendel-Gleason",
            "username": "GavinMendelGleason"
          },
          "distinct": true,
          "id": "6a72c1af7f503e03ac628f8107c51b1bb3614407",
          "message": "Fixing tests to reflect no lexical for subdocument\n\nFixes #401",
          "timestamp": "2021-08-12T13:15:01+02:00",
          "tree_id": "c5c4d43cbf41e32bcf47f95bac31a0de67cfdd05",
          "url": "https://github.com/terminusdb/terminusdb/commit/6a72c1af7f503e03ac628f8107c51b1bb3614407"
        },
        "date": 1628767135565,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 120.43,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 324.603,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 347.729,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 185.737,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 208.751,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 314.712,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 43.756,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 41.075,
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
            "email": "gavin@terminusdb.com",
            "name": "Gavin Mendel-Gleason",
            "username": "GavinMendelGleason"
          },
          "distinct": true,
          "id": "74b473258f1a7701f995e5289c3dcdcade9298aa",
          "message": "Updating CLI documentation for canary",
          "timestamp": "2021-08-12T13:18:15+02:00",
          "tree_id": "7f0e590bec44a90798f22cbc6400401efd87c462",
          "url": "https://github.com/terminusdb/terminusdb/commit/74b473258f1a7701f995e5289c3dcdcade9298aa"
        },
        "date": 1628767292378,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 126.397,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 350.632,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 392.882,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 222.72,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 166.045,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 344.079,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 35.937,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 33.037,
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
            "email": "gavin@terminusdb.com",
            "name": "Gavin Mendel-Gleason",
            "username": "GavinMendelGleason"
          },
          "distinct": true,
          "id": "388dab7ac0f35de8246eeab71cdabd9fb3c4227c",
          "message": "Fix graph query documentation",
          "timestamp": "2021-08-12T13:20:46+02:00",
          "tree_id": "ed89666e81209dd6713fe0ab9a3bae49eebe4bcb",
          "url": "https://github.com/terminusdb/terminusdb/commit/388dab7ac0f35de8246eeab71cdabd9fb3c4227c"
        },
        "date": 1628767455085,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 119.266,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 378.627,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 385.905,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 220.537,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 239.234,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 429.596,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 45.25,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 42.743,
            "unit": "ms"
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
          "id": "d96b6c2787532fac5ad507e044f57384cb496b93",
          "message": "actually put database in deleting state when deleting a database",
          "timestamp": "2021-08-12T13:32:27+02:00",
          "tree_id": "92552a58af08b0173e520986ea17cf3f0188ee9d",
          "url": "https://github.com/terminusdb/terminusdb/commit/d96b6c2787532fac5ad507e044f57384cb496b93"
        },
        "date": 1628768131956,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 105.193,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 260.825,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 317.321,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 160.244,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 172.917,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 261.926,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 38.304,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 34.295,
            "unit": "ms"
          }
        ]
      }
    ]
  }
}