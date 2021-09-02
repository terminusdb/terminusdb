window.BENCHMARK_DATA = {
  "lastUpdate": 1630596799228,
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
          "id": "94c20853e49aeeeeeda08189c5c432937dda607b",
          "message": "Speed up and correctify path treatment.",
          "timestamp": "2021-08-12T15:07:32+02:00",
          "tree_id": "cab6dc906f1c8f05088e948b07e45bdfff68e3f2",
          "url": "https://github.com/terminusdb/terminusdb/commit/94c20853e49aeeeeeda08189c5c432937dda607b"
        },
        "date": 1628773840097,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 93.955,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 254.36,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 253.526,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 140.404,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 160.354,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 313.335,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 37.956,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 36.816,
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
          "id": "1915ed629795eecefc2de424f59cd29000130bcb",
          "message": "Checking in test for commit history",
          "timestamp": "2021-08-12T15:29:40+02:00",
          "tree_id": "5ecba6a9f2d5471dc2b43314a6253226e88fe950",
          "url": "https://github.com/terminusdb/terminusdb/commit/1915ed629795eecefc2de424f59cd29000130bcb"
        },
        "date": 1628775214056,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 132.024,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 380.737,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 370.928,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 213.315,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 224.519,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 335.185,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 46.503,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 42.709,
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
          "id": "08c5fd6b50d00c80c185366f5dc06a3a9b6de792",
          "message": "Fixing path to allow edges as optional",
          "timestamp": "2021-08-12T15:48:29+02:00",
          "tree_id": "72fb0efa90672488e33b2977169af5e496243d29",
          "url": "https://github.com/terminusdb/terminusdb/commit/08c5fd6b50d00c80c185366f5dc06a3a9b6de792"
        },
        "date": 1628776461320,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 95.851,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 240.754,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 320.094,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 139.589,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 152.596,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 249.591,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 37.979,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 36.134,
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
          "id": "d83da97dad0e1250b38a18e10dc5171b6ab8ebc7",
          "message": "Nanoseconds fixed",
          "timestamp": "2021-08-12T18:50:34+02:00",
          "tree_id": "2020b9724d9d5f9f4d48a574f1384fd7d0e5caf0",
          "url": "https://github.com/terminusdb/terminusdb/commit/d83da97dad0e1250b38a18e10dc5171b6ab8ebc7"
        },
        "date": 1628787296123,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 141.801,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 404.988,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 375.691,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 195.654,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 210.868,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 338.676,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 48.903,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 46.898,
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
          "id": "4353ccf6d470475961e4c593a8b294b3bb3d9999",
          "message": "issue resolved. The process exits with an error when two instances with the same ID are added with the same transaction.",
          "timestamp": "2021-08-12T18:15:18+01:00",
          "tree_id": "8fe2b3b0b6ecfe5d051d9679a622c45b23e4da4e",
          "url": "https://github.com/terminusdb/terminusdb/commit/4353ccf6d470475961e4c593a8b294b3bb3d9999"
        },
        "date": 1628788742667,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 89.84,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 265.433,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 272.52,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 158.251,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 165.994,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 85.871,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 38.132,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 33.806,
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
          "id": "368b1c7da4d73d8502d92340c56788c7832eaa12",
          "message": "Issue resolved - deleted redundant code -previously commented out",
          "timestamp": "2021-08-12T18:31:33+01:00",
          "tree_id": "db5bceb2688ddd130404cef1067e53a1902c567f",
          "url": "https://github.com/terminusdb/terminusdb/commit/368b1c7da4d73d8502d92340c56788c7832eaa12"
        },
        "date": 1628789723659,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 119.907,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 381.435,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 392.869,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 222.461,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 243.036,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 102.608,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 42.386,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 40.597,
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
          "id": "b7c0e7969198aedcada1e5169ba2319b0118cd63",
          "message": "Merge pull request #408 from terminusdb/date_nanoseconds\n\nNanoseconds fixed",
          "timestamp": "2021-08-13T09:49:07+02:00",
          "tree_id": "2020b9724d9d5f9f4d48a574f1384fd7d0e5caf0",
          "url": "https://github.com/terminusdb/terminusdb/commit/b7c0e7969198aedcada1e5169ba2319b0118cd63"
        },
        "date": 1628841183690,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 122.005,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 316.967,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 337.626,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 188.331,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 192.179,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 315.974,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 48.113,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 42.452,
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
          "id": "8ab6cfac066c9705787979a25f24b777f0a0ff67",
          "message": "white spaces removed",
          "timestamp": "2021-08-13T09:50:21+01:00",
          "tree_id": "a1c21291ff162c71688fac0821b084f7df2c17e1",
          "url": "https://github.com/terminusdb/terminusdb/commit/8ab6cfac066c9705787979a25f24b777f0a0ff67"
        },
        "date": 1628844859012,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 102.981,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 304.932,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 302.794,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 170.577,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 185.743,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 106.806,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 44.465,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 39.329,
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
          "id": "2d262cffcbc960d752f5540fd2c444fe86fa0c99",
          "message": "test commit",
          "timestamp": "2021-08-13T11:53:44+01:00",
          "tree_id": "a1f04f1ac7d155013756e51672a183a2005c225f",
          "url": "https://github.com/terminusdb/terminusdb/commit/2d262cffcbc960d752f5540fd2c444fe86fa0c99"
        },
        "date": 1628852269008,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 150.192,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 481.169,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 506.495,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 249.361,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 287.075,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 125.364,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 59.095,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 49.62,
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
          "id": "2a0c6ebf2a8b75d353a68834a63f5f90d7762471",
          "message": "Improve syntax error reporting in woql path patterns",
          "timestamp": "2021-08-13T13:02:37+02:00",
          "tree_id": "9d5d657e5a886828d483b3c1d7ec0f0d319aff5e",
          "url": "https://github.com/terminusdb/terminusdb/commit/2a0c6ebf2a8b75d353a68834a63f5f90d7762471"
        },
        "date": 1628852823212,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 114.897,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 326.299,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 311.021,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 173.267,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 190.094,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 298.887,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 42.202,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 35.942,
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
          "id": "c3b1aa55a838da78bd54cc65678d813373449c3b",
          "message": "Merge branch 'check_duplicates' of github.com:terminusdb/terminusdb into check_duplicates",
          "timestamp": "2021-08-13T12:13:43+01:00",
          "tree_id": "a1f04f1ac7d155013756e51672a183a2005c225f",
          "url": "https://github.com/terminusdb/terminusdb/commit/c3b1aa55a838da78bd54cc65678d813373449c3b"
        },
        "date": 1628853469452,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 116.149,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 366.571,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 367.498,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 234.065,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 207.189,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 113.638,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 45.291,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 41.927,
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
          "id": "63abdf6ac90e22356f58b7e62e86afc7f83e5fee",
          "message": "whitespace fixups",
          "timestamp": "2021-08-13T13:32:32+02:00",
          "tree_id": "fc4b7ec4739188f1332a9cbef66281e265579ff3",
          "url": "https://github.com/terminusdb/terminusdb/commit/63abdf6ac90e22356f58b7e62e86afc7f83e5fee"
        },
        "date": 1628854489538,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 112.775,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 304.947,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 313.108,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 181.144,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 188.93,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 110.685,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 55.464,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 45.503,
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
          "id": "17a2d4e7fbff31f6d821c13c1e931c9b75e3da87",
          "message": "Fix test to reflect client woql API",
          "timestamp": "2021-08-13T13:35:12+02:00",
          "tree_id": "8e2542598d757e56b5ed54eef5c1eb41695c2e2b",
          "url": "https://github.com/terminusdb/terminusdb/commit/17a2d4e7fbff31f6d821c13c1e931c9b75e3da87"
        },
        "date": 1628854773778,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 132.146,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 388.835,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 390.599,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 212.512,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 226.962,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 354.361,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 45.326,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 41.343,
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
          "id": "1900d3120f3ed4c66838376a0a92206fcd5c5018",
          "message": "More reporting fixes",
          "timestamp": "2021-08-13T14:28:39+02:00",
          "tree_id": "550f97e3556e103bf4f0f3909f38bab7cbe34b5d",
          "url": "https://github.com/terminusdb/terminusdb/commit/1900d3120f3ed4c66838376a0a92206fcd5c5018"
        },
        "date": 1628857952633,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 98.459,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 272.224,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 271.364,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 151.018,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 161.209,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 263.023,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 36.925,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 34.821,
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
          "id": "ae5f5927f62fd21d63c56cbb29aad6775b5df0e0",
          "message": "Fix marshalling of to/from in PathTimes path query",
          "timestamp": "2021-08-13T14:38:12+02:00",
          "tree_id": "ca044dd010cca64585d6d99db244f107395b84bc",
          "url": "https://github.com/terminusdb/terminusdb/commit/ae5f5927f62fd21d63c56cbb29aad6775b5df0e0"
        },
        "date": 1628858568778,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 119.651,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 325.772,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 336.417,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 182.878,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 198.08,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 317.47,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 47.985,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 44.274,
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
          "id": "40e1581ff787d9c1b0752969cc10f0eb756b0bb7",
          "message": "benchmark.yml: trigger on push only to main",
          "timestamp": "2021-08-13T14:44:09+02:00",
          "tree_id": "940d1dbc0ce6640ca3c1e235f4c215a8c0e3f8af",
          "url": "https://github.com/terminusdb/terminusdb/commit/40e1581ff787d9c1b0752969cc10f0eb756b0bb7"
        },
        "date": 1628858796347,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 125.3,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 341.802,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 345.375,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 199.433,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 205.902,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 323.341,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 42.126,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 37.728,
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
          "id": "fb6e9943ab1911ecb0d156fb3f3c9eb63627331a",
          "message": "whitespace issue resolved",
          "timestamp": "2021-08-13T14:52:52+01:00",
          "tree_id": "d17db74f4e558cc09990729b044d55aada1ad438",
          "url": "https://github.com/terminusdb/terminusdb/commit/fb6e9943ab1911ecb0d156fb3f3c9eb63627331a"
        },
        "date": 1628862984780,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 100.717,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 280.426,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 300.179,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 157.172,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 168.021,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 79.199,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 36.876,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 31.349,
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
          "id": "a3321548ec4862521196eda6360863bd63df09e9",
          "message": "whitespace issue resolved",
          "timestamp": "2021-08-13T14:56:12+01:00",
          "tree_id": "f6e0cf51d7442ce5694de66870da2405b0a97cb4",
          "url": "https://github.com/terminusdb/terminusdb/commit/a3321548ec4862521196eda6360863bd63df09e9"
        },
        "date": 1628863174404,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 78.986,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 236.413,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 239.163,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 134.762,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 139.131,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 89.313,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 37.169,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 33.671,
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
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "5dfad725db66a23d8657636adbeec9d89160e38d",
          "message": "Merge pull request #409 from terminusdb/check_duplicates\n\nSame instance ID in the same transaction bug fixed",
          "timestamp": "2021-08-13T15:56:48+02:00",
          "tree_id": "6098d455e64b9355df633ca53ed3d2fa50327e63",
          "url": "https://github.com/terminusdb/terminusdb/commit/5dfad725db66a23d8657636adbeec9d89160e38d"
        },
        "date": 1628863230809,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 122.714,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 366.664,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 368.206,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 204.037,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 251.522,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 111.269,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 47.378,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 40.807,
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
          "id": "0af51aee76177f5fe49e850e4bd401b926eea809",
          "message": "Merge pull request #410 from terminusdb/syntax_error\n\nFixing the quality of JSON-LD processing with better error messages and some bug fixes.",
          "timestamp": "2021-08-13T16:43:03+02:00",
          "tree_id": "6f152114ce0dabc715aa76c3de8312b675e8ba33",
          "url": "https://github.com/terminusdb/terminusdb/commit/0af51aee76177f5fe49e850e4bd401b926eea809"
        },
        "date": 1628866008407,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 132.57,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 398.152,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 372.16,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 201.876,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 225.771,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 118.67,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 46.798,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 42.025,
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
          "id": "ebc44e57eb640d8bae20f8a12d5da1767c7c637a",
          "message": "Fix documentation.",
          "timestamp": "2021-08-16T15:26:35+02:00",
          "tree_id": "882c02aedfda4a9cead8e26b638457d6d4dba92c",
          "url": "https://github.com/terminusdb/terminusdb/commit/ebc44e57eb640d8bae20f8a12d5da1767c7c637a"
        },
        "date": 1629120618615,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 128.037,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 331.461,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 328.282,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 171.052,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 194.845,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 108.869,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 45.83,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 39.097,
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
          "id": "d8fe84c92d36d5e48b65930c9676205dca6dae38",
          "message": "Adding additional test to schema documents",
          "timestamp": "2021-08-16T15:44:25+02:00",
          "tree_id": "989a8a325772072edfb3094b22438dcdab244d54",
          "url": "https://github.com/terminusdb/terminusdb/commit/d8fe84c92d36d5e48b65930c9676205dca6dae38"
        },
        "date": 1629121733970,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 120.803,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 381.904,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 358.276,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 205.042,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 207.164,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 105.796,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 45.051,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 42.139,
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
          "id": "cc41ec335de6c3d8e4d0081a7a00787ee2458042",
          "message": "Merge pull request #418 from terminusdb/subdocument_update\n\nSubdocument update",
          "timestamp": "2021-08-16T20:29:58+02:00",
          "tree_id": "393f5676d0c91b3cdd4accb5b4731ecdaf0acbf3",
          "url": "https://github.com/terminusdb/terminusdb/commit/cc41ec335de6c3d8e4d0081a7a00787ee2458042"
        },
        "date": 1629138801704,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 90.036,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 246.237,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 239.933,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 141.011,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 147.065,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 85.815,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 38.192,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 35.015,
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
          "id": "c49f9f220c574185d28f2552407fd14a71f7c49f",
          "message": "Merge pull request #415 from terminusdb/subdocument_key_type_restriction\n\nAdding key type restriction for subdocuments",
          "timestamp": "2021-08-16T20:30:28+02:00",
          "tree_id": "89ef5da975bd900be9fe2b5700873c6c33afda30",
          "url": "https://github.com/terminusdb/terminusdb/commit/c49f9f220c574185d28f2552407fd14a71f7c49f"
        },
        "date": 1629138828341,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 89.178,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 243.361,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 241.625,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 135.625,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 148.521,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 86.61,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 37.309,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 32.622,
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
          "id": "12caf60ab4f30a7a47d7287f8123714da94fa7ae",
          "message": "Merge pull request #419 from terminusdb/subdocument-error\n\nRename subdocuments_must_be_random_or_value_hash",
          "timestamp": "2021-08-17T12:40:03+02:00",
          "tree_id": "14b9038ca52042cae6b767e37c1dbcd46095e6f2",
          "url": "https://github.com/terminusdb/terminusdb/commit/12caf60ab4f30a7a47d7287f8123714da94fa7ae"
        },
        "date": 1629197036886,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 132.743,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 372.042,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 379.943,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 219.683,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 230.231,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 121.682,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 46.168,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 43.368,
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
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "2129592156f704f6efdfb3aed016953faed5fe18",
          "message": "Merge pull request #421 from terminusdb/remove-write_schema4\n\nRemove dead write_schema4",
          "timestamp": "2021-08-17T12:45:45+02:00",
          "tree_id": "9dbfb78b88ff0f37ffe83a8b139f4f973cf43d74",
          "url": "https://github.com/terminusdb/terminusdb/commit/2129592156f704f6efdfb3aed016953faed5fe18"
        },
        "date": 1629197357010,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 107.872,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 323.25,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 308.611,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 178.016,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 180.45,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 101.255,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 38.618,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 35.366,
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
          "id": "ac4b4e158816df6b04e781514b8f307c1496cbb9",
          "message": "Merge pull request #422 from terminusdb/duplicates_replace\n\nbug in PUT resolved.",
          "timestamp": "2021-08-17T15:57:48+02:00",
          "tree_id": "8842e5d136aa20f16a05fb483552536ed5a596c6",
          "url": "https://github.com/terminusdb/terminusdb/commit/ac4b4e158816df6b04e781514b8f307c1496cbb9"
        },
        "date": 1629208894764,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 136.55,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 476.569,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 391.881,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 226.23,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 223.715,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 128.041,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 50.183,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 41.873,
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
          "id": "250347573f6528ab98cc9f8adbd641fb38c8fb46",
          "message": "Merge pull request #424 from terminusdb/schema-context-tests\n\nAdd schema context tests",
          "timestamp": "2021-08-17T22:32:11+02:00",
          "tree_id": "2dbedb4798409b9d3ce58350148518e32278b18b",
          "url": "https://github.com/terminusdb/terminusdb/commit/250347573f6528ab98cc9f8adbd641fb38c8fb46"
        },
        "date": 1629232558178,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 153.747,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 387.894,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 408.989,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 223.49,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 242.024,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 120.746,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 46.124,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 43.732,
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
          "id": "b4adfe18675defaf7f00a55be3a0ebfe648ff7a3",
          "message": "better error handling for invalid json data in payloads",
          "timestamp": "2021-08-18T12:01:17+02:00",
          "tree_id": "1c95c62cf46081d6f04e23372635a0e7ebd28901",
          "url": "https://github.com/terminusdb/terminusdb/commit/b4adfe18675defaf7f00a55be3a0ebfe648ff7a3"
        },
        "date": 1629281124828,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 135.748,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 429.924,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 394.305,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 211.166,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 244.777,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 129.641,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 51.736,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 51.316,
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
          "id": "d2bdb23f6c192c78b1f80ba76888028a27fe157b",
          "message": "Merge pull request #425 from terminusdb/context_documentation\n\nContext documentation",
          "timestamp": "2021-08-18T20:08:48+02:00",
          "tree_id": "c4555fcd98910d4fd875bff061b7485f6b356c52",
          "url": "https://github.com/terminusdb/terminusdb/commit/d2bdb23f6c192c78b1f80ba76888028a27fe157b"
        },
        "date": 1629310350284,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 129.557,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 378.63,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 368.21,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 202.366,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 213.179,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 118.11,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 46.995,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 41.991,
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
          "id": "578fe5711a8642e102b114db116b675eaa18f12e",
          "message": "Merge pull request #426 from terminusdb/js_documentation_builder\n\nAdd script for documentation generation",
          "timestamp": "2021-08-18T22:56:35+02:00",
          "tree_id": "abe1a40e9e603672603ec05d9ab603f10707e826",
          "url": "https://github.com/terminusdb/terminusdb/commit/578fe5711a8642e102b114db116b675eaa18f12e"
        },
        "date": 1629320426559,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 141.709,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 421.645,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 426.442,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 230.783,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 245.911,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 128.427,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 52.304,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 47.163,
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
          "id": "dfadfd57d92f885bc6d1fea694640d66d20e6921",
          "message": "Merge pull request #428 from terminusdb/json-to-md-whitespace\n\nRemove excess space in document generation",
          "timestamp": "2021-08-19T10:44:23+02:00",
          "tree_id": "f9a7a10b77566b1e6a8fa787fdabbdf578b392d9",
          "url": "https://github.com/terminusdb/terminusdb/commit/dfadfd57d92f885bc6d1fea694640d66d20e6921"
        },
        "date": 1629362885314,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 145.341,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 360.653,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 421.316,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 223.472,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 251.654,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 121.742,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 47.907,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 58.108,
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
          "id": "2087a4a0a590f4285d0f3a2d104f4fa875260eb0",
          "message": "Merge pull request #427 from terminusdb/fix-jwt-conditional-loading\n\nFix conditional compilation of JWT loading",
          "timestamp": "2021-08-19T10:47:18+02:00",
          "tree_id": "467f2078d3ff9c50703933eaead0201300647002",
          "url": "https://github.com/terminusdb/terminusdb/commit/2087a4a0a590f4285d0f3a2d104f4fa875260eb0"
        },
        "date": 1629363063065,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 126.053,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 371.808,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 318.985,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 180.114,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 188.584,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 100.043,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 52.384,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 45.469,
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
          "id": "fd4c3598e0b979b2a493404b3f991a2a7dfb5638",
          "message": "better errors from the document interface",
          "timestamp": "2021-08-19T11:51:34+02:00",
          "tree_id": "2be643e15ad18888c6b450ab0144bdb3a2c91a4c",
          "url": "https://github.com/terminusdb/terminusdb/commit/fd4c3598e0b979b2a493404b3f991a2a7dfb5638"
        },
        "date": 1629366938432,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 81.958,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 227.433,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 221.827,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 134.385,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 148.278,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 229.124,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 38.186,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 32.242,
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
          "id": "ab9b468f7bb4b5dabf1ae666c4902fa99eb9fb82",
          "message": "Add replace IDs to formerly empty response (#430)\n\nFixes #412",
          "timestamp": "2021-08-19T12:38:47+02:00",
          "tree_id": "31626c5473c0807de64b9163f08b989018981099",
          "url": "https://github.com/terminusdb/terminusdb/commit/ab9b468f7bb4b5dabf1ae666c4902fa99eb9fb82"
        },
        "date": 1629369772545,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 123.499,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 338.96,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 353.048,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 193.974,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 215.292,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 318.837,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 48.006,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 44.078,
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
          "id": "139b9e1c5b348a3d2b242f87fed39a0da1f6e13b",
          "message": "resolve ID singleton",
          "timestamp": "2021-08-19T12:49:00+02:00",
          "tree_id": "adc35ba8ecd97130630853a436fa0896c6b352bb",
          "url": "https://github.com/terminusdb/terminusdb/commit/139b9e1c5b348a3d2b242f87fed39a0da1f6e13b"
        },
        "date": 1629370403946,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 121.979,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 335.209,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 350.382,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 231.169,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 234.502,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 133.559,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 49.196,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 41.393,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "54183981+Francesca-Bit@users.noreply.github.com",
            "name": "Francesca Bitto",
            "username": "Francesca-Bit"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "789af4e75d44f7281895b77af50a93191a948029",
          "message": "update index.tpl, coming soon page",
          "timestamp": "2021-08-19T12:30:49+01:00",
          "tree_id": "510c4f145d444de1ba6cc28916bf714c53e417f1",
          "url": "https://github.com/terminusdb/terminusdb/commit/789af4e75d44f7281895b77af50a93191a948029"
        },
        "date": 1629372872913,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 147.974,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 416.873,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 409.153,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 241.574,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 243.57,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 127.311,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 48.266,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 42.079,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "54183981+Francesca-Bit@users.noreply.github.com",
            "name": "Francesca Bitto",
            "username": "Francesca-Bit"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "b7aaf6ac98a0d00e893f94540aa8a761002508ab",
          "message": "update index.tpl",
          "timestamp": "2021-08-19T12:33:32+01:00",
          "tree_id": "adc35ba8ecd97130630853a436fa0896c6b352bb",
          "url": "https://github.com/terminusdb/terminusdb/commit/b7aaf6ac98a0d00e893f94540aa8a761002508ab"
        },
        "date": 1629373028406,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 137.637,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 425.943,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 409.188,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 226.925,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 237.598,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 124.689,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 50.359,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 45.45,
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
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "adae7c8ff6ed6b564fc50e2f44f73af44bc5abaf",
          "message": "Merge pull request #432 from terminusdb/document_auth\n\nDocument auth",
          "timestamp": "2021-08-19T13:34:04+02:00",
          "tree_id": "c28e7528b3e75aad1866550128eec0ff9a6bf00b",
          "url": "https://github.com/terminusdb/terminusdb/commit/adae7c8ff6ed6b564fc50e2f44f73af44bc5abaf"
        },
        "date": 1629373069516,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 116.15,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 345.992,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 312.742,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 182.767,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 191.917,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 121.054,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 49.204,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 40.697,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "54183981+Francesca-Bit@users.noreply.github.com",
            "name": "Francesca Bitto",
            "username": "Francesca-Bit"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "4dcd93d9e306fe7adcd8bb2456b2c965ed393b88",
          "message": "update index.tpl\n\n<!DOCTYPE html>\r\n<html lang=\"en\" class=\"h-100\">\r\n  <!-- include \"meta-head.html\" -->\r\n<!DOCTYPE html>\r\n<html lang=\"en\">\r\n<head>\r\n    <meta charset=\"utf-8\">\r\n    <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge,chrome=1\">\r\n    <meta name=\"description\" content=\"TerminusDB is an open source model driven graph database for knowledge graph representation designed specifically for the web-age.\">\r\n    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\r\n    <link rel=\"shortcut icon\" type=\"image/png\" href=\"https://terminusdb.com/img/favicon.png\"/>    \r\n    \r\n    <title>TerminusDB</title>\r\n    <!--<link rel=\"stylesheet\" href=\"css/main.css?v=1.0\">-->\r\n    <!--<link href=\"https://fonts.googleapis.com/css?family=Lexend+Deca&display=swap\" rel=\"stylesheet\">\r\n    <link href=\"https://fonts.googleapis.com/css?family=Poppins&display=swap\" rel=\"stylesheet\">-->\r\n    <link rel=\"stylesheet\" href=\"https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css\" integrity=\"sha384-Vkoo8x4CGsO3+Hhxv8T/Q5PaXtkKtu6ug5TOeNV6gBiFeWPGFN9MuhOf23Q9Ifjh\" crossorigin=\"anonymous\">\r\n  </head>\r\n  <style>\r\n      .box{\r\n        margin-left: auto;\r\n        margin-right: auto;\r\n        margin-top: 5%;\r\n        width: 500px;\r\n      }\r\n\r\n      .header-bck{\r\n        background-color: #002855;\r\n      }\r\n\r\n      .cow-image{\r\n        max-width: 350px;\r\n        margin-left: auto;\r\n        margin-right: auto;\r\n      }\r\n\r\n  </style>\r\n  <body class=\"h-100\">\r\n    <div id=\"root\" class=\"h-100\">\r\n        <div class=\"container\">\r\n            <div class=\"card box\">\r\n                <div class=\"card-header header-bck\">\r\n                    \r\n                    <img src=\"https://terminusdb.com/img/logos/logo.svg\" class=\"menu__logo\" alt=\"Terminus DB logo\">\r\n\r\n                </div>  \r\n                <img src=\"https://assets.terminusdb.com/images/21_7-min.png\" class=\"card-img-top cow-image\" alt=\"...\">\r\n                <div class=\"card-body\">\r\n                  <p class=\"card-text\"><strong>We're hard at work getting a new TerminusDB console ready for you. In the meantime, try one of our clients:</strong></p>\r\n                  <p class=\"card-text\"><strong><a href=\"https://github.com/terminusdb/terminusdb-client-python\" \r\n                    rel=\"stylesheet\" target=\"_blank\">terminusdb-client-python</a>\r\n                    <br><a href=\"https://github.com/terminusdb/terminusdb-client-js\" \r\n                    rel=\"stylesheet\" target=\"_blank\">terminusdb-client-js</a></strong></p>\r\n                  \r\n                </div>\r\n              </div>\r\n        </div>\r\n    </div>\r\n    <!--\r\n      This HTML file is a template.\r\n      If you open it directly in the browser, you will see an empty page.\r\n\r\n      You can add webfonts, meta tags, or analytics to this file.\r\n      The build step will place the bundled scripts into the <body> tag.\r\n\r\n      To begin the development, run `npm start` or `yarn start`.\r\n      To create a production bundle, use `npm run build` or `yarn build`.\r\n\r\n    <script src=\"js/main.js\"></script>\r\n\r\n    <script src=\"js/homepage.js\"></script>-->\r\n   <!-- <script async src=\"https://www.googletagmanager.com/gtag/js?id=UA-151888980-1\"></script>-->\r\n  </body>\r\n</html>\r\n\r\n<!-- <script src=\"bundle.js\"></script> -->",
          "timestamp": "2021-08-19T12:54:22+01:00",
          "tree_id": "2caef71d7a74020188a2903a7d4a424ca9812a6f",
          "url": "https://github.com/terminusdb/terminusdb/commit/4dcd93d9e306fe7adcd8bb2456b2c965ed393b88"
        },
        "date": 1629374295253,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 118.87,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 310.698,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 317.474,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 200.482,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 200.554,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 122.867,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 46.48,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 42.68,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "54183981+Francesca-Bit@users.noreply.github.com",
            "name": "Francesca Bitto",
            "username": "Francesca-Bit"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "385c348db0e585923ff0cb6c613ae7e96cfe34e6",
          "message": "update index.tpl",
          "timestamp": "2021-08-19T12:54:58+01:00",
          "tree_id": "0279673d9d19e412d3281cfe04116ca3d4a39f94",
          "url": "https://github.com/terminusdb/terminusdb/commit/385c348db0e585923ff0cb6c613ae7e96cfe34e6"
        },
        "date": 1629374316026,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 131.5,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 408.343,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 395.283,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 210.524,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 250.618,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 124.209,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 51.127,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 46.762,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "54183981+Francesca-Bit@users.noreply.github.com",
            "name": "Francesca Bitto",
            "username": "Francesca-Bit"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "dcbee3bec47ff4d902387b579bb46b7b65e3e9e7",
          "message": "update index.tpl",
          "timestamp": "2021-08-19T12:55:47+01:00",
          "tree_id": "18a1370cf814a7debbde5629955ec3fc9165b6b6",
          "url": "https://github.com/terminusdb/terminusdb/commit/dcbee3bec47ff4d902387b579bb46b7b65e3e9e7"
        },
        "date": 1629374372263,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 143.671,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 443.906,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 436.426,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 246.704,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 269.979,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 132.069,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 47.643,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 45.156,
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
          "id": "b4e6a4cf30b83237a2521632aa766a5d5c2bf130",
          "message": "Template must not be given format args as it has none.",
          "timestamp": "2021-08-19T14:09:58+02:00",
          "tree_id": "e1f6eba0f9fe5df8b6a42ce54dc44610ed4edeec",
          "url": "https://github.com/terminusdb/terminusdb/commit/b4e6a4cf30b83237a2521632aa766a5d5c2bf130"
        },
        "date": 1629375237436,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 93.904,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 282.412,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 275.017,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 145.768,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 156.921,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 93.637,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 39.257,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 36.777,
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
          "id": "ea97d9649a3afbb6467075975ed55464f01fb06c",
          "message": "Makefile: fail on error or warning (#429)\n\nCurrently, `make` doesn't fail if there is an error or warning while\r\nbuilding the target with `swipl`. This adds a few `grep`s to do that.\r\nIt also fixes a warning on the Fedora build caused by UTF-8 Prolog files.",
          "timestamp": "2021-08-19T15:42:19+02:00",
          "tree_id": "03068baa51f46060ad32880fa8373762c9d12af6",
          "url": "https://github.com/terminusdb/terminusdb/commit/ea97d9649a3afbb6467075975ed55464f01fb06c"
        },
        "date": 1629380764465,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 120.591,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 350.983,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 351.841,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 185.91,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 216.108,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 114.778,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 43.39,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 43.468,
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
          "id": "736dce5b44b2ed49f2f99b80edd23a3d2598ed59",
          "message": "Merge pull request #435 from terminusdb/schema_documentation\n\nAdding schema documentation to core schemata",
          "timestamp": "2021-08-19T20:10:29+02:00",
          "tree_id": "a5ad017c0cbcd46737675bed30e1e5d6056594ba",
          "url": "https://github.com/terminusdb/terminusdb/commit/736dce5b44b2ed49f2f99b80edd23a3d2598ed59"
        },
        "date": 1629396856719,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 113.491,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 340.254,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 334.274,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 191.128,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 191.093,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 102.607,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 37.338,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 33.944,
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
          "id": "0159719f74b15acea44a40674980d906c630d7ae",
          "message": "Merge pull request #436 from terminusdb/schema_documentation_script\n\nSchema documentation script",
          "timestamp": "2021-08-19T21:24:53+02:00",
          "tree_id": "471a2c5a7b407d5734b4a1c04067b2c6f9705a63",
          "url": "https://github.com/terminusdb/terminusdb/commit/0159719f74b15acea44a40674980d906c630d7ae"
        },
        "date": 1629401293005,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 83.137,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 229.415,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 229.098,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 131.786,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 143.265,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 93.165,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 36.703,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 32.222,
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
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "2922823d4ce175171fd0f081d25a36ec03e16c69",
          "message": "Merge pull request #439 from terminusdb/more_document_error_handling\n\nMore document error handling",
          "timestamp": "2021-08-20T10:39:45+02:00",
          "tree_id": "943f739409ec7843286253f8559f035a9d4d98f4",
          "url": "https://github.com/terminusdb/terminusdb/commit/2922823d4ce175171fd0f081d25a36ec03e16c69"
        },
        "date": 1629448989262,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 92.472,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 273.709,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 270.792,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 155.6,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 165.012,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 94.709,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 37.151,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 33.806,
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
          "id": "2454104533587370f68c3318f90bca4ecf172d75",
          "message": "Merge pull request #440 from terminusdb/schema-checker-prefixes-squashed\n\nAdd base and schema prefix URI checks",
          "timestamp": "2021-08-20T11:26:45+02:00",
          "tree_id": "e95fa0200268269e6d15d8598f709e3035973755",
          "url": "https://github.com/terminusdb/terminusdb/commit/2454104533587370f68c3318f90bca4ecf172d75"
        },
        "date": 1629451826955,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 121.72,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 347.412,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 354.385,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 206.607,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 215.857,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 123.933,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 45.105,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 38.517,
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
          "id": "a1f5cacb72672835ca37db2137b83769a8ccdfad",
          "message": "Fixing some documentation in preparation for release.",
          "timestamp": "2021-08-20T11:57:21+02:00",
          "tree_id": "f4021aa1e64d53cd2e660b6718eea727743eaf13",
          "url": "https://github.com/terminusdb/terminusdb/commit/a1f5cacb72672835ca37db2137b83769a8ccdfad"
        },
        "date": 1629453663686,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 114.09,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 373.756,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 365.644,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 187.963,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 217.131,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 118.604,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 48.32,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 44.002,
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
          "id": "2912f3b6188f0dbd8d941564d9183b8619dc844d",
          "message": "Move macos build and test into nightly workflow (#445)\n\nThe macOS tests take a long time to run, so we don't do them on every\r\npush and pull request.",
          "timestamp": "2021-08-23T12:55:19+02:00",
          "tree_id": "b23479328048f3554e4a7c405f53f37585b1cf04",
          "url": "https://github.com/terminusdb/terminusdb/commit/2912f3b6188f0dbd8d941564d9183b8619dc844d"
        },
        "date": 1629716330634,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 113.475,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 319.262,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 312.27,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 175.568,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 192.677,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 116.993,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 47.766,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 40.064,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "rrooij@users.noreply.github.com",
            "name": "Robin de Rooij",
            "username": "rrooij"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "f120a4a9702e5482b2ae91b065156a6cd4ca75e0",
          "message": "jwt: use agent name as username (#447)\n\n'sub' can't be changed as easy as a custom entry in the token.",
          "timestamp": "2021-08-23T13:29:08+02:00",
          "tree_id": "a3e5123b4e78c6c9b7114fc3a9e5d938fe20b8db",
          "url": "https://github.com/terminusdb/terminusdb/commit/f120a4a9702e5482b2ae91b065156a6cd4ca75e0"
        },
        "date": 1629718374075,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 144.821,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 433.795,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 435.157,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 236.281,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 245.356,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 124.411,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 47.558,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 42.384,
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
          "id": "fd267139af12e667920320a701be0a6a872699ba",
          "message": "Skip ubuntu job if already run (#446)",
          "timestamp": "2021-08-23T13:32:33+02:00",
          "tree_id": "6707ec9c122fb0fb64af8b1a6e4c111e14eeeb1e",
          "url": "https://github.com/terminusdb/terminusdb/commit/fd267139af12e667920320a701be0a6a872699ba"
        },
        "date": 1629718570905,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 105.838,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 326.133,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 313.341,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 169.273,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 202.711,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 107.394,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 43.828,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 39.317,
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
          "id": "8bb54508345a78364696f93ee3be0e020f7de00a",
          "message": "Fix some JWT environment issues (#444)\n\n* Use TERMINUSDB_JWT_ENABLED env var from build for runtime\r\n* Ignore empty string in TERMINUSDB_SERVER_JWKS_ENDPOINT",
          "timestamp": "2021-08-23T16:27:11+02:00",
          "tree_id": "47187523b0e5b1b56335fb876f45e4c73235d748",
          "url": "https://github.com/terminusdb/terminusdb/commit/8bb54508345a78364696f93ee3be0e020f7de00a"
        },
        "date": 1629729049251,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 108.447,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 338.187,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 334.541,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 189.89,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 194.441,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 94.341,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 37.354,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 33.746,
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
          "id": "7146d0e89a4162f7288c3bf8d67586f86d5e4c82",
          "message": "Adding first cut of schema documentation for document interface.",
          "timestamp": "2021-08-24T00:18:48+02:00",
          "tree_id": "e7046af8726f2c86ab3e65d7f7bf454b592bec62",
          "url": "https://github.com/terminusdb/terminusdb/commit/7146d0e89a4162f7288c3bf8d67586f86d5e4c82"
        },
        "date": 1629757345992,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 115.296,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 343.691,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 331.725,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 182.224,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 193.713,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 103.52,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 40.037,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 37.987,
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
          "id": "fda0be3773979fa3d3fc3e41aa7d5e1e17abbed0",
          "message": "Adding class varieties and subdocuments",
          "timestamp": "2021-08-24T10:12:59+02:00",
          "tree_id": "d4fdf3ff878300d3ab5f41b44cfb5aa95d14cf92",
          "url": "https://github.com/terminusdb/terminusdb/commit/fda0be3773979fa3d3fc3e41aa7d5e1e17abbed0"
        },
        "date": 1629793022572,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 121.592,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 351.66,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 354.029,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 197.924,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 207.863,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 131.445,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 45.568,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 42.336,
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
          "id": "fb6e6f0c90051902e90dbbc19ecb1aa83e6edcb6",
          "message": "Typo",
          "timestamp": "2021-08-24T10:16:39+02:00",
          "tree_id": "e48d1ef4b5d51a453b681a944c79202096fb0cc9",
          "url": "https://github.com/terminusdb/terminusdb/commit/fb6e6f0c90051902e90dbbc19ecb1aa83e6edcb6"
        },
        "date": 1629793223198,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 109.362,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 327.567,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 313.775,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 175.273,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 186.784,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 112.801,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 44.616,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 40.976,
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
          "id": "9eda2b7137a2dba0f36a0a564ed0aab90f9cd75d",
          "message": "Documentation fixes",
          "timestamp": "2021-08-24T11:03:48+02:00",
          "tree_id": "7559fded3ed5298c890157e0545d70e9dfa60f5b",
          "url": "https://github.com/terminusdb/terminusdb/commit/9eda2b7137a2dba0f36a0a564ed0aab90f9cd75d"
        },
        "date": 1629796073310,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 122.047,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 342.348,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 377.294,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 194.125,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 225.874,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 127.23,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 46.092,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 40.569,
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
          "id": "303697c79d9491674cee078245c64bb309044428",
          "message": "Adding natural language description",
          "timestamp": "2021-08-24T11:14:06+02:00",
          "tree_id": "68667affb66828146d3dfced74424f24247146c8",
          "url": "https://github.com/terminusdb/terminusdb/commit/303697c79d9491674cee078245c64bb309044428"
        },
        "date": 1629796682668,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 116.133,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 335.007,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 333.67,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 187.838,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 204.414,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 114.912,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 47.002,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 41.417,
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
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "da16009b8db6e77498ecdbdd653f4ce7dd9059fb",
          "message": "Merge pull request #452 from terminusdb/cli_db_creation_prefixes\n\nEnsure that we have proper default prefixes when creating db from cli",
          "timestamp": "2021-08-24T15:21:23+02:00",
          "tree_id": "d21256e75176612cf75ae39820227bf8f5972e97",
          "url": "https://github.com/terminusdb/terminusdb/commit/da16009b8db6e77498ecdbdd653f4ce7dd9059fb"
        },
        "date": 1629811513491,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 122.851,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 362.695,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 354.126,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 215.249,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 220.844,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 134.46,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 51.443,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 42.267,
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
          "id": "479a20fc8609d3585110015297f4297aacf94b53",
          "message": "fix authentication checking to properly handle strings",
          "timestamp": "2021-08-24T16:41:44+02:00",
          "tree_id": "83054137aced80e8459998590a06f50055ee4dae",
          "url": "https://github.com/terminusdb/terminusdb/commit/479a20fc8609d3585110015297f4297aacf94b53"
        },
        "date": 1629816337443,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 133.358,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 376.793,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 382.008,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 224.091,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 222.97,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 123.38,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 51.737,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 47.45,
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
          "id": "5723fdce70fda193dfa07109ab8465708aadb921",
          "message": "Fixing graph_type marshalling in routes",
          "timestamp": "2021-08-24T16:44:56+02:00",
          "tree_id": "a6b8e45baf0caa67ea9380859d9a78e70bb5c480",
          "url": "https://github.com/terminusdb/terminusdb/commit/5723fdce70fda193dfa07109ab8465708aadb921"
        },
        "date": 1629816539071,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 118.417,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 323.311,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 328.048,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 188.196,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 197.135,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 134.735,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 48.704,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 45.321,
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
          "id": "5e496742ed07316b6833ea00d41418e2904eb4ba",
          "message": "Adding newlines for readability",
          "timestamp": "2021-08-25T09:46:49+02:00",
          "tree_id": "13bb55e857306738e6fbb79da5bfecdc66e79513",
          "url": "https://github.com/terminusdb/terminusdb/commit/5e496742ed07316b6833ea00d41418e2904eb4ba"
        },
        "date": 1629877844092,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 133.779,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 389.064,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 392.39,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 211.641,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 221.591,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 124.83,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 48.238,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 39.343,
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
          "id": "42fcd9deb58fc384b35cdb632a464934703fc92d",
          "message": "Don't run test workflows if only docs are changed (#455)",
          "timestamp": "2021-08-25T09:56:39+02:00",
          "tree_id": "962804a4895542260c62751d988905c8eb76d917",
          "url": "https://github.com/terminusdb/terminusdb/commit/42fcd9deb58fc384b35cdb632a464934703fc92d"
        },
        "date": 1629878432026,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 227.375,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 614.62,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 588.458,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 261.298,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 274.947,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 156.578,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 52.952,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 48.931,
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
          "id": "a9c20196756dcec8532d7e06c6f40fb38bcbe7de",
          "message": "remove accidental http log line",
          "timestamp": "2021-08-25T16:39:20+02:00",
          "tree_id": "fc64a4c08e86b150a43c239239d9bb14a2e2d071",
          "url": "https://github.com/terminusdb/terminusdb/commit/a9c20196756dcec8532d7e06c6f40fb38bcbe7de"
        },
        "date": 1629902590562,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 144.187,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 372.027,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 352.142,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 207.428,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 202.626,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 136.299,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 47.172,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 48.324,
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
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "1d68beb2d980bb2a608aadc8d1f2e67d31a750b2",
          "message": "Merge pull request #463 from terminusdb/query_empty_branch\n\nquerying an empty branch should no longer error",
          "timestamp": "2021-08-25T16:50:08+02:00",
          "tree_id": "54d03a2618c096b198bcda1d162a9c641328e81d",
          "url": "https://github.com/terminusdb/terminusdb/commit/1d68beb2d980bb2a608aadc8d1f2e67d31a750b2"
        },
        "date": 1629903217923,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 91.103,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 260.876,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 254.943,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 146.94,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 155.746,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 94.094,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 37.347,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 34.047,
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
          "id": "dde934b416bfaaed36833ad1e8b151687b9f2a58",
          "message": "Merge pull request #459 from terminusdb/authorization\n\nPrefixes auth fix",
          "timestamp": "2021-08-25T18:01:40+02:00",
          "tree_id": "9d013ef5154c86200b59dc7b591788b4be33811c",
          "url": "https://github.com/terminusdb/terminusdb/commit/dde934b416bfaaed36833ad1e8b151687b9f2a58"
        },
        "date": 1629907523019,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 141.549,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 404.823,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 419.241,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 236.961,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 249.823,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 128.962,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 43.183,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 40.436,
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
          "id": "459698c5a8882b8f266c1cb54b88dc0f16ed4bf6",
          "message": "Merge pull request #465 from terminusdb/rebase_fix\n\nAdding two tests which exhibited no schema failure - and a fix",
          "timestamp": "2021-08-25T22:55:42+02:00",
          "tree_id": "8b01127ebe2dabce274f845a7fb44ffc0cdefebc",
          "url": "https://github.com/terminusdb/terminusdb/commit/459698c5a8882b8f266c1cb54b88dc0f16ed4bf6"
        },
        "date": 1629925147733,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 102.144,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 300.183,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 299.249,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 173.994,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 178.953,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 102.478,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 40.537,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 34.044,
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
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "f763d5bd6a5034e8aa8c3ca9ce91fe3ef55208e8",
          "message": "Merge pull request #468 from terminusdb/create_db_properly\n\nfix: schema and public arguments were swapped in call to db creation",
          "timestamp": "2021-08-26T11:09:20+02:00",
          "tree_id": "0bb8026893b162f9297fc21bf527a80c566650a0",
          "url": "https://github.com/terminusdb/terminusdb/commit/f763d5bd6a5034e8aa8c3ca9ce91fe3ef55208e8"
        },
        "date": 1629969182222,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 122.39,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 368.275,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 362.276,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 196.126,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 208.707,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 126.582,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 48.468,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 42.066,
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
          "id": "7e90da2a8f1e4535007bb5cdade65eb965cdc0e9",
          "message": "Merge pull request #471 from terminusdb/fix-prefixes-write-access\n\nChange update_prefixes auth to write access",
          "timestamp": "2021-08-26T12:21:47+02:00",
          "tree_id": "2b32e8ec111910dd33f19c4be82d0bc0e39bc687",
          "url": "https://github.com/terminusdb/terminusdb/commit/7e90da2a8f1e4535007bb5cdade65eb965cdc0e9"
        },
        "date": 1629973543649,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 125.23,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 360.389,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 359.265,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 196.346,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 215.18,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 126.436,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 48.906,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 40.675,
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
          "id": "829f852d46bc67b32eab22880224a968060c8c03",
          "message": "Merge pull request #467 from terminusdb/empty_not_schemaless\n\nRemove hard removal of schemaless mode from schema.",
          "timestamp": "2021-08-26T12:59:08+02:00",
          "tree_id": "458f145ec273c6b4913a7942d2456f71bcc36dca",
          "url": "https://github.com/terminusdb/terminusdb/commit/829f852d46bc67b32eab22880224a968060c8c03"
        },
        "date": 1629975768929,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 115.294,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 329.761,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 338.211,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 193.536,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 208.366,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 123.545,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 44.551,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 40.027,
            "unit": "ms"
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
          "id": "fdfc6f85b23ebe2a344fe798ef6d2da2ea80690e",
          "message": "add docs and storage to dockerignore",
          "timestamp": "2021-08-26T16:55:48+02:00",
          "tree_id": "5ce4b32ae8998a08fcd6f99461084ea2f8cb19e4",
          "url": "https://github.com/terminusdb/terminusdb/commit/fdfc6f85b23ebe2a344fe798ef6d2da2ea80690e"
        },
        "date": 1629989968325,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 94.449,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 276.628,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 271.51,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 152.807,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 164.614,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 97.501,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 38.966,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 33.77,
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
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "77faea42636d6bbc2be89e4e2b70c2345074e3d6",
          "message": "Merge pull request #475 from terminusdb/orphan-objects\n\ndetect orphan objects in schema checking",
          "timestamp": "2021-08-26T17:05:52+02:00",
          "tree_id": "7ccfbf55ce940c71ddc2547911e2cbc8836808f3",
          "url": "https://github.com/terminusdb/terminusdb/commit/77faea42636d6bbc2be89e4e2b70c2345074e3d6"
        },
        "date": 1629990559931,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 90.049,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 260.047,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 260.275,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 152.35,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 162.494,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 93.375,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 39.164,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 33.639,
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
          "id": "2ea681354640a8b60837ea5def4a9ef7daaed09d",
          "message": "Merge pull request #477 from terminusdb/check-content-type-json\n\nError for missing/bad Content-type",
          "timestamp": "2021-08-26T17:53:07+02:00",
          "tree_id": "60708ea9fb8884c6d2ac1b8c1e3b8437c8c4ab8c",
          "url": "https://github.com/terminusdb/terminusdb/commit/2ea681354640a8b60837ea5def4a9ef7daaed09d"
        },
        "date": 1629993405208,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 118.722,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 366.629,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 332.486,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 185.55,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 197.827,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 122.767,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 43.137,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 44.193,
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
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "85760bab3ebbc1805f4345c417ae7b51354d0c6d",
          "message": "Merge pull request #451 from terminusdb/more_document_error_handling\n\nmore document error handling",
          "timestamp": "2021-08-27T14:44:03+02:00",
          "tree_id": "d17b0ffdfde588f3c5ddf427a9245f22579699e5",
          "url": "https://github.com/terminusdb/terminusdb/commit/85760bab3ebbc1805f4345c417ae7b51354d0c6d"
        },
        "date": 1630068462767,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 122.576,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 361.667,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 369.64,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 200.619,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 216.968,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 118.912,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 45.942,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 49.789,
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
          "id": "6710aecbb180c14a254bd633607c1fc6ca89d104",
          "message": "Merge pull request #481 from terminusdb/woql_schema_extension\n\nUpdate WOQL schema to add TypeOf",
          "timestamp": "2021-08-27T15:37:29+02:00",
          "tree_id": "e7a0b03c8e80af3aba3247289a367d4d620028db",
          "url": "https://github.com/terminusdb/terminusdb/commit/6710aecbb180c14a254bd633607c1fc6ca89d104"
        },
        "date": 1630071681277,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 126.118,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 368.597,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 359.448,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 209.265,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 220.28,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 117.946,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 53.218,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 44.832,
            "unit": "ms"
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
          "id": "c846e77f738570b918e14718f735b8bc99904906",
          "message": "use node for local docs compilation\n\n`js` is not the standard",
          "timestamp": "2021-08-27T15:56:55+02:00",
          "tree_id": "7c8704ce2ef046600cf976785211af81f5633cc6",
          "url": "https://github.com/terminusdb/terminusdb/commit/c846e77f738570b918e14718f735b8bc99904906"
        },
        "date": 1630072853224,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 152.226,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 439.212,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 464.887,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 253.816,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 263.822,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 138.492,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 55.088,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 49.463,
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
          "id": "e544d9fc54da297cb19af111a83ccd7e53017b63",
          "message": "Remove docs.yml\n\nOur documentation has moved to <https://github.com/terminusdb/terminusdb-docs>",
          "timestamp": "2021-08-30T10:37:24+02:00",
          "tree_id": "45be36689c5089bd011bd0fbbb399b4b901a4b73",
          "url": "https://github.com/terminusdb/terminusdb/commit/e544d9fc54da297cb19af111a83ccd7e53017b63"
        },
        "date": 1630312882575,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 115.015,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 353.57,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 349.311,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 207.438,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 202.131,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 114.821,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 40.421,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 38.607,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "rrooij@users.noreply.github.com",
            "name": "Robin de Rooij",
            "username": "rrooij"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0d7a9e089b25af3149e8e7c3c43125533396bd9b",
          "message": "Remove Javscript JSON conversion (#484)",
          "timestamp": "2021-08-30T15:13:47+02:00",
          "tree_id": "ef919da2cd8239e0bcb7c592d5376360b1d1725b",
          "url": "https://github.com/terminusdb/terminusdb/commit/0d7a9e089b25af3149e8e7c3c43125533396bd9b"
        },
        "date": 1630329452073,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 122.187,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 360.607,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 361.013,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 199.982,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 211.625,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 120.729,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 49.674,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 42.454,
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
          "id": "97f2457a58df66a096a5fff4a03dff6b7d1d7405",
          "message": "Merge pull request #491 from terminusdb/key_strategy_update\n\nchanging key strategy now validates existing objects",
          "timestamp": "2021-09-01T08:59:08+02:00",
          "tree_id": "209f2fb43d87e17a29809688db1247d6407b1402",
          "url": "https://github.com/terminusdb/terminusdb/commit/97f2457a58df66a096a5fff4a03dff6b7d1d7405"
        },
        "date": 1630479764844,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 121.502,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 359.198,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 353.65,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 201.042,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 210.918,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 111.183,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 39.913,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 34.62,
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
          "id": "758191e235c2aeceace8d45bf50cff595947364a",
          "message": "Merge pull request #493 from terminusdb/graph-unfiltered\n\nFix graph/graph_filter, DeleteTriple, etc.",
          "timestamp": "2021-09-01T11:56:18+02:00",
          "tree_id": "d790996ad52ab7621b3cdd4943077a8693e8cc55",
          "url": "https://github.com/terminusdb/terminusdb/commit/758191e235c2aeceace8d45bf50cff595947364a"
        },
        "date": 1630490408166,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 126.131,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 378.328,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 389.854,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 215.418,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 241.213,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 128.48,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 46.55,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 42.791,
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
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "16c2e3c21ed4db2abad950251ddda625eb906372",
          "message": "Merge pull request #494 from terminusdb/raise_transaction_retry_count\n\nraise default max transaction retries to multiple of worker count",
          "timestamp": "2021-09-01T15:57:00+02:00",
          "tree_id": "02ddd52df38947dd4a43af044c5343e8d50f65e6",
          "url": "https://github.com/terminusdb/terminusdb/commit/16c2e3c21ed4db2abad950251ddda625eb906372"
        },
        "date": 1630504841152,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 114.317,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 357.163,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 335.693,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 178.243,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 198.809,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 119.094,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 45.727,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 46.535,
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
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "304583f7e41e6b8f9983971a208a824c2542a4ae",
          "message": "Merge pull request #495 from terminusdb/list_array_cardinality_check\n\nproper list and array cardinality checking",
          "timestamp": "2021-09-01T16:12:53+02:00",
          "tree_id": "b4882283077ed4adb233dd0db328a59b777154e1",
          "url": "https://github.com/terminusdb/terminusdb/commit/304583f7e41e6b8f9983971a208a824c2542a4ae"
        },
        "date": 1630505801695,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 126.076,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 363.915,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 364.966,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 207.488,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 233.235,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 118.946,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 47.997,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 41.973,
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
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e6952fe26b927bc5f455b5d757f8ac6e9f211262",
          "message": "Merge pull request #503 from terminusdb/access_control_fix\n\nAccess control fix",
          "timestamp": "2021-09-02T12:37:07+02:00",
          "tree_id": "4a2024ab5bb7149e14dcdbee812beae29167d2a0",
          "url": "https://github.com/terminusdb/terminusdb/commit/e6952fe26b927bc5f455b5d757f8ac6e9f211262"
        },
        "date": 1630579255879,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 130.766,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 377.295,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 371.917,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 213.758,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 233.407,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 123.031,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 57.568,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 47.886,
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
          "id": "502e936702a346327ab818ec7ed9166d93f4e1d4",
          "message": "Speed up CI (#504)\n\n* Move ubuntu install to nightly\r\n* Docker test for all push, pull_request, workflow_dispatch events\r\n* Skip test for pull_request duplicates of push events\r\n* Deploy only for push events with certain refs\r\n* Other cleanups",
          "timestamp": "2021-09-02T12:43:42+02:00",
          "tree_id": "28d7af6fb41b15c1c1161f71d6e896561e136963",
          "url": "https://github.com/terminusdb/terminusdb/commit/502e936702a346327ab818ec7ed9166d93f4e1d4"
        },
        "date": 1630579652130,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 139.066,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 416.358,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 431.251,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 221.96,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 286.397,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 119.071,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 45.835,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 43.283,
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
          "id": "441b426a8a08bfe28198717123bed20a6ce3be7b",
          "message": "Fix deploy job in CI workflow",
          "timestamp": "2021-09-02T13:05:33+02:00",
          "tree_id": "eed4dabdfc87aed5b36657e49952b58d715b3775",
          "url": "https://github.com/terminusdb/terminusdb/commit/441b426a8a08bfe28198717123bed20a6ce3be7b"
        },
        "date": 1630580927834,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 126.831,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 378.154,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 394.531,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 228.166,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 239.738,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 116.26,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 49.092,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 42.232,
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
          "id": "6c6a15350ed5ed096955b0621f5a74ccd0346a2a",
          "message": "Save/upload and download/load Docker image",
          "timestamp": "2021-09-02T13:17:55+02:00",
          "tree_id": "9f82faac01d7b1211f2f20b532352d4e66c72a1a",
          "url": "https://github.com/terminusdb/terminusdb/commit/6c6a15350ed5ed096955b0621f5a74ccd0346a2a"
        },
        "date": 1630581719275,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 116.746,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 340.042,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 403.441,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 202.681,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 223.146,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 127.566,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 48.14,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 43.587,
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
          "id": "a40542b800593adc9ea07f616dd8855aeadec47e",
          "message": "Compress the Docker image with gzip\n\nThis reduces the file size from ~150 MB to ~50 MB.",
          "timestamp": "2021-09-02T13:34:44+02:00",
          "tree_id": "7209ab92ad7e4273c41ffcb7eb365e63c9898a13",
          "url": "https://github.com/terminusdb/terminusdb/commit/a40542b800593adc9ea07f616dd8855aeadec47e"
        },
        "date": 1630582776891,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 91.484,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 288.45,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 270.626,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 149.137,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 168.846,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 93.168,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 36.9,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 32.258,
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
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "7bd262e09063c9a90d5e11ea2ea6eab0d1a8fd06",
          "message": "Merge pull request #489 from terminusdb/prefix_errors\n\nPrefix errors",
          "timestamp": "2021-09-02T16:50:27+02:00",
          "tree_id": "3147266b626e3cf98a0168ad064f8c4cf0459cea",
          "url": "https://github.com/terminusdb/terminusdb/commit/7bd262e09063c9a90d5e11ea2ea6eab0d1a8fd06"
        },
        "date": 1630594442525,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 122.979,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 357.974,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 355.057,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 197.567,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 213.785,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 114.73,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 46.47,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 40.723,
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
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "da9a046ef92c9a5275deba0c874b5e0e78639230",
          "message": "Merge pull request #507 from terminusdb/default_schema_checking_to_true\n\ndefault schema checking to true on new db creation",
          "timestamp": "2021-09-02T17:21:45+02:00",
          "tree_id": "f0870e899454f4ab8c49531d035c4203b36ec120",
          "url": "https://github.com/terminusdb/terminusdb/commit/da9a046ef92c9a5275deba0c874b5e0e78639230"
        },
        "date": 1630596327359,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 118.925,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 335.929,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 354.523,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 193.72,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 208.161,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 118.123,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 48.671,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 43.242,
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
          "id": "be41d7e90daa74bfa86faba070d82538c1cf21b9",
          "message": "Merge pull request #499 from terminusdb/id_generation\n\nId generation",
          "timestamp": "2021-09-02T17:29:34+02:00",
          "tree_id": "4a946e6ee6d8c4034eb7f05ebcd99a402c332efb",
          "url": "https://github.com/terminusdb/terminusdb/commit/be41d7e90daa74bfa86faba070d82538c1cf21b9"
        },
        "date": 1630596798583,
        "tool": "generic",
        "benches": [
          {
            "name": "/api/db::DELETE:http_req_duration:p(90)",
            "value": 115.411,
            "unit": "ms"
          },
          {
            "name": "/api/db::POST:http_req_duration:p(90)",
            "value": 340.178,
            "unit": "ms"
          },
          {
            "name": "/api/db:prefixes:POST:http_req_duration:p(90)",
            "value": 347.838,
            "unit": "ms"
          },
          {
            "name": "/api/document:one:POST:http_req_duration:p(90)",
            "value": 187.172,
            "unit": "ms"
          },
          {
            "name": "/api/document:person:POST:http_req_duration:p(90)",
            "value": 212.719,
            "unit": "ms"
          },
          {
            "name": "/api/document:woql:POST:http_req_duration:p(90)",
            "value": 125.471,
            "unit": "ms"
          },
          {
            "name": "/api/info::GET:http_req_duration:p(90)",
            "value": 49.423,
            "unit": "ms"
          },
          {
            "name": "/api/ok::GET:http_req_duration:p(90)",
            "value": 40.149,
            "unit": "ms"
          }
        ]
      }
    ]
  }
}