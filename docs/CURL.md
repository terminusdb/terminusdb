# CURL

Some CURL examples for helping people building clients in various languages.

```bash
curl -X PUT "https://127.0.0.1:6363/api/csv/admin/TEST_DB" -u admin -k -F "csv=@test/0CE.csv"
```

```bash
curl -X GET "https://127.0.0.1:6363/api/csv/admin/TEST_DB" -u "admin:root" -k -d '{}'
```
