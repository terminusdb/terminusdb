## How To Fix Certificate Issues in the Python Client

With `https` connection, a certificate is required. However, if you are running TerminusDB locally (on 127.0.0.1) you will have to by-pass this check. Simply apply the keyword argument `insecure=True` when creating the WOQLClient. For example: `client=WOQLClient("https://127.0.0.1:6363", insecure=True)`
