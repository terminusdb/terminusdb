curl -u rrooij:$GITHUB_API_TOKEN \
  -X POST \
  -H "Accept: application/vnd.github.v3+json" \
  https://api.github.com/repos/terminusdb-labs/terminusdb-appimage/actions/workflows/2394580/dispatches \
  -d "{\"ref\":\"master\", \"inputs\": {\"terminusdb_branch\": \"$TRAVIS_BRANCH\"}}"
