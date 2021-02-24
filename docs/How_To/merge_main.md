

## Merge

You may want to perform a merge from current branch to another branch. The below example shows how to merge from sourceBranch to
targetBranch. This example assumes that woqlClient is already connected and ready for use.

Steps:
1. Checkout to targetBranch
2. Format a rebase_source json object with the sourceBranch info
3. Use woqlClient.resource() which generates a resource string for the required context based on "branch" or "ref" special resources
4. woqlClient.rebase(rebase_source) merges sourceBranch to targetBranch based on the URL attained from woqlClient.resource()


```javascript
let targetBranch="main"
let sourceBranch="branch_1"

woqlClient.checkout(targetBranch) // make sure to checkout targetBranch
      
let branchUrl=woqlClient.resource('branch', sourceBranch)

var rebase_source = {}
rebase_source.rebase_from = branchUrl
rebase_source.message = "some commit message"

woqlClient.rebase(rebase_source).then(() => {
	console.log("Success in merge, do something")
})
.catch((err) => {console.log("Error in merge, do something")})
.finally(() => {console.log("do something")})
```

Note: In this example, resource is based on the branch URL by specifying "branch" as the first parameter

```javascript
woqlClient.resource('branch', sourceBranch)
```

You can also rebase based on a particular commit ID, by using "ref" as the first parameter, in this case you will also require the
commit hash id of interest.

```javascript
let commit="jg9p1eleuae8fhajlyd7slwr0lirdnk"
let refUrl=woqlClient.resource('ref', commit)
var rebase_source = {}
rebase_source.rebase_from = refUrl
```

