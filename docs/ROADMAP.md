# Terminus Store Roadmap

Terminus Store is the backend storage layer for TerminusDB. However, it is designed to be an independently useful library for storing and searching graphs using the RDF data model.

The library is already quite effective for storing graphs with edges in the tens of millions compactly. However there are a number of important features that are lacking which we would like to implement in the future.

In rough order of priority we've outlined the following features that we hope to implement. If you've an interest in working on one of these features that would be fantastic, and you should contact the maintainer <matthijs@terminusdb.com> and we can try to help you get started.

We have vaguely divided the tasks into "Now", "Next", and "Later" depending on when we plan to work on them. However this should not discourage people from working on our future priorities now!

# Now

* Delta Rollups

    Delta rollups are performance optimisations that allow multiple delta layers to be "rolled-up" into a single layer which gives us effective access to the top layer of the delta-rollup with much better performance, while retaining all of the individual commits in a queryable manner.

* Content addressable hashing

    Layers are currently addressed using a randomly generated identifier. It would be much better to generate this from a hash of the data. This will simplify much of layer management and avoid duplication.

    More on this issue can be found at [here](./CONTENT.md).

* Lexical xsd type storage

    Currently we store all datatypes in a canonical xsd format. The lexical ordering is unfortunately not the same as the natural ordering of the domain. This will make range based searches fast and will significantly reduce storage space.

    More on this issue can be found at [here](./LEXICAL.md).

# Next

* Garbage collection

    We need to do cleanup of layers when they are no longer referenced.

    More on this issue can be found at [here](./GARBAGE.md).

# Later

* Encrypted Layers

    Many operations do not require access to the underlying content. This means we could use a public-key crypto-system to allow layer storage and transport without revealing layer content.

* Signed Commit Messages

    In order to track provenance of commits, it would be very useful to have digital signatures on commit messages. This is a relatively simple change, but requires a bit of tooling in the build process.

* Blob-store backend

    We have tried to write the library with the idea of plugging in various backends in the future. Something like an S3 backend would be desirable.

