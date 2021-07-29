---
author:
- |
    Matthijs van Otterdijk^1,2^ and Gavin Mendel-Gleason^1,3^ and Kevin
    Feeney^1,4^ ^1^DataChemist Ltd. <http://datachemist.com>\
    ^2^[matthijs\@datachemist.com](matthijs@datachemist.com)\
    ^3^[gavin\@datachemist.com](gavin@datachemist.com)\
    ^4^[kevin\@datachemist.com](kevin@datachemist.com)
bibliography:
- 'terminusdb.bib'
title: Succinct Data Structures and Delta Encoding for Modern Databases
---

\[\] []{.lettrine}

Introduction
============

There has been an explosion of new database designs, including graph
databases, key-value stores, document databases and multi-model
databases. Yet the majority of production databases are still based on
the RDBMS designs of the 1970s[@Codd:1970:RMD:362384.362685].

Meanwhile both hardware infrastructure and software design process have
moved on significantly over the course of the last 40 years. In
particular machines with terrabytes of RAM are now available for prices
reasonable enough for some small and medium sized enterprises.

At the same time flexible revision control systems have revolutionised
the software development process. The maintenance of histories, of
records of modification and the ability to roll back enables engineers
to have confidence in making modificiations collaboratively. This is
augmented with important features such as branching, labelling,
rebasing, and cloning. When combined with continuous
integration/continuous delivery[@65147][@LAUKKANEN201755] (CI/CD) teams
of programmers can have confidence that central repositories are
maintained in correct states once testing and verification of have been
passed.

These two developments suggest a solution at their intersection. Namely
the use of in-memory immutable *succinct* data structures and *deltas*
as used in revision control systems. TerminusDB demonstrates how these
features can be combined to produce a flexible transactional graph
database.

Design
======

TerminusDB is a full featured graph database management system (GDBMS)
including a rich query language: WOQL (the Web Object Query Language).
However, we restrict our attention here to the underlying datastructure
design and layout which we have implemented in a
Rust[@Blandy:2015:RPL:3019371] library which we call *terminus-store*.

We describe in turn the graph database model which is used, the succinct
data structure approach, and finally how we implement revision control
type operations using *deltas* which we collect together with some
metadata into an object which we term *layers*.

Graph Databases
---------------

Graph databases are one of the fastest growing of the new database
paradigms. Since graphs are very general it is possible to render many
database modeling techniques in a graph database. The simplicity and
generality make it a good candidate for a *general purpose* delta
encoded approach to an online transaction processing database.

The TerminusDB infrastucture is based on the *RDF* standard. This
standard specifies finite labelled directed graphs which are parameteric
in some universe of datatypes. The names for nodes and labels are drawn
from a set of IRIs (Internationalized Resource Identifiers). For
TerminusDB we have chosen the *XSD* datatypes as our universe of
concrete values.

More formally, in TerminusDB a graph $G$ is a set of triples drawn from
the set $IRI \times IRI \times (IRI \oplus XSD)$ where $IRI$ is a set of
valid IRIs and $XSD$ is the set of valid XSD values. While some RDF
databases allow multiplicity of triples (i.e. a bag), the choice of a
set simplifies transaction processing in our setting.

For schema design TerminusDB uses the OWL language, with two
modifications to make it suitable as a schema language. Namely we
dispense with the open world interpretation and insist on the unique
name assumption[@DBLP:journals/semweb/FeeneyMB18]. This provides us with
a rich modelling language which can provide constraints on the allowable
shapes in the graph.

TerminusDB, following on from the RDF tradition, is not a property
graph. However it can model properties using an intermediate nodes and
this pattern can be made explicit in the OWL schema design. Again this
choice leads to simplicity of the underlying representation, which, as
we will see is important when constructing succinct data structures with
change sets.

Succinct Data Structures
------------------------

Succinct data structures[@Jacobson:1988:SSD:915547] are a family of data
structures which are close in size to the information theoretic minimum
representation. Technically they can be defined as data structures whose
size is:

$$n + o(n)$$

Where $n$ is the information theoretic minimum size. Succinct
representations are generally somewhat more computationally expensive
than less compact representations with pointers when working with small
problems. However, as the size of the datastructure grows, the ability
to avoid new cache reads at various levels of the memory hierarchy
(including reading information from disk) means that these
representations can prove very speed competitive[@doi:10.1002/spe.2198]
in practice.

TerminusDB largely borrows its graph data structure design from
HDT[@10.1007/978-3-642-30284-8_36] with some modifications which
simplify the use of change sets. The authors originally evaluated HDT as
a possibility for a graph which was too large to fit in memory when
loaded into postgresql and found that queries on the resulting graph
performed very well in practice.

In particular, the primary datastructures of the HDT format are
retained, namely *front coded dictionaries*, *bit sequences* and
*wavelet trees*.

### Plain Front-Coding Dictionary

Due to the unusual quantity of shared prefixes found in RDF data due to
the nature of URIs and IRIs, front-coding provides a fast dictionary
format with significant compression of data[@MARTINEZPRIETO201673].

The primary operations exposed by the datastructure are *string-id*
which gives us a natural number corresponding with the string, and
*id-string* which gives a string corresponding with a natural number.

  String          Offset Remainder
  ------------- -------- -------------
  Pearl Jam            0 Pearl Jam
  Pink Floyd           1 ink Floyd
  Pixies               2 xies
  The Beatles          0 The Beatles
  The Who              4 Who

  : Plain Front Coding Dictionary[]{label="tab:pfc"}

The data strucure sorts the strings and allows sharing of prefixes by
reference to the number of characters from the preceeding strings which
are shared. An example is given in
Table [\[tab:pfc\]](#tab:pfc){reference-type="ref" reference="tab:pfc"}.
The position in the dictionary gives us the implicit natural number
identifier.

### Succinct Graphs Encoding

Once subject, object and property of an edge have been appropriately
converted into integers by use of the subject-object dictionary, the
value dictionary and the predicate dictionary, we can use these integers
to encode the triples using bit sequences.

Succinct sequences encode sequences drawing from some alphabet $\sigma$.
In the case of a bit-sequence, $\sigma=\{0,1\}$. They typically expose
(at least) the following primitives:

-   $rank(a, S, i)$ which counts occurances of $a$ in the sequence from
    $S[0,i]$.

-   $select(a, S, i)$ which returns the location of the $i$-th occurance
    of $a$ in the sequence $S$.

-   $access(S, i)$ which returns the symbol at $S[i]$.

Given a sorted set of triples, for each subject identifier, in order
from $\{0..n\}$ where $n$ is the number of triples, we emit a $1$
followed by a $0$ for every predicate associated in a triple with that
subject. We then produce a vector of all predicates used and the
association with the subject is apparent from the position of zeros in
the bit sequence.

We repeat the process for predicates and objects resulting in a complete
encoded for our triples. We can see an example in
Table [\[tab:graph\]](#tab:graph){reference-type="ref"
reference="tab:graph"}. We have written the vectors in this table so
that the triples are vertically aligned, with subjects in blue,
predicates in red and objects in green in order to make the encoding
easier to see. The subject ids are actually implicit in the number of
$1$s encoding in the subject bit sequence and are only written in the
table for clarity.

  Triples                                                           Encoding                                                                                                         
  ----------------------------------------------------------------- ---------------------------------------------------------------------------------------------------------------- --------------------------------
  $(\textcolor{Blue}{1},\textcolor{Red}{2},\textcolor{Green}{3})$   $\textcolor{Blue}{1}\;\;\;\;\textcolor{Blue}{2} \;\;\;\;\textcolor{Blue}{3}$                                     Subject Ids
  $(\textcolor{Blue}{1},\textcolor{Red}{2},\textcolor{Green}{4})$   $1\;\;\;\;    1\;    0\;    1$                                                                                   Encoded Subject Bit Sequence
  $(\textcolor{Blue}{2},\textcolor{Red}{3},\textcolor{Green}{5})$   $\textcolor{Red}{2}\;\;\;\;\textcolor{Red}{3}\;\textcolor{Red}{4}\;\textcolor{Red}{5}$                           Predicate Vector
  $(\textcolor{Blue}{2},\textcolor{Red}{4},\textcolor{Green}{6})$   $1\;   0\;    1\;    1\;    1$                                                                                   Encoded Predicate Bit Sequence
  $(\textcolor{Blue}{3},\textcolor{Red}{5},\textcolor{Green}{7})$   $\textcolor{Green}{3}\;\textcolor{Green}{4}\;\textcolor{Green}{5}\;\textcolor{Green}{6}\;\textcolor{Green}{7}$   Object Vector

  : Succinct Graph Representation[]{label="tab:graph"}

This format allows fast lookup of triples based on query modes in which
the subject identifier is known, as we can use $select$ to find the
position in the predicate vector and subsequently use the predicate
identifier to $select$ in the object vector. We use a wavelet tree to
enable search starting from the predicate. Details of this can be found
in [@10.1007/978-3-642-30284-8_36].

Delta Encoded Databases
-----------------------

The use of *delta encoding* in software development is now ubiquitous
due to the enormous popularity of the *git* revision control system
which makes use of these techniques to store histories of revisions.

Git stores objects which contain either the complete dataset of interest
or the information about what is updated (deleted / added) as a delta.
All changes to the source control system are thereby simply management
problems of these objects.

This approach exposes a number of very powerful operations to sofware
developers collaborating on a code base. The operations include:

-   **History** Since new updates are actually layered over previous
    ones, developers can *time travel*, looking into the past, rolling
    back to the past, or even reapplying changes to current versions.

-   **Branching** Developers can create a derived version of a given
    code-base where additional operations can be performed without
    disrupting the original.

-   **Merging** When two branches diverge, the changes can be merged
    into a single version by choosing a strategy for combining changes.

TerminusDB uses an analogous approach to updates. A given database is
comprised of *layers* which stands in place of the objecs of git. Each
layer has a unique identifier, a 20-byte name. The base layer contains a
simple graph represented using the succinct data strucures already
described earlier.

![A graph composed of
layers[]{label="fig:layers"}](layers-diagram.png){#fig:layers
width="\\linewidth"}

Above this layer, we can have further layers. Each additional layer
above the base layer is comprised of additional dictionaries for newly
added subjects and objects, predicates or values. It also contains the
index structures used for the base graph to represent *positive* edges
which have been added to the graph. And we have a membership set of
*negative* edges which describe those triples which have been deleted as
shown in Figure [1](#fig:layers){reference-type="ref"
reference="fig:layers"}.

Each layer has a pointer to the previous layer which is achieved by
referring to its 20-byte name.

This immutable chain strucure allows for straightforward uncoordinated
multi-read access sometimes called muliversion concurrency control
(MVCC)[@Mohan:1992:EFM:130283.130306][@Sadoghi:2014:RDL:2733004.2733006].
This approach also makes branching simple to implement. Any number of
new layers can point to the same former parent layer.

In order to manage these layers as datastores, we use a *label*. A label
is a name which points to one of the 20-byte identifiers. In the present
implementation this is a file with the name of the label containing the
20-byte identifier.

### Dictionary modfications

Due to the use of delta encodings, new triples can be added which are
not present in the original dictionary. We therefore start new
dictionaries with a recorded offset, remembering the last bucket from
the previous dictionary.

Write Transactions
------------------

When an update transaction is initiated, a new *layer builder* is
created, which logs all newly inserted or deleted edges. When this
*layer builder* is committed, it yields a *layer* which has organised
the insertions in our succinct data structures.

In TerminusDB we require that graphs conform to the constraints imposed
by the OWL description of the dataset. This means that we produce a
hypothetical database by committing the layer builder without advancing
head. First we check the constraints hold on this new intermediate
database and after these are passed, it is safe to advance head to this
newly created layer. *Advancing* is done by side-effecting the label to
point to the new 20-byte value. The problem of coordination in the face
of side-effects is reduced to the problem of label management,
simplifying much of the architecture. A schematic of the workflow of the
write transaction is given in Figure [2](#workflow){reference-type="ref"
reference="workflow"}.

![Write transaction
workflow[]{label="workflow"}](query_write_commit_head){#workflow
width="\\linewidth"}

Delta compression
-----------------

As new updates are performed the database layer depth increases. This
will incur a performance penalty requiring repeated searching at each
layer for every query. In order to improve performance, we can perform a
*delta compression* similar to the mechnaims used in git. Alternatively
we can recalculate the full dataset as a new base-layer. In git this
*delta compression* step can be performed manually or it will occur when
a depth threshold is passed.

Since the layers are immutable, this operation can be done concurrently.
Commis that occur before the process is completed simply layer over the
delta with no visible change in the content of the database.

Compressed deltas of this type can allow older layers to be archived, or
even deleted. The removal of previous layers removes the capacity to
time-travel or to track whether the database arose from a branch.
However, this information can be kept seperately in a metadata
repository allowing memory of the branching structure and other
information about previous commits, but not the capacity to time-tralve
to them. We plan to implement this graph metadata repository in future
version of TerminusDB.

Future Work
===========

Values are stored as strings using a plain front coding dictionary
uniformly for all data types. Obviously this is less than ideal in that
it causes an expansion in size for the storage of integers, dates and
other specific types. It also means that only search from the beginning
of the datatype is optimised. In future versions of store we hope to
differentiate our indexing strategies for the various datatypes in XSD.

For strings the use of succinct data structure immediately suggests a
potential candidate: the FM-index[@Ferragina:2005:ICT:1082036.1082039].
With FM-indexing very large datasets could still have reasonable query
times for queries which are typically done on full text indexes using
inverted term-document indexing. We have yet to explore the candidates
for numeric and date types.

Currently the tracking of history and branches is implicit. We intend to
adopt a more explicit approach, storing a graph of the various commits
coupled with timestamps and other metadata which will facilitate
effective management.

Conclusion
==========

The use of advanced CI/CD workflows for databases as yet has not been
practical due to the lack of tool-chain support. In the software world
we have seen just what a large impact appropriate tools can make with
advent of git.

TerminusDB makes possible these collaborative CI/CD type operations in
the universe of data management.

This is made possible because of the synergies which an immutable
layered approach has with the *succinct datastructure* approach that we
have used for encoding.

TerminusDB provides a practical tool for enabling branch, merge,
rollback and the various automated and manual testing regimes which are
faciliated by them on a transactional database management system which
can provide sophisticated query support.
