# How should you understand data provenance and why does it matter? What is TerminusDB going to do for me? 

TerminusDB uses an immutable database engine - this means that all historical states of the database can be queried at the same time. 

## The Commit Graph

To access historical states every TerminusDB database has a special graph called the commit graph. 

This contains a record of each and every change to the database, what exactly was changed and who changed it. 

TerminusDB stores an author and timestamp and message along with every change to any aspect of the database. 

This allows you to query the commit graph to find out exactly who made what changes and when. 

