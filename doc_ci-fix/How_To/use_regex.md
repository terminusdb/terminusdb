# How to use regex in WOQL

WOQL makes available PCRE regular expressions using the predicate
`regexp` with the structure:

```javascript
regexp(pattern, string, match_list)
```
For instance we can match the string `"test"` with a pattern as follows:

```javascript
regexp("t(..)t","test", ["v:All", "v:Match"])
```

This gives the result:

```
-----------------
| All  |  Match |
-----------------
| test |  es    |
-----------------
```

The first result `"v:ALL"` matches the entire string, whereas each
subsequent variable in the list is a match for the subsequent groups
(those parts contained in parentheses).

```javascript
regexp("m...h","this is a match of a string", ["v:All"])
```

In this case we extract only the matching section.

```
---------
| All   |
---------
| match |
---------
```

If we want to ensure we start the pattern at the beginning of the
string, we can use standard PCRE anchoring:

```javascript
regexp("^m...h$","this is a match of a string", ["v:All"])
```

The above query results in no matches, whereas:

```javascript
regexp("^m...h$","match", ["v:All"])
```

Obtains the match:

```
---------
| All   |
---------
| match |
---------
```

More match groups simply result in a greater number of elements in the
match list, for instance

```javascript
regexp("^(....)-(..)-(..)$","2020-10-12", ["v:All", "v:Year", "v:Month", "v:Day"])
```

Which allows us to extract the year, month and day from a date:

```
--------------------------------------
| All	        | Year | Month | Day |
--------------------------------------
| 2020-10-12 	| 2020 | 10    | 12  |
--------------------------------------
```

