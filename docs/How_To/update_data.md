# How to update a triple

## Python

```python
WOQL().woql_and(
    WOQL().woql_or(
        WOQL().woql_and(
            WOQL().triple("doc:my_obj","scm:my_property","v:Old_Value"),
            WOQL().delete_triple("doc:my_obj", "scm:my_property", "v:Old_Value")
        ),
        WOQL().true),
    
    WOQL().triple("doc:my_obj","scm:my_property","v:Old_Value")
)
```
