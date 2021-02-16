
## Mathematics

You may want to perform some mathematical operations on data
going into or out of the database. WOQL has basic mathematical
expressions: `plus`, `multiply`, `divide`, `exp`, `div` (for integer
division) and others. To use them you need to put the mathematical
expression in a form called `WOQL.eval` as follows:

```javascript
let [ x ] = vars('x')
WOQL.eval(plus(1,2), x)
```

This binds `x` to the result of the addition of `1` and `2`. You can
use variables in place of `1` and `2` and re-use `x` in later
queries. For instance we could write:

```javascript
let [product,result] = vars('product', 'result')
and(
    WOQL.eval(times(3,2), product),
    WOQL.eval(div(product,2), result)
)
```

Here of course we get back the number `3` as we'd expect. The complete
definition of mathematical operators is in the python-client
reference documentation.
