mongoql
=======

[![Build Status](https://travis-ci.org/hachreak/mongoql.svg?branch=master)](https://travis-ci.org/hachreak/mongoql)

An OTP library to translate a special search query language in a MongoDB query.


Examples
--------

E.g. select data where temperature>23 AND house is in Milano and
pression <= 1015, ordered by house name ascending:

```
house.temperature>23 house.city:"Milano" house.pression<:1015 house.name-asc
```

Translated in the follow MongoDB query:

```erlang
Query = {
  '$query', {
    '$and', [
      {<<"house.temperature">>, {'$gt', 23}},
      {<<"house.city">>, {'$eq', <<"Milano">>}},
      {<<"house.pression">>, {'$lte', 1015}}
    ]
  },
  '$orderby', [
    {<<"house.name">>, 1}
  ]
},
mongopool_app:find(Pool, Table, Query).
```

How to use
----------

```erlang
MyQueryString = "house.temperature>23 house.city:\"Milano\" house.pression<:1015 house.name-asc"
{ok, Query} = mongoql:parse(MyQueryString),
mongopool_app:find(Pool, Table, Query).
```

Operators
---------

Op.           | Name             | Example
--------------|------------------|------------------------------------------
 <            | Minor            | `temperature < 10.5`
 <:           | Minor Equal      | `temperature <: 7.3`
 :            | Equal            | `temperature : 5` or `name : "FuuBar"`
 >:           | Major Equal      | `temperature >: 2`
 >            | Major            | `temperature > 4.4`
 !:           | Not Equal        | `temperature !: 4` or `name !: "FuuBar"`
 {name}-asc   | Order Ascending  | `name-asc`
 {name}-desc  | Order Descending | `name-desc`


Types supported
---------------

Type     | Example
---------|----------------------
Integer  | `15`, `-23`, `543`
Float    | `34.56`, `-235.32`
String   | `"Hello world!"`
Datetime | `2016-01-15T18:19:28Z`


Build
-----

    $ ./utils/rebar3 compile
