mongoql
=======

[![Build Status](https://travis-ci.org/hachreak/mongoql.svg?branch=master)](https://travis-ci.org/hachreak/mongoql)

An OTP library to translate a special search query language
(including aggregation) in MongoDB query.

Note: the queries are compatible with
[mongodb-erlang](https://github.com/comtihon/mongodb-erlang) package.

Examples Query
--------------

E.g. select data where temperature>23 AND house is in Milano and
pression <= 1015, ordered by house name ascending:

```
house.temperature>23 house.city:"Milano" house.pression<:1015 house.when>2017-12-15T10:20:00Z house.name-asc
```

Translated in the follow MongoDB query:

```erlang
Query = {
  '$query', {
    '$and', [
      {<<"house.temperature">>, {'$gt', 23}},
      {<<"house.city">>, {'$eq', <<"Milano">>}},
      {<<"house.pression">>, {'$lte', 1015}},
      {<<"house.when">>, {'$gt', {1513, 333200, 0}}}
    ]
  },
  '$orderby', [
    {<<"house.name">>, 1}
  ]
},
mongopool_app:find(Pool, Table, Query).
```

Example Aggregation
-------------------

E.g. count how many times received a new log message today
grouped by log level:

```
date > now-24h date-asc $group id: level count: $count(1)
```

How to use
----------

Simple query:

```erlang
MyQueryString = "house.temperature>23 house.city:\"Milano\" house.pression<:1015 house.when>2017-12-15T10:20:00Z house.when < now house.name-asc",
{ok, Query} = mongoql:parse(MyQueryString),
mongopool_app:find(Pool, Table, Query).
```

Aggregation query:

```erlang
MyAggString = "date > now-24h date-asc $group id: level count: $count(1)"
{ok, Agg} = mongoql:agg(MyAggString),
mongopool_app:command(Pool, TableInBinaryString, pipeline, Agg).
```

Operators
---------

Op.            | Name             | Example
---------------|------------------|------------------------------------------
`<`            | Minor            | `temperature < 10.5`
`<:`           | Minor Equal      | `temperature <: 7.3`
`:`            | Equal            | `temperature : 5` or `name : "FuuBar"`
`>:`           | Major Equal      | `temperature >: 2`
`>`            | Major            | `temperature > 4.4`
`!:`           | Not Equal        | `temperature !: 4` or `name !: "FuuBar"`
`~`            | Regex            | `name ~ "Mi*"`
`in`           | In               | `temperature in [16 17 18]` or `city in ["Milano" "Roma"]`
`not`          | Not              | `not temperature > 5` or `not name in ["Milano" "Roma"]`
`exists`       | Exists*(#)       | `name exists` or `not name exists`
`{name}-asc`   | Order Ascending  | `name-asc`
`{name}-desc`  | Order Descending | `name-desc`


(#) matches the documents that contain the field.

Types supported
---------------

Type     | Example
---------|----------------------
Integer  | `15`, `-23`, `543`
Float    | `34.56`, `-235.32`
String   | `"Hello world!"`
Datetime | `2016-01-15T18:19:28Z` (Note: without doublequote)
Datetime | `now` (automatically translated with now datetime of the server
Datetime | `now - 1h`, `now - 1m`, `now - 1s`


Build
-----

    $ ./utils/rebar3 compile
