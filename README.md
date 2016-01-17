mongoql
=======

An OTP library to translate a SQL-like query in a MongoDB query.


Examples
--------

E.g. select data where temperature>23 AND house is in Milano and
pression <= 1015, ordered by house name ascending:

```
house.temperature>23 house.city:"Milano" house.pression<:1015 +house.name
```

Translated in the follow MongoDB query:

```erlang
Query = {
  '$query', {
    '$and', [
      {<<"house.temperature">>, {<<"$gt">>,23}},
      {<<"house.city">>, {<<"$eq">>,<<"Milano">>}},
      {<<"house.pression">>, {<<"$lte">>,1015}}
    ]
  },
  '$orderby', [
    {<<"house.name">>,1}
  ]
},
mongopool_app:find(Pool, Table, Query).
```

How to use
----------

```erlang
MyQueryString = "house.temperature>23 house.city:\"Milano\" house.pression<:1015 +house.name"
{ok, Query} = mongoql:parse(MyQueryString),
mongopool_app:find(Pool, Table, Query).
```

Build
-----

    $ ./utils/rebar3 compile
