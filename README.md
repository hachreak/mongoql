mongoql
=======

An OTP library to translate a search query language in a MongoDB query.


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

Operators
---------

Op. | Name             | Example
----|------------------|------------------------------------------
 <  | Minor            | `temperature < 10.5`
 <: | Minor Equal      | `temperature <: 7.3`
 :  | Equal            | `temperature : 5` or `name : "FuuBar"`
 >: | Major Equal      | `temperature >: 2`
 >  | Major            | `temperature > 4.4`
 !: | Not Equal        | `temperature !: 4` or `name |: "FuuBar"`
 +  | Order Ascending  | `+name`
 -  | Order Descending | `-name`


Build
-----

    $ ./utils/rebar3 compile
