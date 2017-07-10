# Custom Sumo Store Example

This example is about creating a store in order to add some functionality.
We are going to use `PostgreSQL` as a backend and we want to store people (only with _name_ and _city_ fields).

# Requirements
- `PostgreSQL` installed and a database created (in our code we called it _store_example_)

## The Problem

`SumoDB` provides us a great bunch of functions for CRUD operations but we need a special one. We would like
to have a `count_by_city/1` function which returns us the number of people from a given city.

With the standard `SumoDB` API we could filter by city and then return the size of the array but we want to use the native power of `PostgreSQL`.

## Creating our own store

As we are using `PostgreSQL` we are going to use our `sumo_db_pgsql` adapter for sumo, you can find it [here](https://github.com/inaka/sumo_db_pgsql).
This Adapter provides us a store called `sumo_db_pgsql` which provides us the standard `SumoDB` API. We want to extend that module and add our custom `count_by_city/1` function. In order to achive that we create our new `our_pgsql_store` which gets all the functionality from `sumo_db_pgsql` using `mixer`.

```erlang
-mixin([sumo_store_pgsql]).
```

Then we can create our function:

```erlang
count_by_city(City, DocName, State) ->
  #{conn := Conn} = State,
  Query = [
    "SELECT Count(city) FROM ", escape(DocName),
    " GROUP BY CITY ",
    " HAVING ",
    " city = $1"

  ],
  parse_result(epgsql:equery(Conn, stringify(Query), [City]), State).
```

We are using `epgsql` directly from here because it is the driver we are using in the adapter.
Don't worry about `escape/1`, `stringify/1` and `parse_result/1`, you can check the real code in the [source](src/our_pgsql_store.erl).

Now we have to use this new store on our config file.

## Calling to our new function

We have added the function to our store but now we have to call that function integrated with `SumoDB`, we are going to create a new function in `store_example` module:

```erlang
count_by_city(City) ->
  sumo:call(people, count_by_city, [City]).
```

This is how we call directly to a store. The first parameter is the model (defined in the config), the second one is the name of the store's function (our new created function) and the last one is a list with the parameters the that new function, in this case only one parameter.

and thats all!

## Running the example

- First clone the code to your local system.
- You must create a new database in `PostgreSQL`.
- Review the `config/sys.config` info about your database, by default the database configuration is:

```erlang
{host,     "127.0.0.1"},
{port,      5432},
{database, "store_example"},
{username, "ferigis"},
{password, "123456"}
```
replace what you need
- Compile the code

```
rebar3 compile
```

- Run

```
erl -pa _build/default/lib/*/ebin -config config/sys.config -s store_example
```

## Play!

Lets create some people:
```erlang
2> sumo:persist(people, person:new("Felipe", "San Jose")).
#{city => "San Jose",id => 1,name => "Felipe"}
3> sumo:persist(people, person:new("Carlos", "Cali")).
#{city => "Cali",id => 2,name => "Carlos"}
4> sumo:persist(people, person:new("Brujo", "Buenos Aires")).
#{city => "Buenos Aires",id => 3,name => "Brujo"}
5> sumo:persist(people, person:new("Marcos", "Buenos Aires")).
#{city => "Buenos Aires",id => 4,name => "Marcos"}
6> sumo:persist(people, person:new("Euen", "Buenos Aires")).
#{city => "Buenos Aires",id => 5,name => "Euen"}
```

Lets find one of them (here we are checking we have extended from the default PostgreSQL store correctly):

```erlang
7> sumo:find_by(people, [{name, "Brujo"}]).
[#{city => <<"Buenos Aires">>,id => 3,name => <<"Brujo">>}]
```

Great!

Now lets use our new function:

```erlang
8> store_example:count_by_city("Buenos Aires").
3
9> store_example:count_by_city("San Jose").
1
10> store_example:count_by_city("Cali").
1
11> store_example:count_by_city("Namek").
0
```

It worked!

This is the idea behind creating your own store
