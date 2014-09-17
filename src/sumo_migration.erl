-module(sumo_migration).

-behavior(sumo_doc).

-export([migrate/0, rollback/0]).

-export([migration_update_list/1]).

%%% sumo_db callbacks
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

-callback up() -> string().
-callback down() -> string().

-type migration() :: atom().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec migrate() -> ok.
migrate() ->
    ensure_sumo_migration_doc(),
    LastMigration = last_migration_update(),
    Migrations = migration_update_list(LastMigration),
    lists:foreach(fun(M) -> M:up() end, Migrations).

-spec rollback() -> ok.
rollback() ->
    ensure_sumo_migration_doc(),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% sumo_doc callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
    Fields = [sumo:new_field(version, string)],
    sumo:new_schema(?MODULE, Fields).

-spec sumo_sleep(migration()) -> sumo:doc().
sumo_sleep(Migration) ->
    [{version, Migration}].

-spec sumo_wakeup(sumo:doc()) -> migration().
sumo_wakeup(Migration) ->
    VersionBin = proplists:get_value(version, Migration),
    binary_to_atom(VersionBin, utf8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Make sure the schema_version table is present in the datastore.
-spec ensure_sumo_migration_doc() -> ok.
ensure_sumo_migration_doc() ->
    sumo:create_schema(?MODULE).

-spec last_migration_update() -> migration().
last_migration_update() ->
    Migrations = sumo:find_all(?MODULE),
    lists:max(Migrations).

-spec migration_update_list(migration()) -> [migration()].
migration_update_list(LastMigration) ->
    MigrationsDir = migrations_dir(),
    Files = filelib:wildcard("*.erl", MigrationsDir),
    F = compose([fun filename:rootname/1, fun list_to_atom/1]),
    AvailableMigrations = lists:map(F, Files),
    lists:filter(fun(X) -> X > LastMigration end, AvailableMigrations).

-spec migrations_dir() -> string().
migrations_dir() ->
    case application:get_env(migrations_dir, sumo_db) of
        undefined -> "src/migrations";
        Dir -> Dir
    end.

-spec compose([fun()]) -> fun().
compose(Funs) ->
    Compose = fun(F, X) -> F(X) end,
    fun(X) ->
            lists:foldl(Compose, X, Funs)
    end.
