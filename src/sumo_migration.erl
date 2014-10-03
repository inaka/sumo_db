-module(sumo_migration).

-behavior(sumo_doc).

-export([migrate/0, rollback/0]).

%%% sumo_db callbacks
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

-callback up() -> ok.
-callback down() -> ok.

-type version() :: atom().
-record(migration, {id :: integer(),
                    version :: version()}).

-type migration() :: #migration{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec migrate() -> ok.
migrate() ->
    ensure_sumo_migration_doc(),
    LastVersion = last_migration_version(),
    Versions = migration_update_list(LastVersion),
    lists:foreach(fun run_migration/1, Versions).

-spec rollback() -> ok.
rollback() ->
    ensure_sumo_migration_doc(),
    case last_migration_version() of
        undefined -> ok;
        LastVersion ->
            run_rollback(LastVersion)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% sumo_doc callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
    Fields = [sumo:new_field(id,      integer, [id, not_null, auto_increment]),
              sumo:new_field(version, string,  [{length, 255}, unique])],
    sumo:new_schema(?MODULE, Fields).

-spec sumo_sleep(migration()) -> sumo:doc().
sumo_sleep(Migration) ->
    [{id, Migration#migration.id},
     {version, atom_to_binary(Migration#migration.version, utf8)}].

-spec sumo_wakeup(sumo:doc()) -> migration().
sumo_wakeup(Migration) ->
    #migration{
       id = proplists:get_value(version, Migration),
       version = binary_to_atom(proplists:get_value(version, Migration), utf8)
      }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Make sure the schema_version table is present in the datastore.
-spec ensure_sumo_migration_doc() -> ok.
ensure_sumo_migration_doc() ->
    sumo:create_schema(?MODULE).

-spec last_migration_version() -> version() | undefined.
last_migration_version() ->
    case sumo:find_all(?MODULE) of
        [] -> undefined;
        Migrations ->
            Versions = [Version || #migration{version = Version} <- Migrations],
            lists:max(Versions)
    end.

-spec migration_update_list(version() | undefined) -> [version()].
migration_update_list(LastVersion) ->
    MigrationsDir = migrations_dir(),
    Files = filelib:wildcard("*.erl", MigrationsDir),

    F = compose([fun filename:rootname/1, fun list_to_atom/1]),
    AvailableVersions = lists:map(F, lists:sort(Files)),

    FunFilter = fun(X) -> (LastVersion == undefined) or (X > LastVersion) end,
    lists:filter(FunFilter, AvailableVersions).

-spec migrations_dir() -> string().
migrations_dir() ->
    case application:get_env(sumo_db, migrations_dir) of
        undefined -> "src/migrations";
        {ok, Dir} -> Dir
    end.

-spec run_migration(version()) -> ok.
run_migration(Version) ->
    Version:up(),
    Migration = #migration{version = Version},
    sumo:persist(sumo_migration, Migration).

-spec run_rollback(version()) -> ok.
run_rollback(Version) ->
    Version:down(),
    sumo:delete_by(sumo_migration, [{version, Version}]).

-spec compose([fun()]) -> fun().
compose(Funs) ->
    Compose = fun(F, X) -> F(X) end,
    fun(X) ->
            lists:foldl(Compose, X, Funs)
    end.
