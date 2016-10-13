-module(sumo_test_people_events_manager).

-behaviour(gen_event).

-export([
  init/1,
  terminate/2,
  handle_info/2,
  handle_call/2,
  code_change/3,
  handle_event/2
]).

-export([
  pick_up_event/0,
  clean_events/0
]).

-record(state, {event_list = [] :: list()}).
-type state() :: #state{}.

-spec pick_up_event() -> tuple() | no_event.
pick_up_event() ->
  gen_event:call(?MODULE, ?MODULE, pick_up_event).

-spec clean_events() -> ok.
clean_events() ->
  gen_event:call(?MODULE, ?MODULE, clean_events).

-spec init([]) -> {ok, state()}.
init([]) ->
  {ok, #state{}}.

-spec handle_info(term(), state()) -> {ok, state()}.
handle_info(_Info, State) ->
  {ok, State}.

-spec handle_call(Event, State) -> Result when
  Event  :: term(),
  State  :: state(),
  Result :: {ok, ok | no_event | not_implemented | term(), state()}.
handle_call(clean_events, _State) ->
  {ok, ok, #state{event_list = []}};
handle_call(pick_up_event, #state{event_list = EventList} = State) ->
  try
    Event = lists:last(EventList),
    {ok, Event, #state{event_list = lists:delete(Event, EventList)}}
  catch
    error:function_clause ->
      {ok, no_event, State}
  end;
handle_call(_, State) ->
  {ok, not_implemented, State}.

-spec handle_event(term(), state()) -> {ok, state()}.
handle_event(Event, #state{event_list = EventList}) ->
  {ok, #state{event_list = [Event | EventList]}}.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Arg, _State) ->
  ok.
