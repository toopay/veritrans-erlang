-module(veritrans).
-behaviour(gen_server).

%% API
-export([start_link/0, start/0, stop/0]).
-export([
    set_key/1,
    set_flag/1,
    charge/1,
    capture/1,
    approve/1,
    cancel/1,
    status/1,
    expire/1
]).

%% Gen server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Constant and record
-record(state, {key = <<"undefined">>, flag = sandbox}).
-define(DEFAULT_TIMEOUT, 60000).

%% Include generic headers (datatype and endpoint constants)
-include("veritrans.hrl").


%%====================================================================
%% API
%%====================================================================
%% @doc Starts the veritrans process linked to the calling process. Usually invoked by the supervisor veritrans_sup
%% @spec start_link() -> {ok, pid()}
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Starts the veritrans process without linking. 
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], [{debug, []}]).

%% @doc Stop the veritrans process. 
stop() ->
    case catch gen_server:call(?MODULE, stop) of
        {'EXIT',{noproc,_}} ->
            ok;
        Res ->
            Res
    end.

%% @doc Set the server key
set_key(Key) ->
    gen_server:cast(?MODULE, {change_key, Key}), %% Async call to set the current key
    ok.

%% @doc Set the server flag
set_flag(Flag) ->
    gen_server:cast(?MODULE, {change_flag, Flag}), %% Async call to set the current flag
    ok.

%% @doc Do the charge
charge(Data) ->
    gen_server:call(?MODULE, {post, ?CHARGE_ENDPOINT, Data},?DEFAULT_TIMEOUT).

%% @doc Capture authorized transaction
capture(Data) ->
    gen_server:call(?MODULE, {post, ?CAPTURE_ENDPOINT, Data},?DEFAULT_TIMEOUT).

%% @doc Get a transaction status
status(TransactionId) ->
    gen_server:call(?MODULE, {get_with_placeholder, ?STATUS_SUFFIX_ENDPOINT, get_binary(TransactionId)},?DEFAULT_TIMEOUT).

%% @doc Approve a pending transaction
approve(TransactionId) ->
    gen_server:call(?MODULE, {post_with_placeholder, ?APPROVE_SUFFIX_ENDPOINT, get_binary(TransactionId)},?DEFAULT_TIMEOUT).

%% @doc Cancel a pending transaction
cancel(TransactionId) ->
    gen_server:call(?MODULE, {post_with_placeholder, ?CANCEL_SUFFIX_ENDPOINT, get_binary(TransactionId)},?DEFAULT_TIMEOUT).

%% @doc Expires a pending transaction
expire(TransactionId) ->
    gen_server:call(?MODULE, {post_with_placeholder, ?EXPIRE_SUFFIX_ENDPOINT, get_binary(TransactionId)},?DEFAULT_TIMEOUT).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init(_) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({post, Endpoint, Params}, _From, State) ->
    Url = build_url(State#state.flag, Endpoint),
    Reply = case ibrowse:send_req(get_list(Url), build_headers(), post, jsx:encode(Params), build_options(State#state.key)) of
        {ok, _Status, _Head, Body} -> {ok, get_data(Body)};
        {error, Reason} -> {error, get_binary(Reason)}
    end,
    {reply, Reply, State};
handle_call({post_with_placeholder, Endpoint, Param}, _From, State) ->
    Url = build_url_with_prefix(State#state.flag, Endpoint, Param),
    Reply = case ibrowse:send_req(get_list(Url), build_headers(), post, [], build_options(State#state.key)) of
        {ok, _Status, _Head, Body} -> {ok, get_data(Body)};
        {error, Reason} -> {error, get_binary(Reason)}
    end,
    {reply, Reply, State};
handle_call({get_with_placeholder, Endpoint, Param}, _From, State) ->
    Url = build_url_with_prefix(State#state.flag, Endpoint, Param),
    Reply = case ibrowse:send_req(get_list(Url), build_headers(), get, [], build_options(State#state.key)) of
        {ok, _Status, _Head, Body} -> {ok, get_data(Body)};
        {error, Reason} -> {error, get_binary(Reason)}
    end,
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({change_key, Key}, State) ->
    {noreply, State#state{key=Key}};
handle_cast({change_flag, Flag}, State) ->
    {noreply, State#state{flag=Flag}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal function
%%====================================================================
%% @doc Generate a url
build_url(Flag, Endpoint) ->
    Root = case Flag of
        live -> ?LIVE_ROOT;
        _ -> ?SANDBOX_ROOT
    end,
    <<Root/binary,Endpoint/binary>>.

%% @doc Generate a url with prefix
build_url_with_prefix(Flag, Endpoint, Prefix) ->
    build_url(Flag, <<"/",Prefix/binary,Endpoint/binary>>).

%% @doc Generates headers
build_headers() ->
    [
        {"Content-Type","application/json"},
        {"Accept","application/json"}
    ].

%% @doc Generates options
build_options(Key) ->
    [{basic_auth,{get_list(Key), ""}}].

%% @doc Get the data
get_data(MaybeJson) ->
    try
        jsx:decode(get_binary(MaybeJson))
    catch _:_ ->
        get_binary(MaybeJson)
    end.

%% @doc Get the list representation of either binary or list
get_list(Binary) when is_binary(Binary)-> binary_to_list(Binary);
get_list(List) -> List.

%% @doc Get the binary representation of either undefined, integer, list, tuple or atom
get_binary(undefined) -> <<>>;
get_binary(Atom) when is_atom(Atom)-> list_to_binary(atom_to_list(Atom));
get_binary(Binary) when is_binary(Binary)-> Binary;
get_binary(Integer) when is_integer(Integer)-> integer_to_binary(Integer);
get_binary(List) when is_list(List)-> list_to_binary(List);
get_binary(Tuple) when is_tuple(Tuple) ->
    {_, Val} = Tuple,
    get_binary(Val).