-module(veritrans).
-behaviour(gen_server).

%% API
-export([start_link/0, start/0, stop/0]).
-export([
    set_key/1,
    set_flag/1,
    address/7,
    item_detail/4,
    customer_details/6,
    charge/1,
    capture/1,
    approve/1,
    cancel/1,
    status/1,
    expire/1
]).
-export([
    charge_credit_card/5
    ,charge_credit_card/6
    ,charge_mandiri_clickpay/7
    ,charge_mandiri_clickpay/8
    ,charge_mandiri_echannel/5
    ,charge_mandiri_echannel/6
    ,charge_mandiri_ecash/6
    ,charge_cimb_clicks/7
    ,charge_virtual_account/7
    ,charge_bri_epay/6
    ,charge_tcash/7
    ,charge_xl_tunai/6
    ,charge_bbm_money/6
    ,charge_cstore/8
    ,charge_indosat_dompetku/7
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

%% @doc Bundle address
-spec address(binary(), binary(), binary(), binary(), 
                binary(), binary(), binary()) -> address().
address(FirstName, LastName, Address, City, PostalCode, Phone, CountryCode) ->
    #address{
        first_name=FirstName,
        last_name=LastName,
        address=Address,
        city=City,
        postal_code=PostalCode,
        phone=Phone,
        country_code=CountryCode
    }.

%% @doc Bundle item details
-spec item_detail(binary(), integer(), integer(), binary()) -> item_detail(). 
item_detail(Id, Price, Quantity, Name) -> 
    #item_detail{id=Id,price=Price,quantity=Quantity,name=Name}.

%% @doc Bundle customer details
-spec customer_details(binary(), binary(), binary(), 
                        binary(), address(), address()) -> customer_details(). 
customer_details(FirstName, LastName, Email, Phone, BillingAddress, ShippingAddress) ->
        #customer_details{
            first_name=FirstName,
            last_name=LastName,
            email=Email,
            phone=Phone,
            billing_address=BillingAddress,
            shipping_address=ShippingAddress
        }.

%% @doc Charge credit card 
-spec charge_credit_card(binary(), 
                        binary(), 
                        binary(), 
                        integer(), 
                        list()) -> tuple().
charge_credit_card(Token, Bank, OrderId,
                    GrossAmount, ItemDetails) ->
    charge_credit_card(Token, Bank, OrderId,
                        GrossAmount, ItemDetails, #customer_details{}).
-spec charge_credit_card(binary(), binary(), binary(),
                        integer(), list(), customer_details()) -> tuple().
charge_credit_card(Token, Bank, OrderId,
                    GrossAmount, ItemDetails, CustomerDetails) ->
    charge(#credit_card_data{
            credit_card=#card{token_id=Token,bank=Bank},
            transaction_details=#transaction_details{order_id=OrderId,gross_amount=GrossAmount},
            item_details=ItemDetails,
            customer_details=CustomerDetails
        }).

%% @doc Charge Mandiri Click Pay
-spec charge_mandiri_clickpay(binary(), binary(), binary(),
                                binary(), binary(), binary(),
                                integer()) -> tuple().
charge_mandiri_clickpay(CardNumber, Input1, Input2,
                        Input3, Token, OrderId,
                        GrossAmount) ->
    charge_mandiri_clickpay(CardNumber, Input1, Input2,
                            Input3, Token, OrderId,
                            GrossAmount, #customer_details{}).
-spec charge_mandiri_clickpay(binary(), binary(), binary(),
                                binary(), binary(), binary(),
                                integer(), customer_details()) -> tuple().
charge_mandiri_clickpay(CardNumber, Input1, Input2,
                        Input3, Token, OrderId,
                        GrossAmount, CustomerDetails) ->
    charge(#mandiri_clickpay_data{
            mandiri_clickpay=#mandiri_clickpay{
                card_number=CardNumber,
                input1=Input1,
                input2=Input2,
                input3=Input3,
                token=Token
            },
            transaction_details=#transaction_details{order_id=OrderId,gross_amount=GrossAmount},
            customer_details=CustomerDetails
        }).

%% @doc Charge Mandiri EChannel
-spec charge_mandiri_echannel(binary(), binary(), binary(),
                                integer(), list()) -> tuple().
charge_mandiri_echannel(BillInfo1, BillInfo2, OrderId,
                        GrossAmount, ItemDetails) ->
    charge_mandiri_echannel(BillInfo1, BillInfo2, OrderId,
                            GrossAmount, ItemDetails, #customer_details{}).
-spec charge_mandiri_echannel(binary(), binary(), binary(),
                                integer(), list(), customer_details()) -> tuple().
charge_mandiri_echannel(BillInfo1, BillInfo2, OrderId,
                        GrossAmount, ItemDetails, CustomerDetails) ->
    charge(#mandiri_echannel_data{
            echannel=#mandiri_echannel{bill_info1=BillInfo1,bill_info2=BillInfo2},
            transaction_details=#transaction_details{order_id=OrderId,gross_amount=GrossAmount},
            item_details=ItemDetails,
            customer_details=CustomerDetails
        }).

%% @doc Charge Mandiri ECash
-spec charge_mandiri_ecash(binary(), binary(), binary(), binary(), binary(), binary()) -> tuple().
charge_mandiri_ecash(OrderId, GrossAmount, FirstName, LastName, Email, Phone) -> 
     charge(#mandiri_ecash_data{
            transaction_details=#transaction_details{order_id=OrderId,gross_amount=GrossAmount},
            customer_details=#customer_details{
                first_name=FirstName,
                last_name=LastName,
                email=Email,
                phone=Phone
            }
        }).

%% @doc Charge CIMB Clicks
-spec charge_cimb_clicks(binary(), binary(), binary(), binary(),
                        binary(), binary(), binary()) -> tuple().
charge_cimb_clicks(Description, OrderId, GrossAmount, FirstName, LastName, Email, Phone) -> 
     charge(#cimb_clicks_data{
            cimb_clicks=#cimb_clicks{description=Description},
            transaction_details=#transaction_details{order_id=OrderId,gross_amount=GrossAmount},
            customer_details=#customer_details{
                first_name=FirstName,
                last_name=LastName,
                email=Email,
                phone=Phone
            }
        }).

%% @doc Charge virtual account
-spec charge_virtual_account(binary(), binary(), binary(), binary(),
                        binary(), binary(), binary()) -> tuple().
charge_virtual_account(BankName, OrderId, GrossAmount, FirstName, LastName, Email, Phone) -> 
     charge(#virtual_account_data{
            bank_transfer=#virtual_account{bank=BankName},
            transaction_details=#transaction_details{order_id=OrderId,gross_amount=GrossAmount},
            customer_details=#customer_details{
                first_name=FirstName,
                last_name=LastName,
                email=Email,
                phone=Phone
            }
        }).

%% @doc Charge BRI EPay
-spec charge_bri_epay(binary(), binary(), binary(), binary(), binary(), binary()) -> tuple().
charge_bri_epay(OrderId, GrossAmount, FirstName, LastName, Email, Phone) -> 
     charge(#bri_epay_data{
            transaction_details=#transaction_details{order_id=OrderId,gross_amount=GrossAmount},
            customer_details=#customer_details{
                first_name=FirstName,
                last_name=LastName,
                email=Email,
                phone=Phone
            }
        }).

%% @doc Charge TCash
-spec charge_tcash(binary(), binary(), binary(), binary(), binary(), binary(), binary()) -> tuple().
charge_tcash(TokenNumber, OrderId, GrossAmount, FirstName, LastName, Email, Phone) -> 
     charge(#tcash_data{
            telkomsel_cash=#tcash{customer=TokenNumber},
            transaction_details=#transaction_details{order_id=OrderId,gross_amount=GrossAmount},
            customer_details=#customer_details{
                first_name=FirstName,
                last_name=LastName,
                email=Email,
                phone=Phone
            }
        }).

%% @doc Charge XL Tunai
-spec charge_xl_tunai(binary(), binary(), binary(), binary(), binary(), binary()) -> tuple().
charge_xl_tunai(OrderId, GrossAmount, FirstName, LastName, Email, Phone) -> 
     charge(#xl_tunai_data{
            transaction_details=#transaction_details{order_id=OrderId,gross_amount=GrossAmount},
            customer_details=#customer_details{
                first_name=FirstName,
                last_name=LastName,
                email=Email,
                phone=Phone
            }
        }).

%% @doc Charge BBM Money
-spec charge_bbm_money(binary(), binary(), binary(), binary(), binary(), binary()) -> tuple().
charge_bbm_money(OrderId, GrossAmount, FirstName, LastName, Email, Phone) -> 
     charge(#bbm_money_data{
            transaction_details=#transaction_details{order_id=OrderId,gross_amount=GrossAmount},
            customer_details=#customer_details{
                first_name=FirstName,
                last_name=LastName,
                email=Email,
                phone=Phone
            }
        }).

%% @doc Charge CStore
-spec charge_cstore(binary(), binary(), binary(), binary(), binary(), binary(), binary(), binary()) -> tuple().
charge_cstore(Store, Message, OrderId, GrossAmount, FirstName, LastName, Email, Phone) -> 
     charge(#indomaret_data{
            cstore=#cstore{store=Store, message=Message},
            transaction_details=#transaction_details{order_id=OrderId,gross_amount=GrossAmount},
            customer_details=#customer_details{
                first_name=FirstName,
                last_name=LastName,
                email=Email,
                phone=Phone
            }
        }).

%% @doc Charge Indosat Dompetku
-spec charge_indosat_dompetku(binary(), binary(), binary(), binary(), binary(), binary(), binary()) -> tuple().
charge_indosat_dompetku(MSISDN, OrderId, GrossAmount, FirstName, LastName, Email, Phone) -> 
     charge(#indosat_dompetku_data{
            indosat_dompetku=#indosat_dompetku{msisdn=MSISDN},
            transaction_details=#transaction_details{order_id=OrderId,gross_amount=GrossAmount},
            customer_details=#customer_details{
                first_name=FirstName,
                last_name=LastName,
                email=Email,
                phone=Phone
            }
        }).

%% @doc Do the charge
-spec charge(credit_card_data()
            | mandiri_clickpay_data()
            | mandiri_echannel_data()
            | mandiri_ecash_data()
            | cimb_clicks_data()
            | virtual_account_data()
            | bri_epay_data()
            | tcash_data()
            | xl_tunai_data()
            | bbm_money_data()
            | indomaret_data()
            | indosat_dompetku_data()) -> tuple().
charge(Data) ->
    Payload = filter_payload(get_payload(Data)),
    gen_server:call(?MODULE, {post, ?CHARGE_ENDPOINT, Payload},?DEFAULT_TIMEOUT).

%% @doc Capture authorized transaction
capture(Data) ->
    gen_server:call(?MODULE, {post, ?CAPTURE_ENDPOINT, Data},?DEFAULT_TIMEOUT).

%% @doc Get a transaction status
status(TransactionId) ->
    gen_server:call(?MODULE, {
        get_with_placeholder, 
        ?STATUS_SUFFIX_ENDPOINT, 
        get_binary(TransactionId)
    },?DEFAULT_TIMEOUT).

%% @doc Approve a pending transaction
approve(TransactionId) ->
    gen_server:call(?MODULE, {
        post_with_placeholder, 
        ?APPROVE_SUFFIX_ENDPOINT, 
        get_binary(TransactionId)
    },?DEFAULT_TIMEOUT).

%% @doc Cancel a pending transaction
cancel(TransactionId) ->
    gen_server:call(?MODULE, {
        post_with_placeholder, 
        ?CANCEL_SUFFIX_ENDPOINT, 
        get_binary(TransactionId)
    },?DEFAULT_TIMEOUT).

%% @doc Expires a pending transaction
expire(TransactionId) ->
    gen_server:call(?MODULE, {
        post_with_placeholder, 
        ?EXPIRE_SUFFIX_ENDPOINT, 
        get_binary(TransactionId)
    },?DEFAULT_TIMEOUT).

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
    Reply = case ibrowse:send_req(
                get_list(Url), 
                build_headers(), 
                post, 
                jsx:encode(Params), 
                build_options(State#state.key)) of
        {ok, _Status, _Head, Body} -> {ok, get_data(Body)};
        {error, Reason} -> {error, get_binary(Reason)}
    end,
    {reply, Reply, State};
handle_call({post_with_placeholder, Endpoint, Param}, _From, State) ->
    Url = build_url_with_prefix(State#state.flag, Endpoint, Param),
    Reply = case ibrowse:send_req(
                get_list(Url), 
                build_headers(), 
                post, [], build_options(State#state.key)) of
        {ok, _Status, _Head, Body} -> {ok, get_data(Body)};
        {error, Reason} -> {error, get_binary(Reason)}
    end,
    {reply, Reply, State};
handle_call({get_with_placeholder, Endpoint, Param}, _From, State) ->
    Url = build_url_with_prefix(State#state.flag, Endpoint, Param),
    Reply = case ibrowse:send_req(
                get_list(Url), 
                build_headers(), 
                get, [], build_options(State#state.key)) of
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

%% @doc Filter payload
filter_payload(Payload) ->
    lists:filtermap(fun
        % Filter item detail(s)
        (#item_detail{} = MaybeItemDetail) -> 
            ItemDetail = filter_payload(get_payload(MaybeItemDetail)),
            HasItemDetail = length(ItemDetail) > 0,
            case HasItemDetail of 
                true -> {true,ItemDetail};
                false -> false
            end;
        % Final checking on tuple value
        ({Key, Value}) ->
        case Value of 
            [] -> false;
            0 -> false;
            <<>> -> false;
            MaybeValid when is_list(MaybeValid) -> 
                Items = filter_payload(MaybeValid),
                HasItems = length(Items) > 0,
                case HasItems of 
                    true -> {true, {Key, Items}};
                    false -> false
                end;
             % Filter address (if the customer details contains either billing/shipping addresses)
            (#address{} = MaybeAddress) -> 
                Address = filter_payload(get_payload(MaybeAddress)),
                HasAddress = length(Address) > 0,
                case HasAddress of 
                    true -> {true,{Key,Address}};
                    false -> false
                end;
            _ -> true
        end
    end, Payload).

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

%% @doc Get the json payload from item type
get_payload(#custom_expiry{} = Payload) ->
  lists:zip(record_info(fields, custom_expiry), tl(tuple_to_list(Payload)));
get_payload(#transaction_details{} = Payload) ->
  lists:zip(record_info(fields, transaction_details), tl(tuple_to_list(Payload)));
get_payload(#item_detail{} = Payload) ->
  lists:zip(record_info(fields, item_detail), tl(tuple_to_list(Payload)));
get_payload(#customer_details{} = Payload) ->
  lists:zip(record_info(fields, customer_details), tl(tuple_to_list(Payload)));
get_payload(#address{} = Payload) ->
  lists:zip(record_info(fields, address), tl(tuple_to_list(Payload)));

%% @doc Get the json payload from driver specific
get_payload(#card{} = Payload) ->
  lists:zip(record_info(fields, card), tl(tuple_to_list(Payload)));
get_payload(#mandiri_clickpay{} = Payload) ->
  lists:zip(record_info(fields, mandiri_clickpay), tl(tuple_to_list(Payload)));
get_payload(#mandiri_echannel{} = Payload) ->
  lists:zip(record_info(fields, mandiri_echannel), tl(tuple_to_list(Payload)));
get_payload(#cimb_clicks{} = Payload) ->
  lists:zip(record_info(fields, cimb_clicks), tl(tuple_to_list(Payload)));
get_payload(#virtual_account{} = Payload) ->
  lists:zip(record_info(fields, virtual_account), tl(tuple_to_list(Payload)));
get_payload(#tcash{} = Payload) ->
  lists:zip(record_info(fields, tcash), tl(tuple_to_list(Payload)));
get_payload(#cstore{} = Payload) ->
  lists:zip(record_info(fields, cstore), tl(tuple_to_list(Payload)));
get_payload(#indosat_dompetku{} = Payload) ->
  lists:zip(record_info(fields, indosat_dompetku), tl(tuple_to_list(Payload)));

%% @doc Get the json payload from data
get_payload(#credit_card_data{} = Payload) ->
  lists:zip(record_info(fields, credit_card_data), 
    [ get_payload(MaybeTuple) || MaybeTuple <- tl(tuple_to_list(Payload))]);
get_payload(#mandiri_clickpay_data{} = Payload) ->
  lists:zip(record_info(fields, mandiri_clickpay_data), 
    [ get_payload(MaybeTuple) || MaybeTuple <- tl(tuple_to_list(Payload))]);
get_payload(#mandiri_echannel_data{} = Payload) ->
  lists:zip(record_info(fields, mandiri_echannel_data), 
    [ get_payload(MaybeTuple) || MaybeTuple <- tl(tuple_to_list(Payload))]);
get_payload(#mandiri_ecash_data{} = Payload) ->
  lists:zip(record_info(fields, mandiri_ecash_data), 
    [ get_payload(MaybeTuple) || MaybeTuple <- tl(tuple_to_list(Payload))]);
get_payload(#cimb_clicks_data{} = Payload) ->
  lists:zip(record_info(fields, cimb_clicks_data), 
    [ get_payload(MaybeTuple) || MaybeTuple <- tl(tuple_to_list(Payload))]);
get_payload(#virtual_account_data{} = Payload) ->
  lists:zip(record_info(fields, virtual_account_data), 
    [ get_payload(MaybeTuple) || MaybeTuple <- tl(tuple_to_list(Payload))]);
get_payload(#bri_epay_data{} = Payload) ->
  lists:zip(record_info(fields, bri_epay_data), 
    [ get_payload(MaybeTuple) || MaybeTuple <- tl(tuple_to_list(Payload))]);
get_payload(#tcash_data{} = Payload) ->
  lists:zip(record_info(fields, tcash_data), 
    [ get_payload(MaybeTuple) || MaybeTuple <- tl(tuple_to_list(Payload))]);
get_payload(#xl_tunai_data{} = Payload) ->
  lists:zip(record_info(fields, xl_tunai_data), 
    [ get_payload(MaybeTuple) || MaybeTuple <- tl(tuple_to_list(Payload))]);
get_payload(#bbm_money_data{} = Payload) ->
  lists:zip(record_info(fields, bbm_money_data), 
    [ get_payload(MaybeTuple) || MaybeTuple <- tl(tuple_to_list(Payload))]);
get_payload(#indomaret_data{} = Payload) ->
  lists:zip(record_info(fields, indomaret_data), 
    [ get_payload(MaybeTuple) || MaybeTuple <- tl(tuple_to_list(Payload))]);
get_payload(#indosat_dompetku_data{} = Payload) ->
  lists:zip(record_info(fields, indosat_dompetku_data), 
    [ get_payload(MaybeTuple) || MaybeTuple <- tl(tuple_to_list(Payload))]);
get_payload(Other) ->
  Other.