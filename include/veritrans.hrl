-record(card, {
	token_id = <<>> :: binary(),
	bank = <<>> :: binary()
}).

-record(mandiri_clickpay, {
	card_number = <<>> :: binary(),
	input1 = <<>> :: binary(),
	input2 = <<>> :: binary(),
	input3 = <<>> :: binary(),
	token = <<>> :: binary()
}).

-record(mandiri_echannel, {
	bill_info1 = <<"Payment via Veritrans.">> :: binary(),
	bill_info2 = <<"debt">> :: binary()
}).

-record(cimb_clicks, {
	description = <<"Payment via Veritrans.">> :: binary()
}).

-record(virtual_account, {
	bank = <<>> :: binary()
}).

-record(tcash, {
	customer = <<>> :: binary(),
	promo = <<"false">> :: binary(),
	is_reversal = <<"0">> :: binary()
}).

-record(cstore, {
	store = <<"Indomaret">> :: binary(),
	message = <<"Payment via Veritrans.">> :: binary()
}).

-record(indosat_dompetku, {
	msisdn = <<>> :: binary()
}).

-record(credit_card_data, {
	payment_type = <<"credit_card">> :: binary(),
	credit_card = #card{} :: card(),
	transaction_details = [] :: list(),
	item_details = [] :: list(),
	customer_details = [] :: list()
}).

-record(mandiri_clickpay_data, {
	payment_type = <<"mandiri_clickpay">> :: binary(),
	mandiri_clickpay = #mandiri_clickpay{} :: mandiri_clickpay(),
	transaction_details = [] :: list(),
	customer_details = [] :: list()
}).

-record(mandiri_echannel_data, {
	payment_type = <<"echannel">> :: binary(),
	echannel = #mandiri_echannel{} :: mandiri_echannel(),
	transaction_details = [] :: list(),
	item_details = [] :: list(),
	customer_details = [] :: list()
}).

-record(mandiri_ecash_data, {
	payment_type = <<"mandiri_ecash">> :: binary(),
	transaction_details = [] :: list(),
	customer_details = [] :: list()
}).

-record(cimb_clicks_data, {
	payment_type = <<"cimb_clicks">> :: binary(),
	cimb_clicks = #cimb_clicks{} :: cimb_clicks(),
	transaction_details = [] :: list(),
	customer_details = [] :: list()
}).

-record(virtual_account_data, {
	payment_type = <<"bank_transfer">> :: binary(),
	bank_transfer = #virtual_account{} :: virtual_account(),
	transaction_details = [] :: list(),
	customer_details = [] :: list()
}).

-record(bri_epay_data, {
	payment_type = <<"bri_epay">> :: binary(),
	transaction_details = [] :: list(),
	customer_details = [] :: list()
}).

-record(tcash_data, {
	payment_type = <<"telkomsel_cash">> :: binary(),
	telkomsel_cash = #tcash{} :: tcash(),
	transaction_details = [] :: list(),
	customer_details = [] :: list()
}).

-record(xl_tunai_data, {
	payment_type = <<"xl_tunai">> :: binary(),
	transaction_details = [] :: list(),
	customer_details = [] :: list()
}).

-record(bbm_money_data, {
	payment_type = <<"bbm_money">> :: binary(),
	transaction_details = [] :: list(),
	customer_details = [] :: list()
}).

-record(indomaret_data, {
	payment_type = <<"cstore">> :: binary(),
	cstore = #cstore{} :: cstore(),
	transaction_details = [] :: list(),
	customer_details = [] :: list()
}).

-record(indosat_dompetku_data, {
	payment_type = <<"indosat_dompetku">> :: binary(),
	indosat_dompetku = #indosat_dompetku{} :: indosat_dompetku(),
	transaction_details = [] :: list(),
	customer_details = [] :: list()
}).

-type card() :: #card{}.
-type mandiri_clickpay() :: #mandiri_clickpay{}.
-type mandiri_echannel() :: #mandiri_echannel{}.
-type cimb_clicks() :: #cimb_clicks{}.
-type virtual_account() :: #virtual_account{}.
-type tcash() :: #tcash{}.
-type cstore() :: #cstore{}.
-type indosat_dompetku() :: #indosat_dompetku{}.
-type credit_card_data() :: #credit_card_data{}.
-type mandiri_clickpay_data() :: #mandiri_clickpay_data{}.
-type mandiri_echannel_data() :: #mandiri_echannel_data{}.
-type mandiri_ecash_data() :: #mandiri_ecash_data{}.
-type cimb_clicks_data() :: #cimb_clicks_data{}.
-type virtual_account_data() :: #virtual_account_data{}.
-type bri_epay_data() :: #bri_epay_data{}.
-type tcash_data() :: #tcash_data{}.
-type xl_tunai_data() :: #xl_tunai_data{}.
-type bbm_money_data() :: #bbm_money_data{}.
-type indomaret_data() :: #indomaret_data{}.
-type indosat_dompetku_data() :: #indosat_dompetku_data{}.

-export_type([
	card/0
	,mandiri_clickpay/0
	,mandiri_echannel/0
	,cimb_clicks/0
	,virtual_account/0
	,tcash/0
	,cstore/0
	,indosat_dompetku/0
	,credit_card_data/0
	,mandiri_clickpay_data/0
	,mandiri_echannel_data/0
	,mandiri_ecash_data/0
	,cimb_clicks_data/0
	,virtual_account_data/0
	,bri_epay_data/0
	,tcash_data/0
	,xl_tunai_data/0
	,bbm_money_data/0
	,indomaret_data/0
	,indosat_dompetku_data/0
]).