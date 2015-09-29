# Veritrans-Erlang
Erlang client library for Veritrans (VT-Direct).

## Usage

On your application file, start the library :

```erlang
veritrans:start(),
veritrans:set_key(<<"Your-Veritrans-Key">>),
veritrans:set_flag(sandbox). % Or `live` atom
```

Now we can perform any Veritrans VT-Direct operations.

## Datatypes and API Reference

### Vendor Specific Charges
 Method  | Spec 
---------|------
`charge_credit_card/5` | `Token :: binary()`, `Bank :: binary()` , OrderId :: binary(), GrossAmount :: integer(), ItemDetails :: list()
charge_credit_card/6 | Token :: binary(), Bank :: binary(), OrderId :: binary(), GrossAmount :: integer(), ItemDetails :: list(), CustomerDetails :: customer_details()
charge_mandiri_clickpay/7 | CardNumber :: binary(), Input1 :: binary(), Input2 :: binary(), Input3 :: binary(), Token :: binary(), OrderId :: binary(), GrossAmount :: integer()
charge_mandiri_clickpay/8 | CardNumber :: binary(), Input1 :: binary(), Input2 :: binary(), Input3 :: binary(), Token :: binary(), OrderId :: binary(), GrossAmount :: integer(), CustomerDetails :: customer_details()
charge_mandiri_echannel/5 | BillInfo1 :: binary(), BillInfo2 :: binary(), OrderId :: binary(), GrossAmount :: integer(), ItemDetails :: list()
charge_mandiri_echannel/6 | BillInfo1 :: binary(), BillInfo2 :: binary(), OrderId :: binary(), GrossAmount :: integer(), ItemDetails :: list(), CustomerDetails :: customer_details()
charge_mandiri_ecash/6 | OrderId :: binary(), GrossAmount :: integer(), FirstName :: binary(), LastName :: binary(), Email :: binary(), Phone :: binary()
charge_cimb_clicks/7 | Description :: binary(), OrderId :: binary(), GrossAmount :: integer(), FirstName :: binary(), LastName :: binary(), Email :: binary(), Phone :: binary()
charge_virtual_account/7 | BankName :: binary(), OrderId :: binary(), GrossAmount :: integer(), FirstName :: binary(), LastName :: binary(), Email :: binary(), Phone :: binary()
charge_bri_epay/6 | OrderId :: binary(), GrossAmount :: integer(), FirstName :: binary(), LastName :: binary(), Email :: binary(), Phone :: binary()
charge_tcash/7 | TokenNumber :: binary(), OrderId :: binary(), GrossAmount :: integer(), FirstName :: binary(), LastName :: binary(), Email :: binary(), Phone :: binary()
charge_xl_tunai/6 | OrderId :: binary(), GrossAmount :: integer(), FirstName :: binary(), LastName :: binary(), Email :: binary(), Phone :: binary()
charge_bbm_money/6 | OrderId :: binary(), GrossAmount :: integer(), FirstName :: binary(), LastName :: binary(), Email :: binary(), Phone :: binary()
charge_cstore/8 | Store :: binary(), Message :: binary(), OrderId :: binary(), GrossAmount :: integer(), FirstName :: binary(), LastName :: binary(), Email :: binary(), Phone :: binary()
charge_indosat_dompetku/7 | MSISDN :: binary(), OrderId :: binary(), GrossAmount :: integer(), FirstName :: binary(), LastName :: binary(), Email :: binary(), Phone :: binary()

## LICENSE

See [LICENSE file](https://github.com/toopay/veritrans-erlang/blob/master/LICENSE)