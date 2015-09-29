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

## LICENSE

See [LICENSE file](https://github.com/toopay/veritrans-erlang/blob/master/LICENSE)