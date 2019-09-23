# erlphonenumber
Erlang port of libphonenumber

Usage

```
1> application:ensure_all_started(erlphonenumber).
{ok,[erlphonenumber]}
2> erlphonenumber:parse(<<"+886987654321">>).
[#{country_code => <<"886">>,country_id => <<"TW">>,
   is_valid => true,national_number => <<"987654321">>,
   raw_input => <<"+886987654321">>,type => mobile}]
3> erlphonenumber:is_mobile(<<"+84912345678">>).         
true
4> erlphonenumber:is_mobile(<<"+639051234567">>).
true
5> erlphonenumber:check_type(<<"+62812345678">>, mobile).
true
```
