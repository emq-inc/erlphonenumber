# erlphonenumber
Erlang port of libphonenumber

Usage

```
1> application:ensure_all_started(erlphonenumber).
{ok,[erlphonenumber]}
2> erlphonenumber:parse(<<"+886987654321">>).
#{country_code => <<"886">>,
  national_number => <<"987654321">>,
  raw_input => <<"+886987654321">>}
3> erlphonenumber:number_type(<<"+84912345678">>).         
mobile
4> erlphonenumber:number_type(<<"+84912345678">>, <<"VN">>).
mobile
5> erlphonenumber:is_valid_number(<<"+62812345678">>).
true
6> erlphonenumber:is_valid_number(<<"+62812345678">>, <<"ID">>).
true
7> erlphonenumber:is_valid_number(<<"+62812345678">>, <<"US">>).
false
```
