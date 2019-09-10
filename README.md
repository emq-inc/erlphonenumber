# erlphonenumber
Erlang port of libphonenumber

Usage

```
1> application:ensure_all_started(erlphonenumber).
{ok,[erlphonenumber]}
2> erlphonenumber:parse(<<"+886987654321">>).
#{country_code => <<"886">>,is_valid => true,
  national_number => <<"987654321">>,
  raw_input => <<"+886987654321">>,type => mobile}
```
