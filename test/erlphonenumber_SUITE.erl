-module(erlphonenumber_SUITE).

-include_lib("common_test/include/ct.hrl").
%-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_phonenumbers/1]).


all() -> [test_phonenumbers].


init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erlphonenumber),
    Config.


end_per_suite(_Config) ->
    ok.


test_phonenumbers(_) ->
    #{ country_code := <<"886">>,
       is_valid := true,
       national_number := <<"987654321">>,
       raw_input := <<"+886987654321">>,
       type := mobile
     } = erlphonenumber:parse(<<"+886987654321">>),

    #{ country_code := <<"886">>,
       is_valid := false,
       national_number := <<"12345">>,
      raw_input := <<"+88612345">>,
      type := undefined
     } = erlphonenumber:parse(<<"+88612345">>),

    #{ country_code := undefined,
       is_valid := false,
       national_number := undefined,
       raw_input := <<"foobar">>,
       type := undefined
     } = erlphonenumber:parse(<<"foobar">>),

    ok.

