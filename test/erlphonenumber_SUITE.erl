-module(erlphonenumber_SUITE).

-include_lib("common_test/include/ct.hrl").
%-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_phonenumbers/1,
         test_premium_rate/1,
         test_toll_free/1,
         test_mobile/1,
         test_is_not_mobile/1,
         test_fixed_line/1,
         test_shared_cost/1,
         test_voip/1,
         test_personal_number/1,
         test_is_valid/1,
         test_is_not_valid/1]).

all() -> [test_phonenumbers,
          test_premium_rate,
          test_toll_free,
          test_mobile,
          test_is_not_mobile,
          test_fixed_line,
          test_shared_cost,
          test_voip,
          test_personal_number,
          test_is_valid,
          test_is_not_valid].


init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erlphonenumber),
    Config.


end_per_suite(_Config) ->
    ok.


-define(BS_MOBILE, <<"+12423570000">>).
-define(GB_MOBILE, <<"+447912345678">>).
-define(ID_MOBILE, <<"+62812345678">>).
-define(IT_MOBILE, <<"+39345678901">>).
-define(PH_MOBILE, <<"+639051234567">>).
-define(UZ_MOBILE, <<"+998950123456">>).
-define(VN_MOBILE, <<"+84912345678">>).
-define(BS_NUMBER, <<"+12423651234">>).
-define(DE_NUMBER, <<"+4930123456">>).
-define(GB_NUMBER, <<"+442070313000">>).
-define(IT_NUMBER, <<"+390236618300">>).
-define(US_NUMBER, <<"+16502530000">>).
-define(US_PREMIUM, <<"+19002530000">>).
-define(UNIVERSAL_PREMIUM_RATE, <<"+979123456789">>).
-define(INTERNATIONAL_TOLL_FREE, <<"+80012345678">>).


test_phonenumbers(_) ->
    [#{ country_code := <<"886">>,
        country_id := <<"TW">>,
        is_valid := true,
        national_number := <<"987654321">>,
        raw_input := <<"+886987654321">>,
        type := mobile
      }] = erlphonenumber:parse(<<"+886987654321">>),

    [#{ country_code := <<"886">>,
        country_id := <<"TW">>,
        is_valid := false,
        national_number := <<"12345">>,
        raw_input := <<"+88612345">>,
        type := undefined
      }] = erlphonenumber:parse(<<"+88612345">>),

    [#{ country_code := undefined,
        country_id := undefined,
        is_valid := false,
        national_number := undefined,
        raw_input := <<"foobar">>,
        type := undefined
      }] = erlphonenumber:parse(<<"foobar">>),

    ok.


test_premium_rate(_Config) ->
    true = erlphonenumber:is_premium_rate(?US_PREMIUM),
    true = erlphonenumber:is_premium_rate(<<"+39892123">>),
    true = erlphonenumber:is_premium_rate(<<"+449187654321">>),
    true = erlphonenumber:is_premium_rate(<<"+499001654321">>),
    true = erlphonenumber:is_premium_rate(<<"+4990091234567">>),
    true = erlphonenumber:is_premium_rate(?UNIVERSAL_PREMIUM_RATE),
    ok.


test_toll_free(_Config) ->
    true = erlphonenumber:is_toll_free(<<"+39803123">>),
    true = erlphonenumber:is_toll_free(<<"+498001234567">>),
    true = erlphonenumber:is_toll_free(?INTERNATIONAL_TOLL_FREE),
    ok.


test_mobile(_Config) ->
    true = erlphonenumber:is_mobile(?BS_MOBILE),
    true = erlphonenumber:is_mobile(?GB_MOBILE),
    true = erlphonenumber:is_mobile(?ID_MOBILE),
    true = erlphonenumber:is_mobile(?IT_MOBILE),
    true = erlphonenumber:is_mobile(?PH_MOBILE),
    true = erlphonenumber:is_mobile(?UZ_MOBILE),
    true = erlphonenumber:is_mobile(?VN_MOBILE),
    true = erlphonenumber:is_mobile(<<"+4915123456789">>),
    ok.


test_is_not_mobile(_Config) ->
    % data from production
    false = erlphonenumber:is_mobile(<<"+630930763744">>),
    false = erlphonenumber:is_mobile(<<"+630975657611">>),
    false = erlphonenumber:is_mobile(<<"+630000000000">>),
    false = erlphonenumber:is_mobile(<<"+630900000000">>),
    false = erlphonenumber:is_mobile(<<"+630589265985">>),
    false = erlphonenumber:is_mobile(<<"+630905514044">>),
    ok.


test_fixed_line(_Config) ->
    true = erlphonenumber:is_fixed_line(?BS_NUMBER),
    true = erlphonenumber:is_fixed_line(?IT_NUMBER),
    true = erlphonenumber:is_fixed_line(?GB_NUMBER),
    true = erlphonenumber:is_fixed_line(?DE_NUMBER),
    ok.


test_shared_cost(_Config) ->
    true = erlphonenumber:is_shared_cost(<<"+628041234567">>),
    ok.


test_voip(_Config) ->
    true = erlphonenumber:is_voip(<<"+445631231234">>),
    ok.


test_personal_number(_Config) ->
    true = erlphonenumber:is_personal_number(<<"+447031231234">>),
    ok.


test_is_valid(_Config) ->
    true = erlphonenumber:is_valid(?US_NUMBER),
    true = erlphonenumber:is_valid(?IT_NUMBER),
    true = erlphonenumber:is_valid(?GB_MOBILE),
    true = erlphonenumber:is_valid(?INTERNATIONAL_TOLL_FREE),
    true = erlphonenumber:is_valid(?UNIVERSAL_PREMIUM_RATE),
    true = erlphonenumber:is_valid(<<"+6421387835">>),
    ok.


test_is_not_valid(_Config) ->
    false = erlphonenumber:is_valid(<<"+12530000">>),
    false = erlphonenumber:is_valid(<<"+3923661830000">>),
    false = erlphonenumber:is_valid(<<"+44791234567">>),
    false = erlphonenumber:is_valid(<<"+491234">>),
    false = erlphonenumber:is_valid(<<"+643316005">>),
    false = erlphonenumber:is_valid(<<"+9991234567">>),
    false = erlphonenumber:is_valid(<<"+800123456789">>),
    ok.