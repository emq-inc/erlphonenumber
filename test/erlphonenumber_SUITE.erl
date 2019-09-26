-module(erlphonenumber_SUITE).

-include_lib("common_test/include/ct.hrl").
%-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_parse/1,
         test_premium_rate/1,
         test_toll_free/1,
         test_mobile/1,
         test_is_not_mobile/1,
         test_fixed_line/1,
         test_shared_cost/1,
         test_voip/1,
         test_personal_number/1,
         test_is_valid_number/1,
         test_is_not_valid_number/1,
         test_is_valid_number_for_region/1]).

all() -> [test_parse,
          test_premium_rate,
          test_toll_free,
          test_mobile,
          test_is_not_mobile,
          test_fixed_line,
          test_shared_cost,
          test_voip,
          test_personal_number,
          test_is_valid_number,
          test_is_not_valid_number,
          test_is_valid_number_for_region].


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


test_parse(_) ->
    #{ country_code := <<"886">>,
       national_number := <<"987654321">>,
       raw_input := <<"+886987654321">>
     } = erlphonenumber:parse(<<"+886987654321">>),

    #{ country_code := <<"886">>,
       national_number := <<"12345">>,
       raw_input := <<"+88612345">>
     } = erlphonenumber:parse(<<"+88612345">>),

    #{ country_code := undefined,
       national_number := undefined,
       raw_input := <<"foobar">>
     } = erlphonenumber:parse(<<"foobar">>),

    ok.


test_premium_rate(_Config) ->
    premiumRate = erlphonenumber:number_type(?US_PREMIUM),
    premiumRate = erlphonenumber:number_type(#{ country_code => <<"39">>, national_number => <<"892123">> }),
    premiumRate = erlphonenumber:number_type(<<"+449187654321">>),
    premiumRate = erlphonenumber:number_type(erlphonenumber:parse(<<"+499001654321">>)),
    premiumRate = erlphonenumber:number_type(erlphonenumber:parse(<<"+4990091234567">>)),
    premiumRate = erlphonenumber:number_type(erlphonenumber:parse(?UNIVERSAL_PREMIUM_RATE)),
    ok.


test_toll_free(_Config) ->
    tollFree = erlphonenumber:number_type(<<"+39803123">>),
    tollFree = erlphonenumber:number_type(#{ country_code => <<"49">>, national_number => <<"8001234567">> }),
    tollFree = erlphonenumber:number_type(erlphonenumber:parse(?INTERNATIONAL_TOLL_FREE)),
    ok.


test_mobile(_Config) ->
    mobile = erlphonenumber:number_type(?BS_MOBILE),
    mobile = erlphonenumber:number_type(?GB_MOBILE),
    mobile = erlphonenumber:number_type(?ID_MOBILE, <<"ID">>),
    mobile = erlphonenumber:number_type(?IT_MOBILE, <<"IT">>),
    mobile = erlphonenumber:number_type(erlphonenumber:parse(?PH_MOBILE)),
    mobile = erlphonenumber:number_type(erlphonenumber:parse(?UZ_MOBILE)),
    mobile = erlphonenumber:number_type(erlphonenumber:parse(?VN_MOBILE), <<"VN">>),
    mobile = erlphonenumber:number_type(erlphonenumber:parse(<<"+4915123456789">>)),
    ok.


test_is_not_mobile(_Config) ->
    % data from production
    undefined = erlphonenumber:number_type(<<"+630930763744">>),
    undefined = erlphonenumber:number_type(<<"+630975657611">>, <<"PH">>),
    undefined = erlphonenumber:number_type(<<"+630000000000">>),
    undefined = erlphonenumber:number_type(erlphonenumber:parse(<<"+630900000000">>)),
    undefined = erlphonenumber:number_type(erlphonenumber:parse(<<"+630589265985">>)),
    undefined = erlphonenumber:number_type(erlphonenumber:parse(<<"+630905514044">>), <<"PH">>),
    ok.


test_fixed_line(_Config) ->
    fixedLine = erlphonenumber:number_type(?BS_NUMBER),
    fixedLine = erlphonenumber:number_type(?IT_NUMBER),
    fixedLine = erlphonenumber:number_type(erlphonenumber:parse(?GB_NUMBER)),
    fixedLine = erlphonenumber:number_type(erlphonenumber:parse(?DE_NUMBER)),
    ok.


test_shared_cost(_Config) ->
    sharedCost = erlphonenumber:number_type(<<"+628041234567">>),
    ok.


test_voip(_Config) ->
    voip = erlphonenumber:number_type(<<"+445631231234">>),
    ok.


test_personal_number(_Config) ->
    personalNumber = erlphonenumber:number_type(<<"+447031231234">>),
    ok.


test_is_valid_number(_Config) ->
    true = erlphonenumber:is_valid_number(?US_NUMBER),
    true = erlphonenumber:is_valid_number(?IT_NUMBER),
    true = erlphonenumber:is_valid_number(?GB_MOBILE),
    true = erlphonenumber:is_valid_number(?INTERNATIONAL_TOLL_FREE),
    true = erlphonenumber:is_valid_number(?UNIVERSAL_PREMIUM_RATE),
    true = erlphonenumber:is_valid_number(<<"+6421387835">>),
    ok.


test_is_not_valid_number(_Config) ->
    false = erlphonenumber:is_valid_number(<<"+12530000">>),
    false = erlphonenumber:is_valid_number(<<"+3923661830000">>),
    false = erlphonenumber:is_valid_number(<<"+44791234567">>),
    false = erlphonenumber:is_valid_number(<<"+491234">>),
    false = erlphonenumber:is_valid_number(<<"+643316005">>),
    false = erlphonenumber:is_valid_number(<<"+9991234567">>),
    false = erlphonenumber:is_valid_number(<<"+800123456789">>),
    ok.


test_is_valid_number_for_region(_Config) ->
    % This number is valid for the Bahamas, but is not a valid US number.
    true = erlphonenumber:is_valid_number(?BS_NUMBER, <<"BS">>),
    false = erlphonenumber:is_valid_number(?BS_NUMBER, <<"US">>),

    % This number is no longer valid.
    BSInvalidNumber = #{ country_code => <<"1">>, national_number => <<"2421232345">> },
    false = erlphonenumber:is_valid_number(BSInvalidNumber),

    % La Mayotte and Reunion use 'leadingDigits' to differentiate them.
    RENumber = erlphonenumber:parse(<<"+262262123456">>),
    true = erlphonenumber:is_valid_number(RENumber),
    true = erlphonenumber:is_valid_number(RENumber, <<"RE">>),
    false = erlphonenumber:is_valid_number(RENumber, <<"YT">>),

    % Now change the number to be a number for La Mayotte.
    YTNumber = erlphonenumber:parse(<<"+262269601234">>),
    true = erlphonenumber:is_valid_number(YTNumber, <<"YT">>),
    false = erlphonenumber:is_valid_number(YTNumber, <<"RE">>),

    % This number is no longer valid for La Reunion.
    InvalidRENumber = erlphonenumber:parse(<<"+262269123456">>),
    false = erlphonenumber:is_valid_number(InvalidRENumber, <<"YT">>),
    false = erlphonenumber:is_valid_number(InvalidRENumber, <<"RE">>),
    false = erlphonenumber:is_valid_number(InvalidRENumber),

    % This number is valid in both places.
    BothValidNumber = erlphonenumber:parse(<<"+262800123456">>),
    true = erlphonenumber:is_valid_number(BothValidNumber, <<"YT">>),
    true = erlphonenumber:is_valid_number(BothValidNumber, <<"RE">>),

    true = erlphonenumber:is_valid_number(?INTERNATIONAL_TOLL_FREE, <<"001">>),
    false = erlphonenumber:is_valid_number(?INTERNATIONAL_TOLL_FREE, <<"US">>),

    % Invalid country calling codes.
    Invalid = #{ country_code => <<"3923">>, national_number => <<"2366">> },
    false = erlphonenumber:is_valid_number(Invalid, <<"ZZ">>),
    false = erlphonenumber:is_valid_number(Invalid, <<"001">>),
    Invalid2 = #{ country_code => <<"0">>, national_number => <<"2366">> },
    false = erlphonenumber:is_valid_number(Invalid2, <<"001">>),
    false = erlphonenumber:is_valid_number(Invalid2, <<"ZZ">>),
    ok.
