-module(erlphonenumber).

-include("erlphonenumber.hrl").

-export([start_link/0]).
-export([parse/1,
         check_type/2,
         is_valid/1,
         is_premium_rate/1,
         is_toll_free/1,
         is_mobile/1,
         is_fixed_line/1,
         is_shared_cost/1,
         is_voip/1,
         is_personal_number/1]).
-export([init/1]).


%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).


-define(DEFAULT_NUMOBJ,
        #{ raw_input => undefined,
           country_id => undefined,
           country_code => undefined,
           national_number => undefined,
           is_valid => false,
           type => undefined }).

parse(RawInput) when is_binary(RawInput) ->
    NumObj = maps:merge(?DEFAULT_NUMOBJ, #{ raw_input => RawInput }),
    parse_country(NumObj, [split_country(RawInput, N) || N <- [1, 2, 3]]);
parse(#{ country_code := Country, national_number := National }=NumObj0) ->
    NumObj = maps:merge(?DEFAULT_NUMOBJ, NumObj0),
    parse_country(NumObj, [{Country, National}]).


check_type(PhoneNumber, Type) ->
    NumObjList = parse(PhoneNumber),
    lists:any(
        fun(NumObj) ->
            case NumObj of
                #{ type := Type } -> true;
                _ -> false
            end
        end, NumObjList).


is_valid(PhoneNumber) ->
    NumObjList = parse(PhoneNumber),
    lists:any(
        fun(NumObj) ->
            case NumObj of
                #{ is_valid := true } -> true;
                _ -> false
            end
        end, NumObjList).


is_premium_rate(PhoneNumber) ->
    check_type(PhoneNumber, premiumRate).


is_toll_free(PhoneNumber) ->
    check_type(PhoneNumber, tollFree).


is_mobile(PhoneNumber) ->
    check_type(PhoneNumber, mobile).


is_fixed_line(PhoneNumber) ->
    check_type(PhoneNumber, fixedLine).


is_shared_cost(PhoneNumber) ->
    check_type(PhoneNumber, sharedCost).


is_voip(PhoneNumber) ->
    check_type(PhoneNumber, voip).


is_personal_number(PhoneNumber) ->
    check_type(PhoneNumber, personalNumber).


%%====================================================================
%% gen_server callbacks
%%====================================================================

-define(TABLE, patterns).

init([]) ->
    Table = ets:new(?TABLE, [set, named_table]),
    load_metadata(),
    {ok, #{ table => Table }}.


%%====================================================================
%% Internal functions
%%====================================================================

split_country(<<"+", Bin/binary>>, N) ->
    case lists:split(N, binary_to_list(Bin)) of
        {List1, List2} -> {list_to_binary(List1), list_to_binary(List2)};
        _ -> undefined
    end;
split_country(_, _) ->
    undefined.


parse_country(NumObj, [{CountryCode, National}|Rest]) ->
    CountryIDList = maps:get(CountryCode, ?COUNTRY_CODE_TO_COUNTRY_ID, []),
    case CountryIDList of
        [] ->
            parse_country(NumObj, Rest);
        _ ->
            lists:map(
                fun(CountryID) ->
                    case ets:lookup(?TABLE, {CountryCode, CountryID}) of
                        [{{CountryCode, CountryID}, Patterns}] ->
                            parse_type(NumObj#{ country_code => CountryCode,
                                                country_id => CountryID,
                                                national_number => National }, Patterns);
                        _ ->
                            parse_country(NumObj, Rest)
                    end
                end, CountryIDList)
    end;
parse_country(NumObj, _) ->
    [NumObj].


parse_type(#{ national_number := National }=NumObj, [{Type, Pattern}|Rest]) ->
    case re:run(National, Pattern, [{capture, none}]) of
        match -> NumObj#{ is_valid => true, type => Type };
        nomatch -> parse_type(NumObj, Rest)
    end;
parse_type(NumObj, []) ->
    NumObj.


-define(APP, erlphonenumber).
-define(METADATA_FILENAME, "PhoneNumberMetadata.xml").
-define(TYPES, [noInternationalDialling,
                fixedLine,
                mobile,
                pager,
                tollFree,
                premiumRate,
                sharedCost,
                personalNumber,
                voip,
                uan,
                voicemail]).

load_metadata() ->
    PrivDir = code:priv_dir(?APP),
    MetadataPath = filename:join(PrivDir, ?METADATA_FILENAME),
    {ok, Metadata} = erlphonenumber_metadata:load(MetadataPath),

    % Transform to simplified format for parsing
    % #{ {CountryCode, CountryID} => [{Type, RE}] }
    % Example:
    % #{ {<<"247">>, <<"AC">>} => [ {fixedLine, <<"^6[2-467]\\d{3}$">>,
    %                               {mobile, <<"^4\\d{4}$">>},
    %                               {uan, <<"^(?:0[1-9]|[1589]\\d)\\d{4}$">>} ] }

    lists:foreach(
      fun({{CountryCode, CountryID}, Meta}) ->
              Patterns = compile_patterns(Meta),
              ets:insert(?TABLE, {{CountryCode, CountryID}, Patterns})
      end, maps:to_list(Metadata)).


compile_patterns(Meta) ->
    lists:flatmap(
      fun(Type) ->
              case maps:get(Type, Meta, undefined) of
                  #{ nationalNumberPattern := Pattern } ->
                      {ok, MP} = re:compile(<<"^(?:", Pattern/binary, ")$">>),
                      [{Type, MP}];
                  _ ->
                      []
              end
      end, ?TYPES).

