-module(erlphonenumber).

-export([start_link/0]).
-export([parse/1,
         number_type/1,
         number_type/2,
         is_valid_number/1,
         is_valid_number/2]).
-export([init/1]).


%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).


-define(DEFAULT_NUMOBJ,
        #{ raw_input => undefined,
           country_code => undefined,
           national_number => undefined }).

parse(RawInput) when is_binary(RawInput) ->
    Result = extract_country_code(RawInput),
    Result#{ raw_input => RawInput }.


%% number_type is simply porting from python libphonenumber
%% python verion has more strictly checking criteria
%% https://github.com/daviddrysdale/python-phonenumbers/blob/dev/python/phonenumbers/phonenumberutil.py#L1892
number_type(PhoneNumber) when is_binary(PhoneNumber) ->
    NumObj = parse(PhoneNumber),
    number_type(NumObj);
number_type(NumObj) when is_map(NumObj) ->
    NumObjList = match_number(NumObj),
    number_type(NumObjList);
number_type(NumObjList) ->
    ValidList = lists:filter(
                    fun(NumObj) ->
                        case NumObj of
                            #{ is_valid := true } -> true;
                            _ -> false
                        end
                    end, NumObjList),
    case ValidList of
        [] -> undefined;
        _ -> maps:get(type, hd(ValidList))
    end.


number_type(PhoneNumber, Region) when is_binary(PhoneNumber) ->
    NumObj = parse(PhoneNumber),
    number_type(NumObj, Region);
number_type(NumObj, Region) when is_map(NumObj) ->
    case match_number(NumObj, Region) of
        [#{ is_valid := true, type := Type }] -> Type;
        _ -> undefined
    end.


is_valid_number(PhoneNumber) when is_binary(PhoneNumber) ->
    NumObj = parse(PhoneNumber),
    is_valid_number(NumObj);
is_valid_number(NumObj) when is_map(NumObj) ->
    NumObjList = match_number(NumObj),
    is_valid_number(NumObjList);
is_valid_number(NumObjList) ->
    lists:any(
        fun(NumObj) ->
            case NumObj of
                #{ is_valid := true } -> true;
                _ -> false
            end
        end, NumObjList).


is_valid_number(PhoneNumber, Region) when is_binary(PhoneNumber) ->
    NumObj = parse(PhoneNumber),
    is_valid_number(NumObj, Region);
is_valid_number(NumObj, Region) ->
    case match_number(NumObj, Region) of
        [#{ is_valid := true }] -> true;
        _ -> false
    end.


%%====================================================================
%% gen_server callbacks
%%====================================================================

-define(TABLE, patterns).
-define(REGIONS_BAG, regions).

init([]) ->
    Table = ets:new(?TABLE, [set, named_table]),
    Regions = ets:new(?REGIONS_BAG, [bag, named_table]),
    load_metadata(),
    {ok, #{ table => Table, regions => Regions }}.


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


extract_country_code(RawInput) ->
    PossibleCountry = [split_country(RawInput, N) || N <- [1, 2, 3]],
    lists:foldl(
        fun({CountryCode, National}, NumObj) ->
                case ets:lookup(?REGIONS_BAG, CountryCode) of
                    [] -> NumObj;
                    _ -> NumObj#{ country_code => CountryCode, national_number => National }
                end;
           (undefined, NumObj) ->
                NumObj
        end, ?DEFAULT_NUMOBJ, PossibleCountry).


match_number(#{ country_code := CountryCode } = NumObj) ->
    RegionList = ets:lookup(?REGIONS_BAG, CountryCode),
    case RegionList of
        [] ->
            [NumObj];
        _ ->
            lists:flatmap(
                fun({_CountryCode, Region}) ->
                    match_number(NumObj, Region)
                end, RegionList)
    end.


match_number(#{ country_code := CountryCode } = NumObj, Region) ->
    case ets:lookup(?TABLE, {CountryCode, Region}) of
        [{{CountryCode, Region}, Patterns}] ->
            [parse_type(NumObj#{ region => Region }, Patterns)];
        _ ->
            [NumObj#{ region => Region }]
    end.


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
    % #{ {CountryCode, Region} => [{Type, RE}] }
    % Example:
    % #{ {<<"247">>, <<"AC">>} => [ {fixedLine, <<"^6[2-467]\\d{3}$">>,
    %                               {mobile, <<"^4\\d{4}$">>},
    %                               {uan, <<"^(?:0[1-9]|[1589]\\d)\\d{4}$">>} ] }

    lists:foreach(
      fun({{CountryCode, Region}, Meta}) ->
              Patterns = compile_patterns(Meta),
              ets:insert(?TABLE, {{CountryCode, Region}, Patterns}),
              ets:insert(?REGIONS_BAG, {CountryCode, Region})
      end, maps:to_list(Metadata)).


compile_patterns(Meta) ->
    lists:flatmap(
      fun(Type) ->
              case maps:get(Type, Meta, undefined) of
                  #{ nationalNumberPattern := Pattern } ->
                      % Build a version of the pattern with a non-capturing group around it.
                      % https://github.com/daviddrysdale/python-phonenumbers/blob/dev/python/phonenumbers/re_util.py
                      % Function fullmatch.
                      {ok, MP} = re:compile(<<"^(?:", Pattern/binary, ")$">>),
                      [{Type, MP}];
                  _ ->
                      []
              end
      end, ?TYPES).

