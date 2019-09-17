-module(erlphonenumber).

-export([start_link/0]).
-export([parse/1]).
-export([init/1]).


%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).


-define(DEFAULT_NUMOBJ,
        #{ raw_input => undefined,
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


parse_country(NumObj, [{Country, National}|Rest]) ->
    case ets:lookup(?TABLE, Country) of
        [{Country, Patterns}] ->
            parse_type(NumObj#{ country_code => Country,
                                national_number => National }, Patterns);
        _ ->
            parse_country(NumObj, Rest)
    end;
parse_country(NumObj, _) ->
    NumObj.


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
    % #{ Country => [{Type, RE}] }
    % Example:
    % #{ <<"247">> => [ {fixedLine, <<"^6[2-467]\\d{3}$">>,
    %                   {mobile, <<"^4\\d{4}$">>},
    %                   {uan, <<"^(?:0[1-9]|[1589]\\d)\\d{4}$">>} ] }

    lists:foreach(
      fun({Country, Meta}) ->
              Patterns = compile_patterns(Meta),
              ets:insert(?TABLE, {Country, Patterns})
      end, maps:to_list(Metadata)).


compile_patterns(Meta) ->
    lists:flatmap(
      fun(Type) ->
              case maps:get(Type, Meta, undefined) of
                  #{ nationalNumberPattern := Pattern } ->
                      {ok, MP} = re:compile(<<"^", Pattern/binary, "$">>),
                      [{Type, MP}];
                  _ ->
                      []
              end
      end, ?TYPES).

