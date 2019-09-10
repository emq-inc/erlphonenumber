-module(erlphonenumber).

%% API exports
-export([parse/1]).

%%====================================================================
%% API functions
%%====================================================================

parse(RawInput) ->
    NumObj = #{ raw_input => RawInput,
                country_code => undefined,
                national_number => undefined,
                is_valid => false,
                type => undefined },
    parse_country(NumObj, [split_country(RawInput, N) || N <- [1, 2, 3]]).


split_country(<<"+", Bin/binary>>, N) ->
    case lists:split(N, binary_to_list(Bin)) of
        {List1, List2} -> {list_to_binary(List1), list_to_binary(List2)};
        _ -> undefined
    end;
split_country(_, _) ->
    undefined.


parse_country(NumObj, [{Country, National}|Rest]) ->
    {ok, AllPatterns} = application:get_env(erlphonenumber, patterns),
    case AllPatterns of
        #{ Country := Patterns } ->
            parse_type(NumObj#{ country_code => Country,
                                national_number => National }, Patterns);
        _ ->
            parse_country(NumObj, Rest)
    end;
parse_country(NumObj, _) ->
    NumObj.


parse_type(#{ national_number := National }=NumObj, [{Type, Pattern}|Rest]) ->
    case re:run(National, Pattern) of
        match -> NumObj#{ is_valid => true, type => Type };
        {match, _} -> NumObj#{ is_valid => true, type => Type };
        nomatch -> parse_type(NumObj, Rest)
    end;
parse_type(NumObj, []) ->
    NumObj.


%%====================================================================
%% Internal functions
%%====================================================================
