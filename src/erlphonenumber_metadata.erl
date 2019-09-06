-module(erlphonenumber_metadata).

%% API exports
-export([load/1]).

%%====================================================================
%% API functions
%%====================================================================

% Example outout
% {ok, #{
%    <<"247">> => #{
%        countryCode => <<"247">>,
%        id => <<"AC">>,
%        internationalPrefix => <<"00">>,
%        generalDesc => #{
%          nationalNumberPattern => <<"\n(?:\n[01589]\\d|\n[46]\n)\\d{4}\n">>
%         },
%        fixedLine => #{
%          exampleNumber => <<"62889">>,
%          nationalNumberPattern => <<"6[2-467]\\d{3}">>,
%          possibleLengths => #{national => <<"5">>}
%         },
%        mobile => #{
%          exampleNumber => <<"40123">>,
%          nationalNumberPattern => <<"4\\d{4}">>,
%          possibleLengths => #{national => <<"5">>}
%         },
%        uan => #{
%          exampleNumber => <<"542011">>,
%          nationalNumberPattern => <<"\n(?:\n0[1-9]|\n[1589]\\d\n)\\d{4}\n">>,
%          possibleLengths => #{national => <<"6">>}
%         }
%       }
%   }
% }.
-spec load(string() | binary()) -> {ok, map()}.
load(Path) ->
    atoms(), % Force loading atoms
    {ok, Xml} = file:read_file(Path),
    {ok, Dom, _} = erlsom:simple_form(Xml),
    {ok, parse(Dom)}.

%%====================================================================
%% Internal functions
%%====================================================================

parse({Tag, Attrs, Elements}) ->
    Tag2 = list_to_existing_atom(Tag),
    Elements2 = [{K, [], [V]} || {K, V} <- Attrs] ++ Elements,
    parse2(Tag2, Elements2).

parse2(Tag, [Element]) when Tag == phoneNumberMetadata ->
    parse(Element);
parse2(Tag, Elements) when Tag == territories; Tag == numberFormat ->
    parse3(Elements);
parse2(Tag, Elements) when Tag == territory ->
    Map = parse3(Elements),
    #{ maps:get(countryCode, Map) => Map };
parse2(Tag, Elements) when Tag == availableFormats ->
    #{ Tag => [parse(Element) || Element <- Elements] };
parse2(Tag, [Text]) when Tag == leadingDigits;
                         Tag == nationalNumberPattern;
                         Tag == nationalPrefixForParsing ->
    #{ Tag => re:replace(Text, <<"\s">>, <<>>, [global, {return, binary}]) };
parse2(Tag, [Text]) when Tag == national; Tag == localOnly ->
    {match, Matches} = re:run(Text, "\\[(\\d+)-(\\d+)\\]|,?(\\d+),?",
                              [global, {capture, all_but_first, binary}]),
    lists:flatmap(
      fun([A, B]) -> lists:seq(binary_to_integer(A), binary_to_integer(B));
         ([_, _, N])-> [binary_to_integer(N)]
      end, Matches),
    #{ Tag => [] };
parse2(Tag, [Text]) when is_list(Text) ->
    #{ Tag => list_to_binary(Text) };
parse2(Tag, Elements) ->
    #{ Tag => parse3(Elements) }.

parse3(Elements) ->
    lists:foldl(
      fun(Element, Acc) ->
              maps:merge(Acc, parse(Element))
      end, #{}, Elements).

atoms() ->
    [phoneNumberMetadata,
     territories,
     territory,
     references,
     generalDesc,
     noInternationalDialling,
     fixedLine,
     mobile,
     pager,
     tollFree,
     premiumRate,
     sharedCost,
     personalNumber,
     voip,
     uan,
     voicemail,
     sourceUrl,
     availableFormats,
     nationalNumberPattern,
     exampleNumber,
     numberFormat,
     format,
     intlFormat,
     leadingDigits,
     possibleLengths,
     id,
     countryCode,
     mainCountryForCode,
     leadingDigits,
     preferredInternationalPrefix,
     internationalPrefix,
     nationalPrefix,
     nationalPrefixForParsing,
     nationalPrefixTransformRule,
     preferredExtnPrefix,
     nationalPrefixFormattingRule,
     nationalPrefixOptionalWhenFormatting,
     carrierCodeFormattingRule,
     mobileNumberPortableRegion,
     national,
     localOnly,
     nationalPrefixFormattingRule,
     nationalPrefixOptionalWhenFormatting,
     carrierCodeFormattingRule,
     pattern].
