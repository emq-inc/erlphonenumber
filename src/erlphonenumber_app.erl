-module(erlphonenumber_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    load_metadata(),
    erlphonenumber_sup:start_link().


stop(_State) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

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
    application:set_env(?APP, metadata, Metadata),

    % Transform to simplified format for parsing
    % #{ <<"247">> => [ {fixedLine, <<"^6[2-467]\\d{3}$">>,
    %                    {mobile, <<"^4\\d{4}$">>},
    %                    {uan, <<"^(?:0[1-9]|[1589]\\d)\\d{4}$">>} ] }
    Patterns = maps:map(
                 fun(_Country, Meta) ->
                         lists:flatmap(
                           fun(Type) ->
                                   case maps:get(Type, Meta, undefined) of
                                       #{ nationalNumberPattern := Pattern } ->
                                           {ok, MP} = re:compile(<<"^", Pattern/binary, "$">>),
                                           [{Type, MP}];
                                       _ -> []
                                   end
                           end, ?TYPES)
                 end, Metadata),
    application:set_env(?APP, patterns, Patterns),

    ok.

