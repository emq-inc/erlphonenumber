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

load_metadata() ->
    PrivDir = code:priv_dir(?APP),
    MetadataPath = filename:join(PrivDir, ?METADATA_FILENAME),
    {ok, Metadata} = erlphonenumber_metadata:load(MetadataPath),
    application:set_env(?APP, metadata, Metadata),
    ok.

