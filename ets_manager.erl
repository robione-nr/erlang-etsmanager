-module(ets_manager).
-behaviour(gen_server).

-author("Nolan Robidoux").

-include("ets_manager.hrl").

-export([start_link/1, init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([request_table/1, create_table/2, create_table/3, update_pid/3]).

request_table(TableId) ->
    gen_server:call(?MODULE, {tbl_request, TableId}).

create_table(TableId, TblOpts) ->
    create_table(TableId, TblOpts, []).

create_table(TableId, TblOpts, HeirData) ->
    gen_server:call(?MODULE, {tbl_create, TableId, TblOpts, HeirData}).

update_pid(TableId, Pid, HeirData) ->
    case process_info(Pid, registered_name) of
        {registered_name, ?MODULE} ->
            ets:setopts(TableId, {heir, Pid, HeirData});
        _ ->
            {error, eperm}
    end.

%% ====================================================================
%% Behavioural functions
%% ====================================================================
start_link(Pid) when is_pid(Pid) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Pid, []).


%% ====================================================================
%% Framework Functions
%% ====================================================================

%% init/1
%% ====================================================================
init(FallbackPID) ->
    FallbackPID ! {give_away,{?MASTER_TABLE, self()}},
   {ok, FallbackPID}.

%% handle_call/3
%% ====================================================================
handle_call({tbl_request, TableId}, {Pid, _}, FallbackPID) ->
    Me = self(),

    case ets:lookup(?MASTER_TABLE, TableId) of
    [{TableId, Me, Requestor, HeirData}] ->

        case process_info(FallbackPID, registered_name) of
            {registered_name, Requestor} ->
                ets:give_away(TableId, Pid, HeirData),
                {reply, {ok, TableId},FallbackPID};
            _ ->
                {reply, {error, eaccess}, FallbackPID}
        end;
    [] ->
        {reply, {error, einval}, FallbackPID};
    [{TableId, _, _, _}] ->
        {reply, {error, ebusy}, FallbackPID}
    end;

handle_call({tbl_create, TableId, TblOpts, HeirData}, {Pid, _}, FallbackPID) ->
    Opts = proplists:delete(heir, proplists:delete(named_table, TblOpts)),

    Requestor = 
        case process_info(Pid, registered_name) of
            {registered_name, Module} -> Module;
            _ -> []
        end,
        
    Reply =
        try
            ets:new(TableId,[named_table | [ {heir, self(), HeirData} | Opts]]),
            ets:insert(?MASTER_TABLE, {TableId, Pid, Requestor, HeirData}),
            ets:give_away(TableId, Pid, HeirData)
        catch
            _:_ -> 
                case ets:info(TableId) of
                    undefined -> continue;
                    _ -> ets:delete(TableId)
                end,
                ets:delete(?MASTER_TABLE, TableId),
                {error, ecanceled}
        end,

    {reply, Reply, FallbackPID}.



%% handle_info/2
%% ====================================================================
handle_info({'ETS-TRANSFER', ?MASTER_TABLE, _, _}, FallbackPID) ->
    [{?MODULE, OldPid}] = ets:lookup(?MASTER_TABLE, ?MODULE),
    ets:foldl(fun xfer_state/2,OldPid,?MASTER_TABLE),
    {noreply, FallbackPID};

handle_info({'ETS-TRANSFER', TableId, _, _}, FallbackPID) ->
    ets:update_element(?MASTER_TABLE, TableId,{2, self()}),
    {noreply, FallbackPID}.

%% handle_cast/2
%% ====================================================================
handle_cast(initialize, FallbackPID) ->
    ?MASTER_TABLE = ets:new(?MASTER_TABLE,[named_table, set, private, {heir, FallbackPID, []}]),
    ets:insert(?MASTER_TABLE, {?MODULE, self()}),
    {noreply, FallbackPID}.


%% Placeholders
%% ====================================================================
terminate(_, _) ->
    ok.

code_change(_, FallbackPID, _) ->
    {ok, FallbackPID}.

%% ====================================================================
%% Internal Functions
%% ====================================================================

xfer_state({TableId, OldPid, _, _}, OldPid) ->
    ets:delete(?MASTER_TABLE, TableId), OldPid;
xfer_state({TableId, Pid, _, HeirData}, OldPid) ->
    Pid ! {'ETS-NEWMANAGER', self(), TableId, HeirData}, OldPid;
xfer_state({?MODULE, OldPid}, OldPid) ->
    ets:insert(?MASTER_TABLE, {?MODULE, self()}), OldPid.