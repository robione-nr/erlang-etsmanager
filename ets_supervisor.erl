-module(ets_supervisor).
-behaviour(supervisor).

-author("Nolan Robidoux").

-include("ets_manager.hrl").

-export([start_link/1, init/1]).

%% ====================================================================
%% API/Exposed functions
%% ====================================================================
-export([spawn_init/1]).

spawn_init(SupRef) ->
    register(ets_fallback, self()),
    monitor(process, SupRef),
    loop().

%% ====================================================================
%% Internal functions
%% ====================================================================

loop() ->
    receive
        {give_away,{?MASTER_TABLE, Pid}} ->
            try
                {registered_name, ets_manager} = erlang:process_info(Pid, registered_name),
                ets:give_away(?MASTER_TABLE, Pid, [])
            catch
                error:{badmatch, _} ->  logger:error("Illegal process (~p) attempting ETS Manager table ownership.",[Pid]),
                                        {error, badmatch};
                error:badarg -> gen_server:cast(ets_manager, initialize);
                Type:Reason -> logger:error("Unhandled catch -> ~p : ~p",[Type, Reason]),
                                {Type, Reason}
            end;
        {'DOWN',_,_,_,_} ->
            case proplists:get_value(owner,ets:info(?MASTER_TABLE)) =:= self() of
                true -> ets:delete(?MASTER_TABLE);
                false -> continue
            end,
            exit(shutdown);
        _ -> continue
    end,
    loop().


%% ====================================================================
%% Behavioural functions
%% ====================================================================

start_link([]) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Pid = spawn(?MODULE, spawn_init, [self()]),

	{ok, { #{}, [

        % === ETS Manager: gen_server to not lose table data
		#{	id => ets_manager,
			start => {ets_manager, start_link, [Pid]}
            %% restart =>           #Default: permanent
            %% shutdown => n	    %Default: 5000 | infinity
			%% type => worker,		%DEFAULT
			%% modules => [...]		%Default: [M]	
		}
	]}}.

%sup_flags() = #{strategy => strategy(),        % optional (one_for_one)
%              intensity => non_neg_integer(),  % optional (1) 
%              period => pos_integer()}         % optional (5)