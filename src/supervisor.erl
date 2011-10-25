${file_comment}

-module(${module}).
-behaviour(supervisor).

-export([]).
-export([init/1]).

% public APIs
%--------------------------------------------------------------------

% supervisor callback
%--------------------------------------------------------------------

%
% @private
% @doc
% Whenever a supervisor is started using supervisor:start_link/2,3, 
% this function is called by the new process to find out about restart 
% strategy, maximum restart frequency and child specifications.
%
% @spec init(Args) -> {
%                       ok,
%                       {
%                           {RestartStrategy,MaxR,MaxT},
%                           [ChildSpec]
%                       }
%                     } | 
%                     ignore
%       Args = term()
%       RestartStrategy = one_for_all | one_for_one | rest_for_one | simple_one_for_one
%       MaxR = int()
%       MaxT = int()
%       ChildSpec = {Id,StartFunc,Restart,Shutdown,Type,Modules}
%       Id = term()
%       StartFunc = {ChildModule,ChildFunction,ChildArgs}
%       ChildModule = ChildFunction = atom()
%       ChildArgs = [term()]
%       Restart = permanent | transient | temporary
%       Shutdown = brutal_kill | int()>=0 | infinity
%       Type = worker | supervisor
%       Modules = [Module] | dynamic
%       Module = atom()
% @end
%--------------------------------------------------------------------
init(_Args) ->
    {
        ok,
        {
            {one_for_all,0,1},                  % {RestartStrategy,MaxR,MaxT} 
            [                                   % RestartStrategy: one_for_all | one_for_one | rest_for_one | simple_one_for_one
                {                               % start -- ChildSpec
                 'AName',                       % Id: term()
                 {'AModule',start_link,[]},     % StartFunc: {ChildModule,ChildFunction,ChildArgs}
                 permanent,                     % Restart: permanent | transient | temporary
                 2000,                          % Shutdown: brutal_kill | int()>=0 | infinity
                 worker,                        % Type: worker | supervisor
                 ['AModule']                    % Modules: [Module] | dynamic
                }                               % end   -- ChildSpec
            ]
        }
    }.

% private functions
%--------------------------------------------------------------------
