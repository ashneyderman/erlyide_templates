${file_comment}

-module(${module}).
-behaviour(application).

-export([start/2, stop/1]).

% public APIs
%--------------------------------------------------------------------

% application callbacks
%--------------------------------------------------------------------

%
% @private
% @doc
% This  function is called whenever an application is started using 
% application:start/1,2, and should start the processes of the
% application. If the application is structured according to the OTP 
% design principles as a supervision tree, this means starting the 
% top supervisor of the tree.
%
% @spec start(StartType, StartArgs) -> {ok, Pid} | 
%                                      {ok, Pid, State} | 
%                                      {error, Reason}
%       StartType = normal | {takeover, Node} | {failover, Node}
%       Node = node()
%       StartArgs = term()
%       Pid = pid()
%       State = term()
% @end
%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    case 'TopSupervisor':start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
		end.

%
% @private
% @doc
% This  function is called whenever an application has stopped. It is
% intended to be the opposite of Module:start/2 and should do any 
% necessary cleaning up. The return value is ignored.
%
% @spec stop(State) -> void()
%       State = term()
% @end
%--------------------------------------------------------------------
stop(_State) ->
    ok.

