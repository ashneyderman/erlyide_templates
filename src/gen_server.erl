${file_comment}

-module(${module}).
-behaviour(gen_server).

-export([]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% public APIs
%--------------------------------------------------------------------

% gen_server callbacks
%--------------------------------------------------------------------

%
% @private
% @doc 
% Whenever a gen_server is started using gen_server:start/3,4 or 
% gen_server:start_link/3,4 this function is called by the new
% process to initialize.
%
% @spec init(Args) -> {ok,State} |
%                     {ok,State,Timeout} | 
%                     {ok,State,hibernate} | 
%                     {stop,Reason} | 
%                     ignore
%       Args = term()
%       State = term()
%       Timeout = int() | infinity
%       Reason = term()
% @end
%--------------------------------------------------------------------
init([]) -> {ok, nostate}.

%
% @private
% @doc  
% Whenever a gen_server receives a request sent using gen_server:call/2,3 
% or gen_server:multi_call/2,3,4, this function is called to handle the 
% request.
%
% @spec handle_call(Request, From, State) -> {reply,Reply,NewState} | 
%                                            {reply,Reply,NewState,Timeout} | 
%                                            {reply,Reply,NewState,hibernate} |
%                                            {noreply,NewState} | 
%                                            {noreply,NewState,Timeout} |
%                                            {noreply,NewState,hibernate} |
%                                            {stop,Reason,Reply,NewState} | 
%                                            {stop,Reason,NewState}
%       Request = term()
%       From = {pid(),Tag}
%       State = term()
%       Reply = term()
%       NewState = term()
%       Timeout = int() | infinity
%       Reason = term()
% @end
%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%
% @private
% @doc  
% Whenever a gen_server receives a request sent using gen_server:cast/2 
% or gen_server:abcast/2,3, this function is called to handle the request.
%
% @spec handle_cast(Request, State) -> {noreply,NewState} | 
%                                      {noreply,NewState,Timeout} |
%                                      {noreply,NewState,hibernate} |
%                                      {stop,Reason,NewState}
%       Request = term()
%       State = term()
%       NewState = term()
%       Timeout = int() | infinity
%       Reason = term() 
% @end
%--------------------------------------------------------------------
handle_cast(_Request, State) ->
    {noreply, State}.

%
% @private
% @doc 
% This function is called by a gen_server when a timeout occurs or when 
% it receives any other message than a synchronous or asynchronous 
% request (or a system message).
%
% @spec handle_info(Info, State) -> {noreply,NewState} | 
%                                   {noreply,NewState,Timeout} |
%                                   {noreply,NewState,hibernate} |
%                                   {stop,Reason,NewState}
%       Info = timeout | term()
%       State = term()
%       NewState = term()
%       Timeout = int() | infinity
%       Reason = normal | term()
% @end
%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%
% @private
% @doc 
% This function is called by a gen_server when it is about to terminate. 
% It should be the opposite of Module:init/1 and do any necessary 
% cleaning up. When it returns, the gen_server terminates with Reason. 
% The return value is ignored.
%
% @spec terminate(Reason,State) -> ok
%       Reason =  normal | 
%                 shutdown | 
%                 {shutdown,term()} | 
%                 term()
%       State = term()
% @end
%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%
% @private
% @doc 
% This function is called by a gen_server when it should update its 
% internal state during a release upgrade/downgrade, i.e. when the 
% instruction {update,Module,Change,...} where Change={advanced,Extra} 
% is given in the appup file. See OTP Design Principles for more 
% information.
%
% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%       OldVsn = Vsn | {down, Vsn}
%       Vsn = term()
%       State = term() 
%       NewState = term()
%       Extra = term()
% @end
%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% private functions
%--------------------------------------------------------------------
