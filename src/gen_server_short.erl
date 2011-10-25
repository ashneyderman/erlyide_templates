${file_comment}

-module(${module}).
-behaviour(gen_server).

-export([]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% public APIs
%--------------------------------------------------------------------

% gen_server callbacks
%--------------------------------------------------------------------

% init(Args) -> {ok,State} |
%               {ok,State,Timeout} | 
%               {ok,State,hibernate} | 
%               {stop,Reason} | 
%               ignore
init([]) -> {ok, nostate}.

% handle_call(Request, From, State) -> {reply,Reply,NewState} | 
%                                      {reply,Reply,NewState,Timeout} | 
%                                      {reply,Reply,NewState,hibernate} |
%                                      {noreply,NewState} | 
%                                      {noreply,NewState,Timeout} |
%                                      {noreply,NewState,hibernate} |
%                                      {stop,Reason,Reply,NewState} | 
%                                      {stop,Reason,NewState}
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

% handle_cast(Request, State) -> {noreply,NewState} | 
%                                {noreply,NewState,Timeout} |
%                                {noreply,NewState,hibernate} |
%                                {stop,Reason,NewState}
handle_cast(_Request, State) ->
    {noreply, State}.

% handle_info(Info, State) -> {noreply,NewState} | 
%                             {noreply,NewState,Timeout} |
%                             {noreply,NewState,hibernate} |
%                             {stop,Reason,NewState}
handle_info(_Info, State) ->
    {noreply, State}.

% terminate(Reason,State) -> ok
terminate(_Reason, _State) ->
    ok.

% code_change(OldVsn, State, Extra) -> {ok, NewState}
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% private functions
%--------------------------------------------------------------------
