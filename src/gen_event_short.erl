${file_comment}

-module(${module}).
-behaviour(gen_event).

-export([]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

% public APIs
%--------------------------------------------------------------------

% gen_event callbacks
%--------------------------------------------------------------------

% init(InitArgs) -> {ok,State} | 
%                   {ok,State,hibernate}
init({_Args,_Term}) -> {ok, {}};
init(_Args) -> {ok, {}}.

% handle_event(Event, State) -> {ok,NewState} | 
%                               {ok,NewState,hibernate} |
%                               {swap_handler,Args1,NewState,Handler2,Args2} |
%                               remove_handler
handle_event(_Request, State) ->
    {ok, State}.

% handle_call(Request, State) -> {ok,Reply,NewState} | 
%                                {ok,Reply,NewState,hibernate} | 
%                                {swap_handler,Reply,Args1,NewState,Handler2,Args2} |
%                                {remove_handler, Reply}
handle_call(_Request, State) ->
    {ok, ok, State}.

% handle_info(Info,State) -> {ok,NewState} | 
%                            {ok,NewState,hibernate} | 
%                            {swap_handler,Args1,NewState,Handler2,Args2} | 
%                            remove_handler
handle_info(_Message, State) ->
    {ok, State}.

% terminate(Arg, State) -> term()
terminate(_Arg, _State) ->
    ok.

% code_change(OldVsn, State, Extra) -> {ok, NewState}
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% private functions
%--------------------------------------------------------------------
