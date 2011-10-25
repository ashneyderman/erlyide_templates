${file_comment}

-module(${module}).
-behaviour(gen_event).

-export([]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

% public APIs
%--------------------------------------------------------------------

% gen_event callbacks
%--------------------------------------------------------------------

%
% @private
% @doc 
% Whenever a new event handler is added to an event manager, this 
% function is called to initialize the event handler.
%
% @spec init(InitArgs) -> {ok,State} | 
%                         {ok,State,hibernate}
%       InitArgs = Args | {Args,Term}
%       Args = term() 
%       Term = term()
%       State = term()
% @end
%--------------------------------------------------------------------
init({_Args,_Term}) -> {ok, {}};
init(_Args) -> {ok, {}}.

%
% @private
% @doc  
% Whenever  an event manager receives an event sent using 
% gen_event:notify/2 or gen_event:sync_notify/2 , this function is 
% called for each installed event handler to handle the event.
%
% @spec handle_event(Event, State) -> {ok,NewState} |
%                                     {ok,NewState,hibernate} |
%                                     {swap_handler,Args1,NewState,Handler2,Args2} |
%                                     remove_handler
%       Event = term()
%       State = term()
%       NewState = term()
%       Args1 = term() 
%       Args2 = term()
%       Handler2 = Module2 | {Module2,Id}
%       Module2 = atom()
%       Id = term()
% @end
%--------------------------------------------------------------------
handle_event(_Request, State) ->
    {ok, State}.

%
% @private
% @doc
% Whenever  an  event  manager receives a request sent using 
% gen_event:call/3,4 , this function is called for the specified event
% handler to handle the request.
%
% @spec handle_call(Request, State) -> {ok,Reply,NewState} | 
%                                      {ok,Reply,NewState,hibernate} | 
%                                      {swap_handler,Reply,Args1,NewState,Handler2,Args2} |
%                                      {remove_handler, Reply}
%       Request = term()
%       State = term()
%       Reply = term()
%       NewState = term()
%       Args1 = term() 
%       Args2 = term()
%       Handler2 = Module2 | {Module2,Id}
%       Module2 = atom()
%       Id = term()
% @end
%--------------------------------------------------------------------
handle_call(_Request, State) ->
    {ok, ok, State}.

%
% @private
% @doc 
% This  function  is  called for each installed event handler when an 
% event manager receives any other message than an event or a synchronous 
% request (or a system message).
%
% @spec handle_info(Info,State) -> {ok,NewState} | 
%                                  {ok,NewState,hibernate} | 
%                                  {swap_handler,Args1,NewState,Handler2,Args2} | 
%                                  remove_handler
%       Info = term()
%       State = term()
%       NewState = term()
%       Args1 = term() 
%       Args2 = term()
%       Handler2 = Module2 | {Module2,Id}
%       Module2 = atom()
%       Id = term()
% @end
%--------------------------------------------------------------------
handle_info(_Message, State) ->
    {ok, State}.

%
% @private
% @doc 
% Whenever an event handler is deleted from an event manager, this 
% function is called. It should be the opposite of Module:init/1
% and do any necessary cleaning up.
%
% @spec terminate(Arg, State) -> term()
%       Arg = Args | 
%             {stop,Reason} | 
%             stop | 
%             remove_handler | 
%             {error,{'EXIT',Reason}} | 
%             {error,Term}
%       Args = term()
%       Reason = term() 
%       Term = term()
%       State = term()
% @end
%--------------------------------------------------------------------
terminate(_Arg, _State) ->
    ok.

%
% @private
% @doc 
% This function is called for an installed event handler which should 
% update its internal state during  a  release  upgrade/downgrade,  
% i.e. when the instruction {update,Module,Change,...} where 
% Change={advanced,Extra} is given in the .appup file. See OTP Design 
% Principles for more information.
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
