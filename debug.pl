:- module(debug, [pldoc/0, portray_list_innerds/1]).
/**   <module> Consult me to bring up normal dev environment.

   To run the server, query autostart/0.
   To run the pldoc server, query pldoc/0, which starts the
   pldoc server on http://127.0.0.1:4040/help/source

   @copyright Copyright (C) 2013, Anne Ogborn
   All Rights Reserved
*/

% Needed for http:location/3, don't remove even if red!!!
:- use_module(library(http/http_path)).

:- multifile http:location/3.

http:location(pldoc, root('help/source'), [priority(10)]).

:- doc_server(4040).
% makes codes style strings be "abc" instead of numbers
:- portray_text(true).
% makes string style strings be `abc` instead of "abc"
% which is ez to confuse with codes
:- set_prolog_flag(backquoted_string, true).
% more reasonable default for how many items to print before ellipsizing
:- set_prolog_flag(toplevel_print_options,
	[backquoted_string(true), max_depth(9999),
	 portray(true), spacing(next_argument)]).
:- set_prolog_flag(debugger_print_options,
	[backquoted_string(true), max_depth(9999),
	 portray(true), spacing(next_argument)]).
user:portray([H|T]) :-
	write('['),
	portray_list_innerds([H|T]),
	write(']').

portray_list_innerds([]).
portray_list_innerds([H]) :-
	print(H).
portray_list_innerds([H|T]) :-
	print(H),
	write(','),
	portray_list_innerds(T).


:- ensure_loaded(load).


%%	pldoc is det
%
%	Run the pldoc server on 4040 and open the root page
%
pldoc :-
	doc_server(4040),
	www_open_url('http://127.0.0.1:4040/help/source').

:- writeln('\
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\
%                                           %\n\
%    To run the pldoc server query pldoc.   %\n\
% To run the geohash server autostart.      %\n\
%                                           %\n\
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%').

%%	autostart is nondet
%
%	Start the server in debug mode and show
%	the admin login page
%	debug mode means
%
% * various debug messages are turned on
% * services are set to 'simulate'
%
autostart :-
       start,
       load:server_port(Port),
       format(string(S), 'http://127.0.0.1:~w/' , [Port]),
       www_open_url(S).





