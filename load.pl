:- module(load, [start/0]).
/** <module>  Set up server environment
   Geohashing server

   Copyright (c) 2013, Anne Ogborn


*/

% might have to change this if you move geohashing
user:file_search_path(weblog, '../weblog/weblog/').

% Turn on the logger
:- ensure_loaded(library(http/http_log)).
% Turn on sessions
:- ensure_loaded(library(http/http_session)).
% Needed for http:location/3, don't remove even if red
:- use_module(library(http/http_path)).

% threaded server
:- use_module(library(http/thread_httpd)).
% basic dispatch
:- use_module(library(http/http_dispatch)).
% to set the dialect
:- use_module(library(http/html_write)).
% logging - turns on and gets http_log
:- use_module(library(http/http_log)).
% head - so we can refer to resources
:- use_module(library(http/html_head)).
% so we can serve static files
:- use_module(library(http/http_files)).
% to grab parms
:- use_module(library(http/http_parameters)).

:- use_module(weblog(info/maps/map)).
:- use_module(weblog(info/geohashing/geohashing)).


% flag to ensure we only start server once
:- dynamic started/0.


%%   server_port(-Port:int) is det
%
% Returns the number to run the server on
%
% @param Port the port the server should listen on
server_port(19049).

%	%%%%%%%%%%%%%%%%%%%%  SERVER CONTROL  %%%%%%%%%%%%%%%%%%%

%%	start is nondet
%
%	Starts the server
%	nondet because the server might not start
%
start:-
	started,!,
	server_port(Port),
	format(user_error, 'Already running - browse http://127.0.0.1:~w/\n', [Port]).

start:-
	% for unclear reasons, uncommenting this breaks the google maps
	% demo
%	html_set_options([dialect(xhtml)]),
	format(user_error, 'Starting geohash server\n', []),
	server_port(Port),
	http_server(http_dispatch, [port(Port), timeout(3600)]),
	assert(started),
	http_log('Starting geohash_server on port ~w~n' , [Port]).

%%	stop_server is det
%
%	Stop the web server
%
stop_server :-
	server_port(Port),
	http_stop_server(Port, []),
	format(user_error, 'Server halted on port ~n', [Port]).

%%      bye is det
%
%  shut down server and exit
%
bye :-
	stop_server,
	halt.


%
%  No other good place for this, so it's here
%
:- http_handler(root(.) , redir_to_index,
		[id(indexroot)]).

%
%  Serve css, icons, and js by adding our directories to
%  the file search path
%
user:file_search_path(css, 'static/css').
user:file_search_path(icons, 'static/img').
user:file_search_path(js, 'static/js').

http:location(images, '/img' ,[]).

:- http_handler(images(.) , http_reply_from_files('static/img/', []), [prefix]).

%%	redir_to_index(+Request:http_request) is det
%
%	handle bare domain request by redirection to index
%
%	@param Request the HTTP request as the usual list format
%
redir_to_index(Request) :-
	http_redirect(moved_temporary, location_by_id(index), Request).

:- http_handler(root('index.htm'), index_page , [id(index)]).

% TODO handle errors
index_page(Request) :-
	get_time(Stamp),
	TZ is 3600 * 8,
	stamp_date_time(Stamp, DateTime, TZ),
	date_time_value(date, DateTime, date(Y, M, D)),
	http_parameters(Request, [
			lat(Lat, [default('37')]),
			long(Long, [default('-121')]),
			y(YY, [default(Y), integer]),
			m(MM, [default(M), integer]),
			d(DD, [default(D), integer])
				 ]),
	atom_codes(Lat, CLat),
	atom_codes(Long, CLong),
        code_graticule(CLat, CLong, Grat),
	reply_html_page(
	    title('The Impatient Geohasher'),
	    [
	    \html_requires(css('style.css')),
	    h1(['Impatient Geohasher',
	       form(action=location_by_id(index),
					 [
					  input([type=number, min=1900, max=2099, name=y, value=YY, size=4], []),
					  input([type=number, min=1, max=12, name=m, value=MM, size=2], []),
					  input([type=number, min=1, max=31, name=d, value=DD, size=2], []),
					  'lat:',input([type=number, min= -90, max= 90, name=lat, value=Lat, size=3], []),
					  ' long:', input([type=number, min= -180, max=180, name=long, value=Long, size=4], []),
					  input([type=submit, name=submit, value='Move'], [])
					 ])]),
	    \geo_map(hash_map_opts(info( YY - MM - DD, Grat))),
	     \disclaimer
	    ]).

disclaimer -->
	html([div(class=newsbox, [h2('Disclaimer'),
				  p('This map application is new.'),
				  p(['It has a test suite, which it passes, but only recently was deployed.']),
p('Before you sail far out into Antarctic waters chasing a globalhash, it would be wise to check with another map as well.'),
p(['If you see an incorrect hash point, please report',
'it to ', a(href='mailto:annie66us@yahoo.com', 'Anniepoo'), ' including the url, ',
'and ideally the source of the offending page, along with the local time and your',
'graticule.'])])]).

hash_map_opts(info(Date, Grat), center(Lat, Long)) :-
	hash_point(Date, Grat, point(Lat, Long)).
hash_map_opts(info( YY - MM - DD, Grat), point(X,Y)) :-
	between(0, 3, Offset),
	DDD is DD + Offset,
	minesweeper(YY - MM - DDD, Grat, Pts),
	member(point(X,Y), Pts).
hash_map_opts(_, provider(leaflet)).
hash_map_opts(_, id(minesweeper)).
hash_map_opts(_, zoom(8)).
hash_map_opts(_, icon(monday, '/img/markerM-01.png', '/img/markerMmask-01.png')).
hash_map_opts(_, icon(tuesday, '/img/markerT-01.png', '/img/markerMmask-01.png')).
hash_map_opts(_, icon(wednesday, '/img/markerW-01.png', '/img/markerMmask-01.png')).
hash_map_opts(_, icon(thursday, '/img/markerR-01.png', '/img/markerMmask-01.png')).
hash_map_opts(_, icon(friday, '/img/markerF-01.png', '/img/markerMmask-01.png')).
hash_map_opts(_, icon(saturday, '/img/markerSa-01.png', '/img/markerMmask-01.png')).
hash_map_opts(_, icon(sunday, '/img/markerSu-01.png', '/img/markerMmask-01.png')).

hash_map_opts(_, icon_size(_, 48, 48)).
hash_map_opts(_, shadow_size(_, 48, 48)).
hash_map_opts(_, icon_anchor(_, 27, 47)).
hash_map_opts(_, shadow_anchor(_, 27, 47)).
hash_map_opts(_, popup_anchor(_, -13, -48)).
hash_map_opts(info(YY - MM - DD, Grat), icon_for(Pt, IconName)) :-
	between(0, 3, Offset),
	DDD is DD + Offset,
	minesweeper(YY - MM - DDD, Grat, Pts),
	member(Pt, Pts),!,
	day_of_the_week(date(YY, MM, DDD), DayOfWeek),
	nth1(DayOfWeek,
	     [monday, tuesday, wednesday, thursday, friday, saturday, sunday],
	    IconName).

hash_map_opts(_, popup_for([b(Lat), ', ', b(Long)], point(Lat, Long))).


% globalhash
hash_map_opts(_, icon(globalmonday, '/img/globalM-01.png', '/img/markerMmask-01.png')).
hash_map_opts(_, icon(globaltuesday, '/img/globalT-01.png', '/img/markerMmask-01.png')).
hash_map_opts(_, icon(globalwednesday, '/img/globalW-01.png', '/img/markerMmask-01.png')).
hash_map_opts(_, icon(globalthursday, '/img/globalR-01.png', '/img/markerMmask-01.png')).
hash_map_opts(_, icon(globalfriday, '/img/globalF-01.png', '/img/markerMmask-01.png')).
hash_map_opts(_, icon(globalsaturday, '/img/globalSa-01.png', '/img/markerMmask-01.png')).
hash_map_opts(_, icon(globalsunday, '/img/globalSu-01.png', '/img/markerMmask-01.png')).

hash_map_opts(info( YY - MM - DD, _), Pt) :-
	between(0, 3, Offset),
	DDD is DD + Offset,
	globalhash(YY - MM - DDD, Pt).
hash_map_opts(info(YY - MM - DD, _), icon_for(Pt, IconName)) :-
	between(0, 3, Offset),
	DDD is DD + Offset,
	globalhash(YY - MM - DDD, Pt),!,
	day_of_the_week(date(YY, MM, DDD), DayOfWeek),
	nth1(DayOfWeek,
	     [globalmonday, globaltuesday, globalwednesday, globalthursday, globalfriday, globalsaturday, globalsunday],
	    IconName).






