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
:- use_module(weblog(html_form/radio)).

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
		        centerlat(CenterLat, [default(Lat)]),
			centerlong(CenterLong, [default(Long)]),
			y(YY, [default(Y), integer]),
			m(MM, [default(M), integer]),
			d(DD, [default(D), integer]),
			z(ZZ, [default(8), integer]),
			maptype(MapType, [default(leaflet)])
				 ]),
	atom_codes(Lat, CLat),
	atom_codes(Long, CLong),
        code_graticule(CLat, CLong, Grat),
	maptype_provider(MapType, Provider),
	reply_html_page(
	    title('The Impatient Geohasher'),
	    [
	    \html_requires(css('style.css')),
	    h1(['Impatient Geohasher',
	       form([id=locform, action=location_by_id(index)],
					 [
   div(class=smallbox, [
	\image_radio_set(map_type_buttons_gen(MapType))
%      \map_type_button(MapType, leaflet, 'Leaflet'),
%      \map_type_button(MapType, google, 'Hybrid'),
%      \map_type_button(MapType, sat, 'Overhead'),
%      \map_type_button(MapType, terrain, 'Contour')
		       ]),
   input([type=hidden, id=centerlat, name=centerlat, value=CenterLat], []),
   input([type=hidden, id=centerlong, name=centerlong, value=CenterLong], []),
   input([type=hidden, id=z, name=z, value=ZZ], []),
   input([type=number, min=1900, max=2099, id=y, name=y, value=YY, size=4], []),
   input([type=number, min=1, max=12, id=m, name=m, value=MM, size=2], []),
   input([type=number, min=1, max=31, id=d, name=d, value=DD, size=2], []),
   'lat:',input([type=number, min= -90, max= 90, id=lat, name=lat, value=Lat, size=3], []),
   ' long:', input([type=number, min= -180, max=180, id=long, name=long, value=Long, size=4], []),
   input([type=submit, id=submitt, name=submitt, value='Move'], [])
   ])]),
	    \geo_map(hash_map_opts([
			 centerlat=CenterLat, centerlong=CenterLong,
			 date=(YY - MM - DD),
			 grat=Grat,
			 zoom=ZZ,
			 maptype=MapType])),
	    \dynamic_update_script(Provider),
	    \disclaimer
	    ]).

map_type_buttons_gen(_, set_name(maptype)).
map_type_buttons_gen(_, id(leaflet)).
map_type_buttons_gen(_, id(google)).
map_type_buttons_gen(_, id(sat)).
map_type_buttons_gen(_, id(terrain)).
map_type_buttons_gen(_, image(leaflet, '/img/leaflettype.png')).
map_type_buttons_gen(_, image(google, '/img/googletype.png')).
map_type_buttons_gen(_, image(sat, '/img/sattype.png')).
map_type_buttons_gen(_, image(terrain, '/img/terraintype.png')).
map_type_buttons_gen(_, selected_image(leaflet, '/img/leaflettypesel.png')).
map_type_buttons_gen(_, selected_image(google, '/img/googletypesel.png')).
map_type_buttons_gen(_, selected_image(sat, '/img/sattypesel.png')).
map_type_buttons_gen(_, selected_image(terrain, '/img/terraintypesel.png')).
map_type_buttons_gen(MapType, default(MapType)).

map_type_button(MapType, MapType, Label) -->
	!,html([
p([input([type=radio, name=maptype, checked=checked, value=MapType], []), Label])
	     ]).
map_type_button(_, Type, Label) -->
	html([
p([input([type=radio, name=maptype, value=Type], []), Label])
	     ]).

maptype_provider(leaflet, leaflet).
maptype_provider(google,  google).
maptype_provider(sat, google).
maptype_provider(terrain, google).

maptype_provider_map_type(leaflet, Style) :-
	hash_map_opts([], style(Style)).
maptype_provider_map_type(google, 'HYBRID').
maptype_provider_map_type(sat, 'SATELLITE').
maptype_provider_map_type(terrain, 'TERRAIN').

% temporary til I write the google location adjust
dynamic_update_script(google) -->
	html(\html_post(head, [
	    script(type('text/javascript'), \[
'function gratStyle(x) {
      if(x <= -1.0) return  Math.ceil(x).toString();
      if(x < 0.0) return \'-0\';
      if(x < 1.0) return \'0\';
      return Math.floor(x).toString();
}\n',
'function init2() {\n',
' google.maps.event.addListener(minesweeper, \'zoom_changed\', function() {\n',
'   document.getElementById(\'z\').value = minesweeper.getZoom();
window.setTimeout(function() {	\n',
'    document.getElementById(\'locform\').submit();
    }, 9000);
});\n',

' google.maps.event.addListener(minesweeper, \'center_changed\', function() {\n',
'   document.getElementById(\'lat\').value = gratStyle(minesweeper.getCenter().lat());
    document.getElementById(\'long\').value = gratStyle(minesweeper.getCenter().lng());
    document.getElementById(\'centerlat\').value = minesweeper.getCenter().lat().toString();
    document.getElementById(\'centerlong\').value = minesweeper.getCenter().lng().toString();
window.setTimeout(function() {	\n',
'    document.getElementById(\'locform\').submit();
    }, 3000);
});\n',
'}
google.maps.event.addDomListener(window, \'load\', init2);'])])).

dynamic_update_script(leaflet) -->
	html(
	    script(type('text/javascript'),\[
'function gratStyle(x) {
      if(x <= -1.0) return  Math.ceil(x).toString();
      if(x < 0.0) return \'-0\';
      if(x < 1.0) return \'0\';
      return Math.floor(x).toString();
}',
'function onMoveEnd(e) {
    document.getElementById(\'lat\').value = gratStyle(minesweeper.getCenter().lat);
    document.getElementById(\'long\').value = gratStyle(minesweeper.getCenter().lng);',
'    document.getElementById(\'centerlat\').value = minesweeper.getCenter().lat.toString();
    document.getElementById(\'centerlong\').value = minesweeper.getCenter().lng.toString();
    document.getElementById(\'locform\').submit();
}',
'function onZoomEnd(e) {
     document.getElementById(\'z\').value = minesweeper.getZoom();
}',
'minesweeper.on(\'zoomend\', onZoomEnd);',
'minesweeper.on(\'moveend\', onMoveEnd);'])).

disclaimer -->
	html([div(class=newsbox, [h2('Disclaimer'),
				  p('This map application is new. It has a test suite, which it passes, but only recently was deployed.'),
p('Before you sail far out into the Southern Ocean chasing a globalhash, it would be wise to check with another map as well.'),
p(['If you see an incorrect hash point, please report',
'it to ', a(href='mailto:annie66us@yahoo.com', 'Anniepoo'), ' with your date and graticule.'])])]).

hash_map_opts(Info, center(Lat, Long)) :-
	member(centerlat=Lat, Info),
	member(centerlong=Long, Info).
hash_map_opts(Info, point(X,Y)) :-
	member(date=(YY - MM - DD), Info),
	member(grat=Grat, Info),
	between(0, 3, Offset),
	DDD is DD + Offset,
	minesweeper(YY - MM - DDD, Grat, Pts),
	member(point(X,Y), Pts).
hash_map_opts(Info, provider(Provider)) :-
	member(maptype=MT, Info),
	maptype_provider(MT, Provider).
hash_map_opts(_, id(minesweeper)).
hash_map_opts(Info, zoom(Z)) :-
	member(zoom=Z, Info).
hash_map_opts(Info, maptype(MT)) :-
	member(maptype=UMT, Info),
	maptype_provider_map_type(UMT, MT).

hash_map_opts(_, icon(monday, '/img/markerM-01.png', '/img/markerMmask-01.png')).
hash_map_opts(_, icon(tuesday, '/img/markerT-01.png', '/img/markerMmask-01.png')).
hash_map_opts(_, icon(wednesday, '/img/markerW-01.png', '/img/markerMmask-01.png')).
hash_map_opts(_, icon(thursday, '/img/markerR-01.png', '/img/markerMmask-01.png')).
hash_map_opts(_, icon(friday, '/img/markerF-01.png', '/img/markerMmask-01.png')).
hash_map_opts(_, icon(saturday, '/img/markerSa-01.png', '/img/markerMmask-01.png')).
hash_map_opts(_, icon(sunday, '/img/markerSu-01.png', '/img/markerMmask-01.png')).

hash_map_opts(_, icon_size(Icon, 48, 48)) :- global_icon(Icon).
hash_map_opts(_, shadow_size(Icon, 48, 48)) :- global_icon(Icon).
hash_map_opts(_, icon_anchor(Icon, 27, 47)) :- global_icon(Icon).
hash_map_opts(_, shadow_anchor(Icon, 27, 47)) :- global_icon(Icon).
hash_map_opts(_, popup_anchor(Icon, -13, -48)) :- global_icon(Icon).

hash_map_opts(_, icon_size(_, 96, 96)).
hash_map_opts(_, shadow_size(_, 96, 96)).
hash_map_opts(_, icon_anchor(_, 48, 96)).
hash_map_opts(_, shadow_anchor(_, 48, 96)).
hash_map_opts(_, popup_anchor(_, 0, -64)).
hash_map_opts(Info, icon_for(Pt, IconName)) :-
	member(date=(YY - MM - DD), Info),
	member(grat=Grat, Info),
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

hash_map_opts(Info, Pt) :-
	member(date=(YY - MM - DD), Info),
	between(0, 3, Offset),
	DDD is DD + Offset,
	globalhash(YY - MM - DDD, Pt).
hash_map_opts(Info, icon_for(Pt, IconName)) :-
	member(date=(YY - MM - DD), Info),
	between(0, 3, Offset),
	DDD is DD + Offset,
	globalhash(YY - MM - DDD, Pt),!,
	day_of_the_week(date(YY, MM, DDD), DayOfWeek),
	nth1(DayOfWeek,
	     [globalmonday, globaltuesday, globalwednesday, globalthursday, globalfriday, globalsaturday, globalsunday],
	    IconName).


global_icon(Icon) :-
	member(Icon, [globalmonday, globaltuesday, globalwednesday, globalthursday, globalfriday, globalsaturday, globalsunday]).



