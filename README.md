geohash
=======

Geohashing map server in SWI-Prolog

Requires the Weblog library


Install
=======

install weblog, including setting map keys
in weblog/info/keys

Back in geohash,
modify load.pl near top (roughly line 11) to point at weblog root

Start
=====

To start in debug mode query autostart/0
To start in production query start/0.

To change the port it runs on , modify server_port/1.





