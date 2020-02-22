:- op(100, xfx, at).
:- op(75, xfx, ><). % Arrival >< Departure
:- op(50, xfx, :).


route([
    "Amsterdam Centraal" at 10:05 >< 10:05,
    "Amsterdam Amstel" at 10:13 >< 10:13,
    % They are working here(wait(_)).
    "Utrecht Centraal" at 11:31 >< 11:33,
    "'s-Hertogenbosch" at 12:00 >< 12:06,
    "Eindhoven" at 12:24 >< 12:27,
    "Weert" at 12:47 >< 12:50,
    "Roermond" at 13:04 >< 13:05,
    "Sittard" at 13:21 >< 13:24,
    "Maastricht" at 13:44 >< 13:44
]).

route([
    "Amsterdam Centraal" at 10:00 >< 10:00,
    % Stop train in front of us....
    "Amsterdam Amstel" at 10:22 >< 10:22,
    "Utrecht Centraal" at 10:40 >< 10:44,
    "'s-Hertogenbosch" at 11:10 >< 11:15,
    "Eindhoven" at 11:34 >< 11:38,
    "Weert" at 11:55 >< 11:55,
    "Roermond" at 12:09 >< 12:10,
    "Sittard" at 12:25 >< 12:26,
    "Heerlen" at 12:41 >< 12:41
]).

route([
    "Amsterdam Centraal" at 11:05 >< 11:05,
    "Amsterdam Amstel" at 11:13 >< 11:13,
    "Utrecht Centraal" at 11:31 >< 11:33,
    "'s-Hertogenbosch" at 12:00 >< 12:06,
    "Eindhoven" at 12:24 >< 12:27,
    "Weert" at 12:44 >< 12:44,
    "Roermond" at 12:58 >< 12:58,
    "Sittard" at 13:15 >< 13:16,
    "Maastricht" at 13:31 >< 13:31
]).

route([
    "Amsterdam Centraal" at 10:24 >< 10:25,
    "Amsterdam Amstel" at 10:31 >< 10:33,
    "Utrecht Centraal" at 10:52 >< 10:54,
    "Veenendaal-De Klomp" at 11:12 >< 11:12,
    "Ede-Wageningen" at 11:18 >< 11:18,
    "Arnhem Centraal" at 11:29 >< 11:36,
    "Nijmegen" at 11:49 >< 11:49
]).


route([
    "Utrecht Centraal" at 11:09 >< 11:09,
    "Driebergen-Zeist" at 11:17 >< 11:17,
    "Ede-Wageningen" at 11:33 >< 11:33,
    "Arnhem Centraal" at 11:44 >< 11:51,
    "Nijmegen" at 12:03 >< 12:03
]).

route([
    "Amsterdam Centraal" at 10:54 >< 10:54,
    "Amsterdam Amstel" at 11:01 >< 11:03,
    "Utrecht Centraal" at 11:22 >< 11:24,
    "Veenendaal-De Klomp" at 11:42 >< 11:42,
    "Ede-Wageningen" at 11:48 >< 11:48,
    "Arnhem Centraal" at 11:59 >< 12:07,
    "Nijmegen" at 12:20 >< 12:20
]).

route([
    "Utrecht Centraal" at 11:39 >< 11:39,
    "Driebergen-Zeist" at 11:47 >< 11:47,
    "Ede-Wageningen" at 12:03 >< 12:03,
    "Arnhem Centraal" at 12:13 >< 12:20,
    "Nijmegen" at 12:32 >< 12:32
]).


route([
    "Schiphol Airport" at 10:16 >< 10:16,
    "Amsterdam Zuid" at 10:24 >< 10:24,
    "Amsterdam RAI" at 10:26 >< 10:28,
    "Amsterdam Bijlmer Arena" at 10:33 >< 10:33,
    "Utrecht Centraal" at 10:48 >< 10:54,
    "'s-Hertogenbosch" at 11:22 >< 11:22
]).


route([
    "Schiphol Airport" at 10:47 >< 10:47,
    "Amsterdam Zuid" at 10:54 >< 10:54,
    "Amsterdam RAI" at 10:56 >< 10:58,
    "Amsterdam Bijlmer Arena" at 11:03 >< 11:03,
    "Utrecht Centraal" at 11:18 >< 11:18
]).

route([
    "Schiphol Airport" at 11:16 >< 11:16,
    "Amsterdam Zuid" at 11:24 >< 11:24,
    "Amsterdam RAI" at 11:26 >< 11:28,
    "Arnhem Centraal" at 11:36 >< 11:36,
    "Amsterdam Bijlmer Arena" at 11:33 >< 11:33,
    "Utrecht Centraal" at 11:48 >< 11:48
]).

route([
    "Schiphol Airport" at 11:47 >< 11:47,
    "Amsterdam Zuid" at 11:54 >< 11:54,
    "Amsterdam RAI" at 11:56 >< 11:58,
    "Amsterdam Bijlmer Arena" at 12:03 >< 12:03,
    "Utrecht Centraal" at 12:18 >< 12:24,
    "'s-Hertogenbosch" at 12:52 >< 12:55,
    "Eindhoven" at 13:14 >< 13:14
]).

route([
    "Schiphol Airport" at 10:00 >< 10:00,
    "Amsterdam Zuid" at 10:07 >< 10:07,
    "Amsterdam RAI" at 10:09 >< 10:11,
    "Amsterdam Bijlmer Arena" at 10:15 >< 10:18,
    "Utrecht Centraal" at 10:35 >< 10:39,
    "Driebergen-Zeist" at 10:47 >< 10:47,
    "Ede-Wageningen" at 11:03 >< 11:03,
    "Arnhem Centraal" at 11:13 >< 11:20,
    "Nijmegen" at 11:32 >< 11:32
]).

route([
    "Schiphol Airport" at 10:30 >< 10:30,
    "Amsterdam Zuid" at 10:37 >< 10:37,
    "Amsterdam RAI" at 10:39 >< 10:41,
    "Amsterdam Bijlmer Arena" at 10:45 >< 10:51,
    "Utrecht Centraal" at 11:07 >< 11:09,
    "Driebergen-Zeist" at 11:17 >< 11:17,
    "Ede-Wageningen" at 11:33 >< 11:33,
    "Arnhem Centraal" at 11:44 >< 11:50,
    "Nijmegen" at 12:02 >< 12:02
]).

route([
    "Schiphol Airport" at 11:00 >< 11:00,
    "Amsterdam Zuid" at 11:07 >< 11:07,
    "Amsterdam RAI" at 11:09 >< 11:11,
    "Amsterdam Bijlmer Arena" at 11:15 >< 11:18,
    "Utrecht Centraal" at 11:35 >< 11:39,
    "Driebergen-Zeist" at 11:47 >< 11:47,
    "Ede-Wageningen" at 12:03 >< 12:03,
    "Arnhem Centraal" at 12:13 >< 12:20,
    "Nijmegen" at 12:32 >< 12:32
]).