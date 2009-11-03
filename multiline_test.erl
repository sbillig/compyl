-module(multilinetest).

-compile([
	export_all,
	% {prescan_transform, mymod, dostuff},
	% {prescan_transform, othermod, morestuff},
	{scan_transform, multiline_comments, strip_comments}
	% {parse_transform, mymod} % using standard compile option for now
]).

::"this is a comment
ooga booga booga
"::

f() ->
	X =::"even stupid places":: 2,
	::"and they can span
	   more than one line":: Y = 3,
	X * Y.

g() ->
	true.
