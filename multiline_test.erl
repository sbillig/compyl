-module(multilinetest).

% {prescan_transform, mymod, dostuff},
% {scan_transform, multiline_comments, strip_comments}
% {parse_transform, mod}

-compile([
	export_all,
	{prescan_transform, prescan_stuff, change_f_to_h}
]).
-compile({scan_transform, multiline_comments, strip_comments}).

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
