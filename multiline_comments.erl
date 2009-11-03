-module (multiline_comments).
-compile(export_all).

test() ->
	{ok, Bin} = file:read_file("multiline_test.erl"),
	{ok, Tokens, _} = erl_scan:string(binary_to_list(Bin)),
	compyl:tokens_to_string(strip_comments(Tokens)).

strip_comments(Tokens) ->
	strip_comments(Tokens, []).
strip_comments([{'::',_}|Others], Acc) ->
	case Others of
		[{string, _, _},{'::',_} | Rest] ->
			strip_comments(Rest, Acc);
		[{string, _, _} | Rest] ->
			strip_comments(Rest, Acc);
		[{'::',_} | Rest] ->
			strip_comments(Rest, Acc)
	end;
strip_comments([H|Rest], Acc) ->
	strip_comments(Rest, [H|Acc]);
strip_comments([], Acc) ->
	lists:reverse(Acc).