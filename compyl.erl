-module(compyl).
-compile(export_all).

% {prescan_transform, module, function}
% {scan_transform, module, function}
% {parse_transform, module, function}
% {parse_transform, module}

% modify 'compile' module to accept scan_transform option, 
%  similar to parse_transform.
%  that would allow sytax changes
% then, write scan_transforms to accept nice bits of haskell syntax :]
% and of course, multi-line comments

% ok, not easy to do directly...
% so, scan to tokens
%  then run through scan_transform functions
%  then merge tokens to a binary
%  

% goal: simple pipeline from text file to compiled code
% 1) read file
% 2) prescan_transform
% 3) scan tokens
% 4) scan_transform
% 5) preprocess with epp (expand macros, etc)
% 6) parse
% 7) parse_transform
% 7) compile

% pragmatic reality:
% 1) read file to string
% 1a) scan to tokens
% 1b) get commands from -compyl(List) attribute
% 2) prescan_transform (original string)
% 3) scan to tokens
% 4) scan_transform
% 4a) collapse tokens to string
% 4b) write string to temp file, or use file interface
% 5,6,7) compile temp file 
%         (putting result in proper location, with proper name)
% 
% 
% {ok, Bin} = file:read_file(Path),
% Str = binary_to_list(Bin),
% {ok, Tokens, _N} = erl_scan:string(Str),


test() ->
	{ok, Bin} = file:read_file("multiline_test.erl"),
	get_compiler_options(strip_comments(Bin)).


file(Path) ->
	file(Path, []).
file(Path, Options) ->
	{ok, Bin} = file:read_file(Path),
	string(Bin, Options).

string(S) ->
	string(S, []).

string(S, Options) ->
	FileOpts = get_compiler_options(S),
	
	AllOpts = Options ++ FileOpts,
	
	% apply prescan_transforms to raw binary
	S2 = apply_transforms(keyfindall(prescan_transform, 1, AllOpts), S),
	S3 = binary_to_list(iolist_to_binary(S2)),
	
	% scan to tokens, and apply scan_transforms to list of tokens
	{ok,Tokens, _} = erl_scan:string(S3),
	Tokens2 = apply_transforms(keyfindall(scan_transform, 1, AllOpts), Tokens),
	
	% collapse tokens to string, write string to temp file, then compile temp file
	Path = new_temp_file_path(),
	ok = file:write_file(Path, tokens_to_string(Tokens2)),
	case compile:file(Path, [binary, no_error_module_mismatch, verbose, report]) of
		{ok, ModuleName, Binary} ->
			file:write_file(lists:concat([ModuleName, ".beam"]), Binary),
			{ok, ModuleName};
		Other -> Other
	end.
	

strip_comments(Bin) ->
	iolist_to_binary(re:replace(Bin, ["%","[^\\n]*"], [], [global])).

get_compiler_options(Bin) ->
	Opts = case re:run(Bin, ["compile", "\\s*", "\\(", "([^)]+)", "\\)"], [global]) of
			nomatch -> [];
			{match, [{_,_},{_,_}]=L} -> [L];
			{match, List} -> List
		   end,
	% Opts.
	TermBins = lists:map(fun([{_,_},{S,L}]) -> B = substr_binary(Bin,S,L), 
											   <<B/binary, $.>> end, Opts),
	% TermBins.
	Terms = lists:map(fun(B) -> {ok, Tokens, _} = erl_scan:string(binary_to_list(B)),
								Term = case erl_parse:parse_term(Tokens) of
										{ok, T} -> T;
										{error, Info} ->
											io:format("failed to parse: ~p. error: ~p. skipping.~n", [Tokens, Info]),
											[]
									   end,
								Term end, TermBins),
	lists:flatten(Terms).

% return substring, from Start to End inclusive
%  assuming 8 bit characters
substr_binary(Bin, Start, Length) ->
	substr_binary(Bin, Start, Length, 8).
substr_binary(Bin, Start, Length, Size) ->
	S = Start * Size,
	M = Length * Size,
	<<_:S, Out:M, _/binary>> = Bin,
	<<Out:M>>.


random_letter() ->
	random:uniform($z - $a) + $a.
listof(0, _) -> [];
listof(N, Fun) ->
    [Fun() | listof(N-1, Fun)].
random_string(Length) ->
	listof(Length, fun random_letter/0).

new_temp_file_path() ->
	lists:concat(["/tmp/compyl_", random_string(12), ".erl"]).

apply_transforms(Transforms, Input) ->
	lists:foldr(fun({_, Mod, F}, X)-> Mod:F(X) end, Input, Transforms).

tokens_to_string(Tokens) ->
	tokens_to_string(Tokens, [], 1).
tokens_to_string([H|Rest], Acc, Line) ->
	case H of
		{_Type, N, A} -> ok;
		{dot, N} -> A = '.';
		{A, N} -> ok
	end,
	case N of
		Line ->
			tokens_to_string(Rest, [A|Acc], N);
		_ ->
			Newlines = lists:duplicate(N-Line, $\n),
			tokens_to_string(Rest, [A|[Newlines|Acc]], N)
	end;

tokens_to_string([], Acc, _Line) ->
	lists:concat(lists:reverse(Acc)).


keyfindall(Key, N, Tuplelist) ->
	lists:filter(fun(Tup) -> 
					case is_tuple(Tup) of
						true -> case element(N, Tup) of Key -> true; _ -> false end;
						false -> false
					end end,
				 Tuplelist).


%% capture(List, fun(X)->true/false, fun(X)->true/false)
%  return portion of List, starting with the first element to satisfy FromPred
%                          ending with the first subsequent element to satisfy ToPred
%   eg. capture("hi mom, how are you", fun(X)->X==$m end, fun(X)->X==$e end)
%        >> "mom, how are"

capture(List, FromPred, ToPred) ->
	capture(List, FromPred, ToPred, []).
	
capture([H|T], FromPred, ToPred, []) ->	
	case FromPred(H) of
		false -> capture(T, FromPred, ToPred, []);
		true  -> capture(T, FromPred, ToPred, [H])
	end;
capture([H|T], FromPred, ToPred, Acc) ->
	case ToPred(H) of
		false -> capture(T, FromPred, ToPred, [H|Acc]);
		true  -> lists:reverse([H|Acc])
	end;
capture([], _, _, Acc) ->
	lists:reverse(Acc).



options_from_tokens(Tokens) ->
	L = capture(Tokens, 
				fun(X) -> case X of {atom, _, compile} -> true; _ -> false end end,
				fun(X) -> case X of {'dot', _} -> true; _ -> false end end),	
	case L of
		[] -> [];
		[{atom,N,compile}|Rest] ->
			{ok, Parsed} = erl_parse:parse_exprs([{var, N, 'Ident'}|Rest]),
			{value, Options, _Bindings} = erl_eval:exprs(Parsed, [{'Ident', fun(X)->X end}]),
			Options
	end.
