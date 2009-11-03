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


keyfindall(Key, N, Tuplelist) ->
	lists:filter(fun(Tup) -> 
					case is_tuple(Tup) of
						true -> case element(N, Tup) of Key -> true; _ -> false end;
						false -> false
					end end,
				 Tuplelist).

file(Path) ->
	file(Path, []).
file(Path, Options) ->
	{ok, Bin} = file:read_file(Path),
	string(binary_to_list(Bin), Options).

string(S) ->
	string(S, []).
string(S, Options) ->
	{ok, Toks, _} = erl_scan:string(S),
	FileOpts = options_from_tokens(Toks),
	AllOpts = Options ++ FileOpts,
	
	% to (potentially) avoid scanning the same string twice,
	%  we could skip this step if keyfindall(prescan...) == []
	S2 = apply_transforms(keyfindall(prescan_transform, 1, AllOpts), S),
	{ok,Tokens, _} = erl_scan:string(S2),
	Tokens2 = apply_transforms(keyfindall(scan_transform, 1, AllOpts), Tokens),
	Path = new_temp_file_path(),
	ok = file:write_file(Path, tokens_to_string(Tokens2)),
	case compile:file(Path, [binary, no_error_module_mismatch, verbose, report]) of
		{ok, ModuleName, Binary} ->
			file:write_file(lists:concat([ModuleName, ".beam"]), Binary),
			{ok, ModuleName};
		Other -> Other
	end.
	
	% ideally, we'd parse, apply the parse transforms, then compile
	%  but to make things easy, we'll just write the tokens to a temp file,
	%  and run it through the compiler
	
	
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

























