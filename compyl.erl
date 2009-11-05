%% compyl, a hacked together literate-programming-enabler
%%
%% Copyright (c) 2009 Sean Billig
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.


-module(compyl).
-compile(export_all).

% {text_transform, module, function}
% {scan_transform, module, function}
% {parse_transform, module}

% goal: simple pipeline from text file to compiled code
% 1) read file
% 2) text_transform
% 3) scan tokens
% 4) scan_transform
% 5) preprocess with epp (expand macros, etc)
% 6) parse
% 7) parse_transform
% 7) compile

test() ->
	{ok, Bin} = file:read_file("multiline_test.erl"),
	get_compiler_options(strip_comments(Bin)).

c(Path) ->
	c(Path, []).
c(Path, Options) ->
	case file(Path, Options) of
		{ok, Mod} -> 
			code:purge(Mod),
			code:load_file(Mod),
			{ok, Mod};
		Other ->
			Other
	end.

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
	
	% apply text_transforms to raw binary
	S2 = apply_transforms(keyfindall(text_transform, 1, AllOpts), S),
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

% return substring, from Start to Start+Length, inclusive
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
		{string, N, Str} -> A = lists:concat(["\"", Str, "\""]);
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
