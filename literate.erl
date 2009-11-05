-module (literate).
-compile(export_all).

change_f_to_h(Bin) ->
	re:replace(Bin, ["f\\(\\)"], "h()", [global]).
	
bird(Bin) ->
	B = re:replace(Bin, ["^\\s*[^>\\n][^\\n]*"], "", [global,multiline]),
	B2 = re:replace(B, ["^\\s*>\\s*"], "", [global,multiline]),
	iolist_to_binary(B2).

bird_test() ->
	{ok, Bin} = file:read_file("test/bird_test.erl"),
	bird(Bin).

latex(Bin) ->
	case re:run(Bin, "(.*?)\\\\begin{code}(.*?)\\\\end{code}", [global, dotall]) of
		nomatch -> Bin;
		{match, [{_,_},{_,_},{_,_}]=L} -> latex(Bin, [L]);
		{match, Matches} -> latex(Bin, Matches)
	end.

latex(Bin, Matches) ->
	{ok, NotANewline} = re:compile("[^\\n]+"),
	Out = lists:map(fun([_All, {NonCodeStart, NonCodeLength}, {CodeStart, CodeLength}]) ->
					
						NonCode = compyl:substr_binary(Bin, NonCodeStart, NonCodeLength),
						Code = compyl:substr_binary(Bin, CodeStart, CodeLength),
					
						[re:replace(NonCode, NotANewline, "", [global]), Code]
					end,
					Matches),
	iolist_to_binary(Out).

latex_test() ->
	{ok, Bin} = file:read_file("test/latex_test.erl"),
	latex(Bin).

markdown(Bin) ->
	B = re:replace(Bin, "^(?!    |\\t)[^\\n]+","", [global,multiline]),
	iolist_to_binary(B).
	% re:replace(B, ["^\\s*>\\s*"], "", [global,multiline]).
	
markdown_test() ->
	{ok, Bin} = file:read_file("test/markdown_test.md"),
	markdown(Bin).
	% {ok, BeginRE} = re:compile("\\\\begin{code}"),
	% re:run(BinLines, BeginRE).

% delatex(Bin) ->
% 	BinLines = re:split(Bin, "\\n"),
% 	{ok, BeginRE} = re:compile("\\\\begin{code}"),
% 	re:run(BinLines, BeginRE).

% count_newlines(Bin) ->
% 	true.
% delatex(Bin) ->
% 	case re:run(Bin, "\\\\begin{code}.*?\\\\end{code}", [global, dotall]) of
% 		nomatch -> Bin;
% 		{match, [{_,_},{_,_}] = L} -> get_nonlatex_chunks(Bin, [L]);
% 		{match, L} -> get_nonlatex_chunks(Bin, L)
% 	end.
% 
% get_nonlatex_chunks(Bin, L) ->
	% given {Pos, Length}, let N = number of newlines between start of Bin and Pos
	% N * \n ++ binary_substr(Bin, Pos, Length) ++ Rest
	
	% let N2 = number of newlines between position of previous /end{code} and Pos
	% Rest = N2 * \n ++ binary_substr(Bin, Pos, Length)
	
	% etc.	
