-module (prescan_stuff).
-compile(export_all).

change_f_to_h(Bin) ->
	re:replace(Bin, ["f\\(\\)"], "h()", [global]).
	
debird(Bin) ->
	B = re:replace(Bin, ["^\\s*[^>\\n][^\\n]*"], "", [global,multiline]),
	re:replace(B, ["^\\s*>\\s*"], "", [global,multiline]).

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
% delatex_test() ->
% 	{ok, Bin} = file:read_file("latex_test.erl"),
% 	delatex(Bin).