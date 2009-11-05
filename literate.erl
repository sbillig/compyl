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


-module (literate).
-compile(export_all).

bird(Bin) ->
	B = re:replace(Bin, ["^\\s*[^>\\n][^\\n]*"], "", [global,multiline]),
	B2 = re:replace(B, ["^\\s*>\\s*"], "", [global,multiline]),
	iolist_to_binary(B2).

bird_test() ->
	{ok, Bin} = file:read_file("test/bird_test.lerl"),
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
	{ok, Bin} = file:read_file("test/latex_test.lerl"),
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
