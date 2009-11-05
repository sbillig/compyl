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


-module (multiline_comments).
-compile(export_all).

test() ->
	{ok, Bin} = file:read_file("multiline_test.lerl"),
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