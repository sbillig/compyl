Markdown test
=============

This is an example of an erlang module defined in a markdown file.  Each line of code must be prefixed with 4 spaces or a tab character.

	-module(markd_test).
	-compile(export_all).

For this file to compile, we must pass it through the appropriate `text_transform` function.

	-compile({text_transform, literate, markdown}).

Now we can define some functions, like we normally would:

	double(X) -> X*2.
	
	sayhi(Name) -> io:format("hello ~p~n",[Name]).
	test() -> lists:concat(["Hi", " friend"]).

	square(X) -> X*3.
	
That's enough for this example.  To compile the module `markd_test`, run `compyl:file("markdown_test.md").` in an erl shell.