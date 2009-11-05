Note: I'm using erlang for a personal research project, and wanted easy support for literate programming.  This is what I hacked together.  I'm making this available to the public, and attempting to document it, in case it's useful to anyone else.  I may or may not continue working on this, depending on how use(ful|less) it proves to be.

compyl
======

`compyl` makes it easy to apply arbitrary text transformations to a file before passing it to the erlang compiler.

One obvious application of this is [literate programming][1].

For example, this file (README.md) is an erlang module, hidden in [Markdown][2] markup.  Anything that's prefixed by four spaces or a tab character (a code block in markdown) will be treated as erlang source code, everything else will be ignored.  So, to define our module, we just start out as usual:

	-module(readme_demo).

and tell `compyl` to pass the file through the `markdown/1` function in the module `literate`.
	
	-compile({text_transform, literate, markdown}).
	
Now we can define some functions:
	
	-export([square/1, add/2]).
	
	square(X) -> X * X.
	add(X,Y) -> X + Y.

Now, we can compile this file, and use the resulting `readme_demo` module:
<pre>
1> compyl:c("README.md").
{ok, readme_demo}
2> readme_demo:square(4).
16
</pre>

***

## Installation

<pre>
git clone git@github.com:sbillig/compyl.git
</pre>
Compile the .erl files, and put them on your erlang path.
<pre>
erlc compyl.erl literate.erl multiline_comments.erl
</pre>

<pre>
1> code:add_pathz("/path/to/compyl").
2> compyl:c("/path/to/compyl/README.md").
{ok, readme_demo}.
</pre>

***

## Usage

### Included transforms

Within `literate.erl` there are currently `text_transform` functions for literate source code files written in latex, markdown, and [Bird-style][3].  Additional transforms can be added easily, see below.

The appropriate compiler flags are, respectively:
<pre>
{text_transform, literate, latex}
{text_transform, literate, markdown}
{text_transform, literate, bird}
</pre>

There's also support for (ugly) multiline comments:
<pre>
{scan_transform, multiline_comments, strip_comments}

::"
this is a 
comment spanning
several lines
"::
	
f() -> ::"this is also a comment"::
  X = ::"and so is this":: 2,
  X*2.
</pre>
The choice of characters is arbitrary.  Other multiline comment syntax could be supported by writing a different `scan_transform` or `text_transform` function.
### Creating your own transforms

#### text_transform

A `text_transform` can be any unary function that accepts a string and returns a string, usually one containing valid erlang syntax. (Input and output are binary strings, like those returned by file:read_file/1).

For example, the `literate:markdown/1` is defined as:
<pre>
markdown(Bin) ->
   B = re:replace(Bin, "^(?!    |\\t)[^\\n]+","", [global,multiline]),
   iolist_to_binary(B).
</pre>
which simply finds all lines that don't start with four spaces or a tab, and replaces them with nothingness.  Ideally, transforms should maintain line numbers.  That is, if a piece of erlang code is on line 14 of the input string, it should be on line 14 of the output string as well.

If you want to apply the `text_transform` function `do_some_stuff` of the module `mytransforms` to a file, just add the compiler option to the file:
<pre>
mpile({text_transform, mytransforms, do_some_stuff}).
</pre>

#### scan_transform

...

***

## Inner workings

The erlang compiler supports optional `parse_transforms`, which can modify the abstract syntax tree at compile time.  `compyl` adds `text_transforms` and `scan_transforms` to the mix to allow for additional modification during the compiling process.

From the [erlang documentation][4]:

>Parse transformations are used if a programmer wants to use Erlang syntax, but with different semantics. The original Erlang code is then transformed into other Erlang code.
>
>Note:
>Programmers are strongly advised not to engage in parse transformations and no support is offered for problems encountered.

`text_` and `scan_transforms` are for operating on files that contain things that aren't valid erlang syntax.

When an erlang module source code file is compiled, the following steps are (basically) performed:

1. scan file to tokens
2. parse erlang syntax
3. apply `parse_transforms`
4. compile

Compiling with `compyl` adds 2 steps:

1. apply `text_transforms`
2. scan file to tokens
3. apply `scan_transforms`
4. parse erlang syntax
5. apply `parse_transforms`
6. compile

The way `compyl.erl` (currently) achieves this isn't particularly elegant, but it was quick and it works.  Calling compyl:file("somefile.erl") does the following:

1. read the file in as a binary string
2. extract compile options from the string, using regular expressions
3. apply any `text_transforms` to the string, in the order they're listed in the file
  eg. if the following options are passed to -compile
  <pre>[{`text_transform`, mod1, f1}, {`text_transform`, mod1, f2}]</pre>
  the following will (essentially) be done:
  <pre>
  {ok, Bin} = file:read_file("whatever.erl"),
  B = mod1:f2(mod1:f1(Bin)),
  </pre>
4. use erl_scan to convert the string into a list of tokens
5. apply any `scan_transforms` to the list of tokens
6. collapse the tokens back to a string
7. write the string to a temporary file (eg. Path = /tmp/compyl_xxxxxxxxxxxx.erl)
8. call `{ok, ModuleName, Binary} = compile:file(Path, [binary, ...]).`
9. write Binary to ModuleName.beam in the current directory

[1]: http://www.haskell.org/haskellwiki/Literate_programming
[2]: http://daringfireball.net/projects/markdown/basics
[3]: http://www.haskell.org/haskellwiki/Literate_programming#Bird_Style
[4]: http://www.erlang.org/doc/man/erl_id_trans.html