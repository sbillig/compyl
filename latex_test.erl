Not code.

\begin{code}
	-module (latex_test).
	-compile(export_all).
	-compile({prescan_transform, prescan_stuff, delatex}).
\end{code}

Also not code.

The following will trigger: "Warning: variable 'Y' is unused".
The reported line number should match the actual line number in this file.

\begin{code}
	
	f(X, Y) -> X+1.
	
\end{code}

the end.
