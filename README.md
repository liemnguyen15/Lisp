# Lisp
I wrote some lisp programs to familiarize myself with Artificial Intelligence.

• The command to use Common LISP is clisp.
• Appendix A of LISPcraft summarizes LISP’s built-in functions. Each function is explained
briefly. You will find this a very useful reference as you write and debug your
programs. Also, you can get help about clisp by typing:

man clisp

• The test program will be provided. It exercises the functions that you write; hence
there is no test data. If ’testl.l’ is the name of the test file in your directory, then,
within LISP, you need only type
> (load "test.l")

Details regarding the test program can be found in the attached materials for this
assignment.

• The test program may be executed by calling ./test.sh.
• You may define additional helper functions that your main functions use. Be sure,
though, to name the main functions as specified since the test program uses those
names.
• If you place a init.lsp file in the directory in which you execute LISP (or your home
directory), LISP will load that file automatically when it starts execution. Such a file
is useful to define your own environment. For instance, you will probably want to put
the command

(setq *print-case* :downcase)

in that file.

• When developing your program, you might find it easier to test your functions first
interactively before using the test program. You might find trace, step, print, etc.
functions useful in debugging your functions.
