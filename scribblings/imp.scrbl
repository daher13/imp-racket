#lang scribble/manual
@require[@for-label[imp
                    racket/base]]

@title{An interpreter for the IMP programming language}
@author{Rodrigo Ribeiro}

@defmodule[imp]

IMP is a core imperative language defined in Wynskell's book on
formal semantics. This package provides an big-step interpreter
for IMP and some dialects for debugging.

@section{IMP dialects}

Language ``imp'' is just the interpreter for the language.
The dialect ``imp/debug/tokenize-only'' will emit the result
of the lexical analysis. Dialect ``imp/debug/parse-only''
prints the result parse tree and ``imp/debug/final-env'' shows
the final value of all program variables.