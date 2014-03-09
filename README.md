scrap-expr-boilerplate
======================

In Scala, given the definition of an AST made of case classes, how do you define local transformations without boilerplate for each case class? Here's my solution, based on reflection.

In other words, this code implements (basic support for) strategic rewriting for expressions. It is of limited generality, but it supports a useful special case.

A fully general solution to this problem would be Scrap Your Boilerplate (SYB); a Scala port is available inside the Shapeless library.

Maturity
--------

This is mainly intended as a playground and for private consumption in my projects.
