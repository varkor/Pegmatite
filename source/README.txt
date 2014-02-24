Parserlib design overview
=========================

This is a fork and extensive rewrite of Achilleas Margaritis's ParserLib.  It
has the following goals:

- Idiomatic C++11
- Simple use
- Reuseable, reentrant grammars with multiple action delegates
- No dependency on RTTI / exceptions (useable in embedded contexts)

It has the following explicit non-goals:

- High performance (ease of use or modification should not be sacrificed in the
  name of performance)
- Compatibility with the old ParserLib

Design outline
--------------

All rules should be immutable after creation.  Ideally they'd be constexpr, but
this is likely not to be possible.  They should have no state associated with
them, however, and so parsing should be entirely reentrant.  State, for a
grammar, is in two categories:

- The current parsing state
- The actions to be performed when parsing

The actions can also be immutable (they don't change over a parse, at least),
but should not be tied to the grammar.  It should be possible to write one
singleton class encapsulating the grammar, with members for the rules, and
singleton subclasses (or, ideally, delegates) providing parser actions.  The
parsing state should be entirely contained within a context object and so the
same grammar can be used from multiple threads and can be used for compilation,
syntax highlighting, and so on.

To do
-----

Not in order:

- Finish reindenting code
- Finish removing bastardised systems Hungarian notation prefixes
- Write parameterised delegate for generating semantic HTML markup
- Write better documentation for how the AST construction works
- Add error delegate
- Rename classes so types start with uppercase
- Fix namespacing (e.g. all private classes should be in anonymous namespace)
- Add a build system!
- Use std::unique_ptr in the AST (trees are, by definition, acyclic and each
  node has a unique parent).
