Pegmatite design overview
=========================

This is a fork and extensive rewrite of Achilleas Margaritis's ParserLib.  It
has the following goals:

- Idiomatic C++11
- Simple use
- Reuseable, reentrant grammars with multiple action delegates
- No dependency on RTTI / exceptions (usable in embedded contexts)

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

Defining a grammar
------------------

You can create freestanding `Rule` instances, however the recommended way of
creating a grammar is to place each rule inside a class.  The Calculator
example follows this pattern, with a `CalculatorGrammar` class containing one
field for each rule.  This is recommended for two reasons:

 1) It allows lazy creation, rather than requiring the rules to be instantiated
    on program creation.  This gives faster start-up times and means that
    memory is only used when the grammar is actually used.
 2) It improves encapsulation.  Although it is completely safe to refer to
    rules in other grammars, placing them in a class makes it easy for people
    binding actions to the grammar to identify all of the rules that they must
    (or, might want to) provide actions for.

Simple character or string recognising expressions can be created using the
`_E` custom literal suffix.  For example `"int"_E` creates an expression that
will match the literal string "int".  This is useful for terminals.

You can define more complex operations from these by using the following
operators (where `a` and `b` are expressions):

 - `*a` matches zero or more instances of `a`
 - `+a` matches one or more instances of `a`
 - `-a` matches zero or one instance of `a`
 - `a >> b` matches `a` and then `b`
 - `a | b` matches either `a` or `b`

Newline rules have no special meaning in parsing, but are used to increment the
line counter for input ranges.  If you declare a rule as matching a newline, it
will increment the line counter every time it is successfully matched.

Whitespace rules allow implicit whitespace in between all non-terminal
expressions (sequences).

Building an AST
---------------

The `ASTContainer` class is intended to be used as the superclass for most AST
nodes.  Any `ASTPtr` and `ASTList` fields of subclasses of this class will
automatically be created from the AST stack.  Each AST class that is
constructed is pushed onto the stack in the order that it is constructed and
then popped off by its parents.

AST nodes do not, by default, keep around the `InputRange` of the text that
they matched.  This is to save space for cases where it is not required.  If
you intend to do helpful error reporting after semantic analysis, then it is
strongly recommended that you do keep such a reference.

After you have defined your grammar and AST, all that remains is to bind the
two together.  To do this, create a subclass of `ASTParserDelegate`, with one
`BindAST` field for each AST node, initialised with the corresponding grammar
rule.  The following is the parser for the Calculator example:

	class CalculatorParser : public ASTParserDelegate
	{
		BindAST<AST::Number> num = CalculatorGrammar::get().num;
		BindAST<AST::AddExpression> add = CalculatorGrammar::get().add_op;
		BindAST<AST::SubtractExpression> sub = CalculatorGrammar::get().sub_op;
		BindAST<AST::MultiplyExpression> mul = CalculatorGrammar::get().mul_op;
		BindAST<AST::DivideExpression> div = CalculatorGrammar::get().div_op;
	public:
		const CalculatorGrammar &g = CalculatorGrammar::get();
	};

Invoking the `parse()` method on this class will cause an `AST::Number` class
to be created for every terminal matching the `num` rule in the grammar, and so
on.  Note that this parser is reentrant.  It is safe to use it from multiple
threads to parse different strings.  It is therefore safe to also make the
parser a singleton.

RTTI Usage
----------

ParserLib requires RTTI for one specific purpose: down-casting from `ast_node`
to a subclass (and checking that the result really is of that class).  If you
are using RTTI in the rest of your application, then you can instruct ParserLib
to use RTTI for these casts by defining the `USE_RTTI` macro before including
the ParserLib headers and when building ParserLib.

If you do not wish to depend on RTTI, then ParserLib provides a macro that you
can use in your own AST classes that will provide the required virtual
functions to implement ad-hoc RTTI for this specific use.  You use them like
this:

	class MyASTClass : parserlib::ast_node
	{
		/* Your methods go here. */
		PARSELIB_RTTI(MyASTClass, parserlib::ast_node)
	};

This macro will be compiled away if you do define `USE_RTTI`, so you can
provide grammars built with ParserLib that don't force consumers to use or
not-use RTTI.  It is also completely safe to build without `USE_RTTI`, but
still compile with RTTI.

What is Pegmatite
-----------------

Pegmatite is a very crystalline, intrusive igneous rock composed of
interlocking crystals usually larger than 2.5 cm in size.

It is also a Parsing Expression Grammar library that rocks!

To do
-----

Not in order:

- Write parameterised delegate for generating semantic HTML markup
- Check whether the `USE_RTTI` setting for the headers and library had to match
  it would be nice if it didn't...
