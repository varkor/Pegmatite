#include <iostream>
#include <sstream>
#include <string>
#include "pegmatite.hh"
// This is very bad style, but it's okay for a short example...
using namespace std;
using namespace pegmatite;


/**
 * The AST namespace contains the abstract syntax tree for this grammar.  
 */
namespace AST
{
/**
 * The base class for expressions in our language.
 */
template<typename T>
class Expression : public ASTContainer
{
public:
	/**
	 * Evaluate this expression.  Returns a `double` representing the result of
	 * the evaluation.
	 */
	virtual T eval() const = 0;
	/**
	 * Print the node, at the specified indent depth.
	 */
	virtual void print(size_t depth = 0) const = 0;
	PEGMATITE_RTTI(Expression, ASTContainer)
};


/**
 * AST node representing a number.
 */
template<typename T>
class Number : public Expression<T>
{
	/**
	 * The parsed value for this number.
	 */
	T value;
public:
	/**
	 * Construct the numerical value from the text in the input range.
	 */
	void construct(const pegmatite::InputRange &r, pegmatite::ASTStack &) override
	{
		pegmatite::constructValue(r, value);
	}

	T eval() const override
	{
		return value;
	}

	void print(size_t depth) const override
	{
		cout << string(depth, '\t') << value << endl;
	}
};


/**
 * Superclass for all of the binary expressions.  Contains pointers to the left
 * and right children.  Subclasses encode the operation type.
 */
template<typename T, class Func, char op>
class BinaryExpression : public Expression<T>
{
	/**
	 * The pointers to the left and right nodes.  The `ASTPtr` class will
	 * automatically fill these in when this node is constructed, popping the
	 * two top values from the AST stack.
	 */
	ASTPtr<Expression<T>> left, right;
public:
	T eval() const override
	{
		Func f;
		return f(left->eval(), right->eval());
	}

	void print(size_t depth=0) const override
	{
		cout << string(depth, '\t') << op << endl;
		left->print(depth+1);
		right->print(depth+1);
	}
};

}

namespace Parser
{
/**
 * The (singleton) calculator grammar.
 */
struct CalculatorGrammar
{
	/**
	 * Only spaces are recognised as whitespace in this toy example.
	 */
	Rule ws     = " \t\n"_E;
	/**
	 * Digits are things in the range 0-9.
	 */
	Rule digits  = "[0-9]+"_R;
	/**
	 * Numbers are one or more digits, optionally followed by a decimal point,
	 * and one or more digits, optionally followed by an exponent (which may
	 * also be negative.
	 */
	Rule num    = digits >> -('.'_E >> digits >> -("eE"_S >> -("+-"_S) >> digits));
	/**
	 * Values are either numbers or expressions in brackets (highest precedence).
	 */
	Rule val    = num |  '(' >> expr >> ')';
	/**
	 * Multiply operations are values or multiply, or divide operations,
	 * followed by a multiply symbol, followed by a values, or multiply or
	 * divide operations.  The sides can never be add or subtract operations,
	 * because they have lower precedence and so can only be parents of
	 * multiply or divide operations (or children via parenthetical
	 * expressions), not direct children.
	 */
	Rule mul_op = mul >> '*' >> mul;
	/**
	 * Divide operations follow the same syntax as multiply.
	 */
	Rule div_op = mul >> '/' >> mul;
	/**
	 * Multiply-precedence operations are either multiply or divide operations,
	 * or simple values (numbers of parenthetical expressions).
	 */
	Rule mul    = mul_op | div_op | val;
	/**
	 * Add operations can have any expression on the left (including other add
	 * expressions), but only higher-precedence operations on the right.
	 */
	Rule add_op = expr >> '+' >> expr;
	/**
	 * Subtract operations follow the same structure as add.
	 */
	Rule sub_op = expr >> '-' >> expr;
	/**
	 * Expressions can be any of the other types.
	 */
	Rule expr   = add_op | sub_op | mul;

	/**
	 * Returns a singleton instance of this grammar.
	 */
	static const CalculatorGrammar& get()
	{
		static CalculatorGrammar g;
		return g;
	}
	protected:
	/**
	 * Private constructor.  This class is immutable, and so only the `get()`
	 * method should be used to return the singleton instance.
	 */
	CalculatorGrammar() {}
};



/**
 * CalculatorParser, constructs an AST from an input string.
 */
template<class T>
struct CalculatorParser : public ASTParserDelegate
{
	const CalculatorGrammar &g = CalculatorGrammar::get();

	BindAST<AST::Number<T>> num = g.num;
	BindAST<AST::BinaryExpression<T,plus<T>,'+'>> add = g.add_op;
	BindAST<AST::BinaryExpression<T,minus<T>,'-'>> sub = g.sub_op;
	BindAST<AST::BinaryExpression<T,multiplies<T>,'*'>> mul = g.mul_op;
	BindAST<AST::BinaryExpression<T,divides<T>,'/'>> div = g.div_op;
};


/**
 * Extend the calculator grammar to provide operations that are only applicable
 * to integers.
 */
struct IntCalculatorGrammar : public CalculatorGrammar
{
	/**
	 * A new rule that parses 
	 */
	Rule mod_op = mul >> '%' >> mul;
	/**
	 * Return the singleton instance of this grammar.
	 */
	static const IntCalculatorGrammar& get()
	{
		static IntCalculatorGrammar g;
		return g;
	}
	private:
	/**
	 * Replace rules in the CalculatorGrammar that we need.
	 */
	IntCalculatorGrammar() : CalculatorGrammar()
	{
		// The `mul` rule should now use the mod operation.
		mul    = mul_op | div_op | mod_op | val;
		// In the integer version, we don't support 
		num    = digits >> -("eE"_S >> -("+-"_S) >> digits);
	}
};

/**
 * Parser for the integer version.
 */
struct IntCalculatorParser : public ASTParserDelegate
{
	typedef long long T;
	const IntCalculatorGrammar &g = IntCalculatorGrammar::get();

	BindAST<AST::Number<T>> num = g.num;
	BindAST<AST::BinaryExpression<T,plus<T>,'+'>> add = g.add_op;
	BindAST<AST::BinaryExpression<T,minus<T>,'-'>> sub = g.sub_op;
	BindAST<AST::BinaryExpression<T,multiplies<T>,'*'>> mul = g.mul_op;
	BindAST<AST::BinaryExpression<T,divides<T>,'/'>> div = g.div_op;
	BindAST<AST::BinaryExpression<T,modulus<T>,'%'>> mod = g.mod_op;
};

}

template<class Parser, class Value>
void runCalculator(const char *ops)
{
	Parser p;
	string s;
	// Report errors.
	ErrorReporter er = [](const InputRange &ir, const string &str)
	{
		cout << "line " << ir.start.line << ", col " << ir.finish.col << ": " << str << endl;
	};
	// Loop until the user gives us an empty line.
	for (;;)
	{
		// Read a line from the user.
		cout << "Enter an expression " << ops << " or enter to exit:\n";
		getline(cin, s);
		if (s.empty()) break;

		// Create an input that wraps the string.
		StringInput i(move(s));

		// Parse the input
		unique_ptr<AST::Expression<Value>> root = 0;
		if (p.parse(i, p.g.expr, p.g.ws, er, root))
		{
			// If we got an AST, print the result and the AST
			double v = root->eval();
			cout << "success\n";
			cout << "result = " << v << endl;
			cout << "parse tree:\n";
			root->print();
			cout << endl;
		}
	}
}

int main()
{
	cout << "Use floating point version?" << endl;
	string l;
	getline(cin, l);
	// If the user answers y/Y, use the floating point version, otherwise use
	// the integer version.
	if (l.size() < 1 || (l[0] != 'y' && l[0] != 'Y'))
	{
		runCalculator<Parser::IntCalculatorParser, long long>("(+ - * / %, integers, parentheses)");
	}
	else
	{
		runCalculator<Parser::CalculatorParser<double>, double>("(+ - * /, floats, parentheses)");
	}
	return 0;
}
