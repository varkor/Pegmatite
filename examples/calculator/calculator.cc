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
class Expression : public ASTContainer
{
public:
	/**
	 * Evaluate this expression.  Returns a `double` representing the result of
	 * the evaluation.
	 */
	virtual double eval() const = 0;
	/**
	 * Print the node, at the specified indent depth.
	 */
	virtual void print(int depth = 0) const = 0;
	PEGMATITE_RTTI(Expression, ASTContainer);
};


/**
 * AST node representing a number.
 */
class Number : public Expression
{
public:
	/**
	 * Construct the numerical value from the text in the input range.
	 */
	virtual void construct(const pegmatite::InputRange &r, pegmatite::ASTStack &st)
	{
		stringstream stream;
		for (char c : r)
		{
			stream << c;
		}
		stream >> value;
	}

	virtual double eval() const
	{
		return value;
	}

	virtual void print(int depth) const
	{
		cout << string(depth, '\t') << value << endl;
	}

private:
	double value;
};


/**
 * Superclass for all of the binary expressions.  Contains pointers to the left
 * and right children.  Subclasses encode the operation type.
 */
class BinaryExpression : public Expression 
{
protected:
	/**
	 * The pointers to the left and right nodes.  The `ASTPtr` class will
	 * automatically fill these in when this node is constructed, popping the
	 * two top values from the AST stack.
	 */
	ASTPtr<Expression> left, right;
public:

	virtual void print(int depth) const
	{
		left->print(depth);
		right->print(depth);
	}
};

/**
 * Add expression node.
 */
class AddExpression : public BinaryExpression
{
public:
	virtual double eval() const
	{
		return left->eval() + right->eval();
	}

	virtual void print(int depth) const
	{
		cout << string(depth, '\t') << "+" << endl;
		BinaryExpression::print(depth+1);
	}
};


/**
 * Subtract expression node.
 */
class SubtractExpression : public BinaryExpression
{
public:
	virtual double eval() const
	{
		return left->eval() - right->eval();
	}

	virtual void print(int depth) const
	{
		cout << string(depth, '\t') << "-" << endl;
		BinaryExpression::print(depth+1);
	}
};

/**
 * Multiply expression node.
 */
class MultiplyExpression : public BinaryExpression
{
public:
	virtual double eval() const
	{
		return left->eval() * right->eval();
	}

	virtual void print(int depth) const
	{
		cout << string(depth, '\t') << "*" << endl;
		BinaryExpression::print(depth+1);
	}
};

/**
 * Divide expression node.
 */
class DivideExpression : public BinaryExpression
{
public:
	virtual double eval() const
	{
		double ret = 0;
		double rval = right->eval();
		if (rval != 0)
		{
			ret = left->eval() / rval;
		}
		return ret;
	}

	virtual void print(int depth) const
	{
		cout << string(depth, '\t') << "/" << endl;
		BinaryExpression::print(depth+1);
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
	private:
	/**
	 * Private constructor.  This class is immutable, and so only the `get()`
	 * method should be used to return the singleton instance.
	 */
	CalculatorGrammar() {};
};



/**
 * CalculatorParser, constructs an AST from an input string.
 */
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

}

int main()
{
	Parser::CalculatorParser p;
	for (;;)
	{
		string s;

		cout << "enter a math expression (+ - * /, floats, parentheses) or enter to exit:\n";
		getline(cin, s);
		if (s.empty()) break;

		//convert the string to input
		StringInput i(std::move(s));

		//parse
		ErrorList el;
		unique_ptr<AST::Expression> root = 0;
		p.parse(i, p.g.expr, p.g.ws, el, root);

		//on success
		if (root)
		{
			double v = root->eval();
			cout << "success\n";
			cout << "result = " << v << endl;
			cout << "parse tree:\n";
			root->print(0);
		}

		//on error
		else
		{
			cout << "errors: \n";
			for (auto &err : el)
			{
				cout << "line " << err.start.line << ", col " << err.finish.col << ": ";
				wcout << "syntax error" << endl;
			}
		}

		//next input
		cout << endl;
	}
	return 0;
}
