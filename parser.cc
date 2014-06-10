/*-
 * Copyright (c) 2012, Achilleas Margaritis
 * Copyright (c) 2014, David T. Chisnall
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <stdexcept>
#include <unordered_map>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>

#include "parser.hh"


using namespace pegmatite;

namespace {
//parser state
// FIXME: This class has an uninformative name.
class ParsingState
{
public:
	//position
	ParserPosition position;

	//size of match vector
	size_t matches;

	//constructor
	ParsingState(Context &con);
};



//match
class ParseMatch
{
public:
	//const Rule matched
	const Rule *matched_rule;

	//begin position
	ParserPosition start;

	//end position
	ParserPosition finish;

	//null constructor
	ParseMatch() {}

	//constructor from parameters
	ParseMatch(const Rule *r, const ParserPosition &b, const ParserPosition &e) :
		matched_rule(r),
		start(b),
		finish(e)
	{
	}
};

/**
 * String expression.  Matches a sequence of characters.
 */
class StringExpr : public Expr
{
public:
	/**
	 * Returns a new string expression recognising the specified string.
	 */
	StringExpr(const char *s) : characters(s, s + strlen(s)) {}
	/**
	 * Returns a new string expression recognising the specified string.
	 */
	StringExpr(const char *s, std::size_t length) : characters(s, s + length) {}
	virtual bool parse_non_term(Context &con) const;
	virtual bool parse_term(Context &con) const;
	virtual void dump() const;
private:
	/**
	 * The characters that this expression will match.
	 */
	std::vector<int> characters;
};


}
namespace pegmatite
{

/**
 * Character expression, matches a single character.
 */
class CharacterExpr : public Expr
{
	/**
	 * The character that will be recognised by this expression.
	 */
	int character;
public:
	/**
	 * Constructs a character expression from the specified integer.
	 */
	CharacterExpr(int c) : character(c) {}
	virtual bool parse_non_term(Context &con) const;
	virtual bool parse_term(Context &con) const;
	virtual void dump() const;
	/**
	 * Returns a range expression that recognises characters in the specified
	 * range.
	 */
	ExprPtr operator-(const CharacterExpr &other);
	/**
	 * Returns a range expression that recognises characters in the specified
	 * range.
	 */
	ExprPtr operator-(int other);
};

CharacterExprPtr operator "" _E(const char x)
{
	return CharacterExprPtr(new CharacterExpr(x));
}
ExprPtr operator "" _E(const char *x, std::size_t len)
{
	return ExprPtr(new StringExpr(x, len));
}
ExprPtr operator-(const CharacterExprPtr &left, const CharacterExprPtr &right)
{
	return (*left) - (*right);
}
ExprPtr operator-(const CharacterExprPtr &left, int right)
{
	return (*left) - right;
}

ExprPtr::ExprPtr(const CharacterExprPtr &e) :
	std::shared_ptr<Expr>(std::static_pointer_cast<Expr>(e)) {}

//parsing context
class Context
{
public:
	//match vector
	typedef std::vector<ParseMatch> ParseMatch_vector;

	//const Rule that parses whitespace
	const Rule &whitespace_rule;

	//current position
	ParserPosition position;

	//Error position
	ParserPosition error_pos;

	//input begin
	Input::iterator start;

	//input end
	Input::iterator finish;

	//matches
	ParseMatch_vector matches;

	const ParserDelegate &delegate;

	//constructor
	Context(Input &i, const Rule &ws, const ParserDelegate &d) :
		whitespace_rule(ws),
		position(i),
		error_pos(i),
		start(i.begin()),
		finish(i.end()),
		delegate(d)
	{
	}

	//check if the end is reached
	bool end() const
	{
		return position.it == finish;
	}

	//get the current symbol
	int symbol() const
	{
		assert(!end());
		return *position.it;
	}

	//set the longest possible error
	void set_error_pos()
	{
		if (position.it > error_pos.it)
		{
			error_pos = position;
		}
	}

	//next column
	void next_col()
	{
		++position.it;
		++position.col;
	}

	//next line
	void next_line()
	{
		++position.line;
		position.col = 1;
	}

	//restore the state
	void restore(const ParsingState &st)
	{
		position = st.position;
		matches.resize(st.matches);
	}

	//parse non-term rule.
	bool parse_non_term(const Rule &r);

	//parse term rule.
	bool parse_term(const Rule &r);

	//parse whitespace terminal
	bool parse_ws() { return parse_term(whitespace_rule); }

	parse_proc get_parse_proc(const Rule &r) const
	{
		return delegate.get_parse_proc(r);
	}

	//execute all the parse procs
	void do_parse_procs(void *d) const
	{
		for(ParseMatch_vector::const_iterator it = matches.begin();
			it != matches.end();
			++it)
		{
			const ParseMatch &m = *it;
			parse_proc p = get_parse_proc(*(m.matched_rule));
			p(m.start, m.finish, d);
		}
	}

private:
	//mode
	enum MatchMode
	{
		PARSE,
		REJECT,
		ACCEPT
	};

	//state
	struct RuleState
	{
		//position in source code, relative to start
		size_t position;

		//mode
		MatchMode mode;

		//constructor
		RuleState(size_t ParserPosition = -1, MatchMode mode = PARSE) :
			position(ParserPosition), mode(mode) {}
	};
	//parse non-term rule.
	//parse term rule.
	std::unordered_map<const Rule*, std::vector<RuleState>> rule_states;
	bool parse_rule(const Rule &r, bool (Context::*parse_func)(const Rule &));
	bool _parse_non_term(const Rule &r);

	bool _parse_term(const Rule &r);
};

}

namespace{

//set expression.
class SetExpr : public Expr
{
public:
	//constructor from ansi string.
	SetExpr(const char *s)
	{
		for(; *s; ++s)
		{
			_add(*s);
		}
	}

	//constructor from range.
	SetExpr(int min, int max)
	{
		assert(min >= 0);
		assert(min <= max);
		mSetExpr.resize((size_t)max + 1U);
		for(; min <= max; ++min)
		{
			mSetExpr[(size_t)min] = true;
		}
	}

	//parse with whitespace
	virtual bool parse_non_term(Context &con) const
	{
		return _parse(con);
	}

	//parse terminal
	virtual bool parse_term(Context &con) const
	{
		return _parse(con);
	}

	virtual void dump() const
	{
		fprintf(stderr, "[");
		char c;
		for (bool v : mSetExpr)
		{
			if (v)
				fprintf(stderr, "%c", c);
			c++;
		}
		fprintf(stderr, "]");
	}

private:
	//set is kept as an array of flags, for quick access
	std::vector<bool> mSetExpr;

	//add character
	void _add(size_t i)
	{
		if (i >= mSetExpr.size())
		{
			mSetExpr.resize(i + 1);
		}
		mSetExpr[i] = true;
	}

	//internal parse
	bool _parse(Context &con) const
	{
		if (!con.end())
		{
			size_t ch = con.symbol();
			if (ch < mSetExpr.size() && mSetExpr[ch])
			{
				con.next_col();
				return true;
			}
		}
		con.set_error_pos();
		return false;
	}
};


//base class for unary expressions
class UnaryExpr : public Expr
{
public:
	UnaryExpr(const ExprPtr e) : expr(e) { }
protected:
	const ExprPtr expr;
};


//terminal
class TerminalExpr : public UnaryExpr
{
public:
	//constructor.
	TerminalExpr(const ExprPtr e) :
		UnaryExpr(e)
	{
	}

	//parse with whitespace
	virtual bool parse_non_term(Context &con) const
	{
		return expr->parse_term(con);
	}

	//parse terminal
	virtual bool parse_term(Context &con) const
	{
		return expr->parse_term(con);
	}

	virtual void dump() const
	{
		expr->dump();
	}

};


//loop 0
class Loop0Expr : public UnaryExpr
{
public:
	//constructor.
	Loop0Expr(const ExprPtr e) :
		UnaryExpr(e)
	{
	}

	//parse with whitespace
	virtual bool parse_non_term(Context &con) const
	{
		//if parsing of the first fails, restore the context and stop
		con.parse_ws();
		ParsingState st(con);
		if (!expr->parse_non_term(con))
		{
			con.restore(st);
			return true;
		}

		//parse the rest
		for(;;)
		{
			con.parse_ws();
			ParsingState st(con);
			if (!expr->parse_non_term(con))
			{
				con.restore(st);
				break;
			}
		}

		return true;
	}

	//parse terminal
	virtual bool parse_term(Context &con) const
	{
		//if parsing of the first fails, restore the context and stop
		ParsingState st(con);
		if (!expr->parse_term(con))
		{
			con.restore(st);
			return true;
		}

		//parse the rest until no more parsing is possible
		for(;;)
		{
			ParsingState st(con);
			if (!expr->parse_term(con))
			{
				con.restore(st);
				break;
			}
		}

		return true;
	}
	virtual void dump() const
	{
		fprintf(stderr, "*( ");
		expr->dump();
		fprintf(stderr, " )");
	}
};


//loop 1
class Loop1Expr : public UnaryExpr
{
public:
	Loop1Expr(const ExprPtr e) : UnaryExpr(e) { }

	//parse with whitespace
	virtual bool parse_non_term(Context &con) const
	{
		//parse the first; if the first fails, stop
		con.parse_ws();
		if (!expr->parse_non_term(con)) return false;

		//parse the rest until no more parsing is possible
		for(;;)
		{
			con.parse_ws();
			ParsingState st(con);
			if (!expr->parse_non_term(con))
			{
				con.restore(st);
				break;
			}
		}

		return true;
	}

	//parse terminal
	virtual bool parse_term(Context &con) const
	{
		//parse the first; if the first fails, stop
		if (!expr->parse_term(con)) return false;

		//parse the rest until no more parsing is possible
		for(;;)
		{
			ParsingState st(con);
			if (!expr->parse_term(con))
			{
				con.restore(st);
				break;
			}
		}

		return true;
	}

	virtual void dump() const
	{
		fprintf(stderr, "+( ");
		expr->dump();
		fprintf(stderr, " )");
	}
};


//optional
class OptionalExpr : public UnaryExpr
{
public:
	OptionalExpr(const ExprPtr e) : UnaryExpr(e) { }

	//parse with whitespace
	virtual bool parse_non_term(Context &con) const
	{
		ParsingState st(con);
		if (!expr->parse_non_term(con)) con.restore(st);
		return true;
	}

	//parse terminal
	virtual bool parse_term(Context &con) const
	{
		ParsingState st(con);
		if (!expr->parse_term(con)) con.restore(st);
		return true;
	}

	virtual void dump() const
	{
		fprintf(stderr, "-( ");
		expr->dump();
		fprintf(stderr, " )");
	}
};


//and
//FIXME: What is this?  Parses the subexpression but resets the state
//independent of success or failure?
//THIS IS WHY COMMENTS MATTER!
class AndExpr : public UnaryExpr
{
public:
	AndExpr(const ExprPtr e) : UnaryExpr(e) { }

	//parse with whitespace
	virtual bool parse_non_term(Context &con) const
	{
		ParsingState st(con);
		bool ok = expr->parse_non_term(con);
		con.restore(st);
		return ok;
	}

	//parse terminal
	virtual bool parse_term(Context &con) const
	{
		ParsingState st(con);
		bool ok = expr->parse_term(con);
		con.restore(st);
		return ok;
	}

	virtual void dump() const
	{
		fprintf(stderr, "&( ");
		expr->dump();
		fprintf(stderr, " )");
	}
};


//not
class NotExpr : public UnaryExpr
{
public:
	NotExpr(const ExprPtr e) : UnaryExpr(e) { }

	//parse with whitespace
	virtual bool parse_non_term(Context &con) const
	{
		ParsingState st(con);
		bool ok = !expr->parse_non_term(con);
		con.restore(st);
		return ok;
	}

	//parse terminal
	virtual bool parse_term(Context &con) const
	{
		ParsingState st(con);
		bool ok = !expr->parse_term(con);
		con.restore(st);
		return ok;
	}

	virtual void dump() const
	{
		fprintf(stderr, "!( ");
		expr->dump();
		fprintf(stderr, " )");
	}
};


//newline
class NewlineExpr : public UnaryExpr
{
public:
	NewlineExpr(const ExprPtr e) : UnaryExpr(e) { }

	//parse with whitespace
	virtual bool parse_non_term(Context &con) const
	{
		if (!expr->parse_non_term(con)) return false;
		con.next_line();
		return true;
	}

	//parse terminal
	virtual bool parse_term(Context &con) const
	{
		if (!expr->parse_term(con)) return false;
		con.next_line();
		return true;
	}

	virtual void dump() const
	{
		fprintf(stderr, "nl( ");
		expr->dump();
		fprintf(stderr, " )");
	}
};


//base class for binary expressions
class BinaryExpr : public Expr
{
public:
	BinaryExpr(const ExprPtr &left, const ExprPtr &right) :
		left(left), right(right) { }
protected:
	const ExprPtr left, right;
};


//sequence
class SequenceExpr : public BinaryExpr
{
public:
	SequenceExpr(const ExprPtr left, const ExprPtr right) : BinaryExpr(left, right) {}

	//parse with whitespace
	virtual bool parse_non_term(Context &con) const
	{
		if (!left->parse_non_term(con)) return false;
		con.parse_ws();
		return right->parse_non_term(con);
	}

	//parse terminal
	virtual bool parse_term(Context &con) const
	{
		if (!left->parse_term(con)) return false;
		return right->parse_term(con);
	}

	virtual void dump() const
	{
		left->dump();
		fprintf(stderr, " >> ");
		right->dump();
	}
};


//choice
class ChoiceExpr : public BinaryExpr
{
public:
	ChoiceExpr(const ExprPtr &left, const ExprPtr &right) :
		BinaryExpr(left, right) {}

	virtual bool parse_non_term(Context &con) const
	{
		ParsingState st(con);
		if (left->parse_non_term(con)) return true;
		con.restore(st);
		return right->parse_non_term(con);
	}

	virtual bool parse_term(Context &con) const
	{
		ParsingState st(con);
		if (left->parse_term(con)) return true;
		con.restore(st);
		return right->parse_term(con);
	}

	virtual void dump() const
	{
		left->dump();
		fprintf(stderr, " | ");
		right->dump();
	}
};


//reference to rule
class RuleReferenceExpr : public Expr
{
public:
	//constructor.
	RuleReferenceExpr(const Rule &r) : referenced_rule(r) { }

	//parse with whitespace
	virtual bool parse_non_term(Context &con) const
	{
		return con.parse_non_term(referenced_rule);
	}

	//parse terminal
	virtual bool parse_term(Context &con) const
	{
		return con.parse_term(referenced_rule);
	}

	virtual void dump() const
	{
		fprintf(stderr, "{Reference to rule}");
	}

private:
	//reference
	const Rule &referenced_rule;
};


//eof
class EndOfFileExpr : public Expr
{
public:
	//parse with whitespace
	virtual bool parse_non_term(Context &con) const
	{
		return parse_term(con);
	}

	//parse terminal
	virtual bool parse_term(Context &con) const
	{
		return con.end();
	}

	virtual void dump() const
	{
		fprintf(stderr, "$eof");
	}
};


//AnyExpr
class AnyExpr : public Expr
{
public:
	//parse with whitespace
	virtual bool parse_non_term(Context &con) const
	{
		return parse_term(con);
	}

	//parse terminal
	virtual bool parse_term(Context &con) const
	{
		if (!con.end())
		{
			con.next_col();
			return true;
		}
		con.set_error_pos();
		return false;
	}

	virtual void dump() const
	{
		fprintf(stderr, "$AnyExpr");
	}
};

//constructor
ParsingState::ParsingState(Context &con) :
	position(con.position),
	matches(con.matches.size())
{
}

static inline bool parseString(Context &con,
                               const std::vector<int> &characters)
{
	for (int c : characters)
	{
		if (con.end() || con.symbol() != c)
		{
			con.set_error_pos();
			return false;
		}
		con.next_col();
	}
	return true;
}
bool StringExpr::parse_non_term(Context &con) const
{
	return parseString(con, characters);
}
bool StringExpr::parse_term(Context &con) const
{
	return parseString(con, characters);
}
void StringExpr::dump() const
{
	fprintf(stderr, "\"");
	for (int c : characters)
	{
		fprintf(stderr, "%c", (char)c);
	}
	fprintf(stderr, "\"");
}



}

namespace pegmatite {


//parse non-term rule.
bool Context::parse_non_term(const Rule &r)
{
	return parse_rule(r, &Context::_parse_non_term);
}

bool Context::parse_rule(const Rule &r, bool (Context::*parse_func)(const Rule &))
{
	//save the state of the rule
	auto &states = rule_states[std::addressof(r)];
	size_t state_size = states.size();
	size_t last_pos = -1;
	MatchMode last_mode = PARSE;
	if (state_size > 0)
	{
		auto &last = states.back();
		last_pos = last.position;
		last_mode = last.mode;
	}

	//success/failure result
	bool ok;

	//compute the new position
	size_t new_pos = position.it - start;

	//check if we have left recursion
	bool lr = new_pos == last_pos;

	//update the rule's state
	states.push_back(RuleState(new_pos, last_mode));
	// Note that we have to look this value up in the vector every time that we
	// use it, because the vector will realloc() its internal storage and move
	// the objects around.

	//handle the mode of the rule
	switch (last_mode)
	{
		//normal parse
		case PARSE:
			if (lr)
			{
				//first try to parse the rule by rejecting it, so alternative
				//branches are examined
				states.back().mode = REJECT;
				ok = (this->*parse_func)(r);
				break;
			}
			else
			{
				ok = (this->*parse_func)(r);
			}
			break;
		case REJECT:
		case ACCEPT:
			if (lr)
			{
				ok = false;
			}
			else
			{
				states.back().mode = PARSE;
				ok = (this->*parse_func)(r);
				states.back().mode = last_mode;
			}
			break;
	}

	//restore the rule's state
	assert(state_size < states.size());
	states.resize(state_size);

	return ok;
}



//parse term rule.
bool Context::parse_term(const Rule &r) 
{
	return parse_rule(r, &Context::_parse_term);
}
const bool debug_parsing = false;


//parse non-term rule internal.
bool Context::_parse_non_term(const Rule &r)
{
	bool ok;
	if (get_parse_proc(r))
	{
		ParserPosition b = position;
		ok = r.expr->parse_non_term(*this);
		if (debug_parsing)
		{
			r.expr->dump();
			fprintf(stderr, "\n");
		}
		if (ok)
		{
			matches.push_back(ParseMatch(std::addressof(r), b, position));
		}
	}
	else
	{
		ok = r.expr->parse_non_term(*this);
	}
	return ok;
}


//parse term rule internal.
bool Context::_parse_term(const Rule &r)
{
	bool ok;
	if (get_parse_proc(r))
	{
		ParserPosition b = position;
		ok = r.expr->parse_term(*this);
		if (ok)
		{
			matches.push_back(ParseMatch(std::addressof(r), b, position));
		}
	}
	else
	{
		ok = r.expr->parse_term(*this);
	}
	return ok;
}


//get the next position
static ParserPosition _next_pos(const ParserPosition &p)
{
	ParserPosition r = p;
	++r.it;
	++r.col;
	return r;
}


//get syntax error
static Error _syntax_Error(Context &con)
{
	std::string str = "syntax error: ";
	str += *con.error_pos.it;
	return Error(con.error_pos, _next_pos(con.error_pos), ERROR_SYNTAX_ERROR);
}


//get eof error
static Error _eof_Error(Context &con)
{
	return Error(con.error_pos, con.error_pos, ERROR_INVALID_EOF);
}

char32_t Input::slowCharacterLookup(Index n)
{
	if (n >= size())
	{
		return 0;
	}
	// Optimise backtracking by jumping back 64 characters so subsequent
	// forward scans are fast.
	// TODO: Profile and find out if 64 is a sensible made-up number.
	if (n < buffer_start)
	{
		buffer_start = (n > 64) ? n - 64 : 0;
	}
	else
	{
		buffer_start = n;
	}
	Index length = static_buffer_size;
	buffer = local_buffer;
	if (!fillBuffer(buffer_start, length, buffer))
	{
		buffer_end = 0;
		return 0;
	}
	buffer_end = n + length;
	if ((n >= buffer_start) && (n < buffer_end))
	{
		return buffer[buffer_start - n];
	}
	return 0;
}
Input::~Input() {}
bool  UnicodeVectorInput::fillBuffer(Index start, Index &length, char32_t *&b)
{
	if (start > vector.size())
	{
		return false;
	}
	length = vector.size() - start;
	// The buffer can't be const because this function is allowed to write into
	// it, but the caller guarantees that it will not write to the returned
	// value.
	// We should probably clean up this interface to a const char32_t* instead
	// of a bool...
	b = const_cast<char32_t*>(vector.data() + start);
	return true;
}
Input::Index UnicodeVectorInput::size() const
{
	return vector.size();
}

bool  StringInput::fillBuffer(Index start, Index &length, char32_t *&b)
{
	if (start > str.size())
	{
		return false;
	}
	length = std::min(length, str.size() - start);
	for (Index i=start ; i<length ; i++)
	{
		b[i] = str[i];
	}
	return true;
}
Input::Index StringInput::size() const
{
	return str.size();
}

AsciiFileInput::AsciiFileInput(int file) : fd(file)
{
	struct stat buf;
	if (fstat(fd, &buf) != 0)
	{
		file_size = 0;
		perror("Input error");
	}
	else
	{
		file_size = buf.st_size;
	}
}

const std::size_t Input::static_buffer_size;
bool AsciiFileInput::fillBuffer(Index start, Index &length, char32_t *&b)
{
	if (start > file_size)
	{
		return false;
	}
	char buffer[static_buffer_size];
	// This should be a no-op
	length = std::min(length, static_buffer_size);
	length = std::min(length, file_size - start);
	pread(fd, buffer, length, start);
	for (Index i=0 ; i<length ; i++)
	{
		b[i] = buffer[i];
	}
	return true;
}
Input::Index AsciiFileInput::size() const
{
	return file_size;
}

/** constructor from input.
	@param i input.
 */
ParserPosition::ParserPosition(Input &i) :
	it(i.begin()),
	line(1),
	col(1)
{
}

/** creates a zero-or-more loop out of this expression.
	@return a zero-or-more loop expression.
 */
ExprPtr operator *(const ExprPtr &e)
{
	return ExprPtr(new Loop0Expr(e));
}


/** creates a one-or-more loop out of this expression.
	@return a one-or-more loop expression.
 */
ExprPtr operator +(const ExprPtr &e)
{
	return ExprPtr(new Loop1Expr(e));
}


/** creates an optional out of e expression.
	@return an optional expression.
 */
ExprPtr operator -(const ExprPtr &e)
{
	return ExprPtr(new OptionalExpr(e));
}


/** creates an AND-expression.
	@return an AND-expression.
 */
ExprPtr operator &(const ExprPtr &e)
{
	return ExprPtr(new AndExpr(e));
}


/** creates a NOT-expression.
	@return a NOT-expression.
 */
ExprPtr operator !(const ExprPtr &e)
{
	return ExprPtr(new NotExpr(e));
}


/** constructor.
	@param b begin position.
	@param e end position.
 */
InputRange::InputRange(const ParserPosition &b, const ParserPosition &e) : start(b), finish(e) { }


/** constructor.
	@param b begin position.
	@param e end position.
	@param t error type.
 */
Error::Error(const ParserPosition &b, const ParserPosition &e, int t) :
	InputRange(b, e), error_type(t) { }


/** compare on begin position.
	@param e the other error to compare this with.
	@return true if this comes before the previous error, false otherwise.
 */
bool Error::operator < (const Error &e) const
{
	return start.it < e.start.it;
}

const Rule::Rule(const ExprPtr e) :
	expr(e)
{
}


/** constructor from rule.
	@param r rule.
 */
ExprPtr::ExprPtr(const char *s) : std::shared_ptr<Expr>(new StringExpr(s)) {};
ExprPtr::ExprPtr(const char s) : std::shared_ptr<Expr>(new CharacterExpr(s)) {};
ExprPtr::ExprPtr(const Rule &r) : std::shared_ptr<Expr>(new RuleReferenceExpr(r)) {};


/** creates a sequence of expressions.
	@param left left operand.
	@param right right operand.
	@return an expression which parses a sequence.
 */
ExprPtr operator >> (const ExprPtr &left, const ExprPtr &right)
{
	return ExprPtr(new SequenceExpr(left, right));
}


/** creates a choice of expressions.
	@param left left operand.
	@param right right operand.
	@return an expression which parses a choice.
 */
ExprPtr operator | (const ExprPtr &left, const ExprPtr &right)
{
	return ExprPtr(new ChoiceExpr(left, right));
}


/** converts a parser expression into a terminal.
	@param e expression.
	@return an expression which parses a terminal.
 */
ExprPtr term(const ExprPtr &e)
{
	return ExprPtr(new TerminalExpr(e));
}


/** creates a set expression from a null-terminated string.
	@param s null-terminated string with characters of the set.
	@return an expression which parses a single character out of a set.
 */
ExprPtr set(const char *s)
{
	return ExprPtr(new SetExpr(s));
}


/** creates a range expression.
	@param min min character.
	@param max max character.
	@return an expression which parses a single character out of range.
 */
ExprPtr range(int min, int max)
{
	return ExprPtr(new SetExpr(min, max));
}


/** creates an expression which increments the line counter
	and resets the column counter when the given expression
	is parsed successfully; used for newline characters.
	@param e expression to wrap into a newline parser.
	@return an expression that handles newlines.
 */
ExprPtr nl(const ExprPtr &e)
{
	return ExprPtr(new NewlineExpr(e));
}


/** creates an expression which tests for the end of input.
	@return an expression that handles the end of input.
 */
ExprPtr eof()
{
	return ExprPtr(new EndOfFileExpr());
}



/** creates an expression that parses any character.
	@return the appropriate expression.
 */
ExprPtr any()
{
	return ExprPtr(new AnyExpr());
}


/** parses the given input.
	The parse procedures of each rule parsed are executed
	before this function returns, if parsing succeeds.
	@param i input.
	@param g root rule of grammar.
	@param ws whitespace rule.
	@param el list of errors.
	@param d user data, passed to the parse procedures.
	@return true on parsing success, false on failure.
 */
bool parse(Input &i, const Rule &g, const Rule &ws, ErrorList &el,
           const ParserDelegate &delegate, void *d)
{
	//prepare context
	Context con(i, ws, delegate);

	//parse initial whitespace
	con.parse_term(con.whitespace_rule);

	//parse grammar
	if (!con.parse_non_term(g))
	{
		el.push_back(_syntax_Error(con));
		return false;
	}

	//parse whitespace at the end
	con.parse_term(con.whitespace_rule);

	//if end is not reached, there was an error
	if (!con.end())
	{
		if (con.error_pos.it < con.finish)
		{
			el.push_back(_syntax_Error(con));
		}
		else
		{
			el.push_back(_eof_Error(con));
		}
		return false;
	}

	//success; execute the parse procedures
	con.do_parse_procs(d);
	return true;
}

ParserDelegate::~ParserDelegate() {}

static inline bool parseCharacter(Context &con, int character)
{
	if (!con.end())
	{
		int ch = con.symbol();
		if (ch == character)
		{
			con.next_col();
			return true;
		}
	}
	con.set_error_pos();
	return false;
}
bool CharacterExpr::parse_non_term(Context &con) const
{
	return parseCharacter(con, character);
}

bool CharacterExpr::parse_term(Context &con) const
{
	return parseCharacter(con, character);
}
void CharacterExpr::dump() const
{
	fprintf(stderr, "'%c'", (char)character);
}

ExprPtr CharacterExpr::operator-(const CharacterExpr &other)
{
	return range(character, other.character);
}
ExprPtr CharacterExpr::operator-(int other)
{
	return range(character, other);
}



} //namespace pegmatite

