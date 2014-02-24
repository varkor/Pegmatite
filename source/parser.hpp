/*-
 * Copyright (c) 2012, Achilleas Margaritis
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
#ifndef PARSER_HPP
#define PARSER_HPP


#include <vector>
#include <string>
#include <list>
#include <functional>


namespace parserlib {


class Expr;
class parserlib_context;
class rule;


/**
 * Abstract superclass for indexing into a buffer with arbitrary storage.
 * The class holds a private buffer of characters and requests that the
 * subclass fill it in, or provide direct access to the underlying storage if
 * it is in the correct format.
 */
class Input
{
	public:
	/**
	 * The type of indexes into the buffer.
	 */
	typedef size_t Index;
	/**
	 * Iterator, refers back into the input stream.
	 */
	class iterator
	{
		friend Input;
		/**
		 * The buffer that this iterator refers to.
		 */
		Input	 *buffer;
		/**
		 * The index into the buffer.
		 */
		Index	  idx;
		inline iterator(Input *b, Index i) : buffer(b), idx(i) {}
		public:
		inline iterator() : buffer(0), idx(-1) {}
		inline char32_t  operator*() const { return (*buffer)[idx]; }
		inline iterator &operator++()
		{
			idx++;
			return *this;
		}
		inline iterator &operator--()
		{
			idx--;
			return *this;
		}
		inline bool operator==(const iterator &other) const
		{
			return (buffer == other.buffer) && (idx == other.idx);
		}
		inline bool operator!=(const iterator &other) const
		{
			return !(*this == other);
		}
		inline bool operator>(const iterator &other) const
		{
			return (idx > other.idx);
		}
		inline bool operator<(const iterator &other) const
		{
			return (idx < other.idx);
		}
		inline Index operator-(const iterator &other) const
		{
			return idx-other.idx;
		}
	};
	inline iterator begin()
	{
		return iterator(this, 0);
	}
	inline iterator end()
	{
		return iterator(this, size());
	}
	/**
	 * Fetch the character at the specified index.  This is intended to be
	 * inlined and returns the character from the cached buffer if possible,
	 * falling back to the (non-inlined) slow path if not.
	 */
	inline char32_t operator[](Index n)
	{
		// If the local buffer can satisfy the request, fetch the value
		char32_t v;
		if ((n >= buffer_start) && (n < buffer_end))
		{
			return buffer[n - buffer_start];
		}
		return slowCharacterLookup(n);
	}
	/**
	 * Default constructor, sets the buffer start to be after the buffer end,
	 * so that the first request will trigger a fetch from the underlying
	 * storage.
	 */
	Input() : buffer_start(1), buffer_end(0), buffer(0) {}
	private:
	/**
	 * A pointer to the start of the buffer.  This must be a contiguous block
	 * of memory, storing 32-bit characters.
	 */
	char32_t *buffer;
	/**
	 * The index of the start of the buffer.
	 */
	Index	 buffer_start;
	/**
	 * The index of the end of the buffer.
	 */
	Index	 buffer_end;
	static const std::size_t static_buffer_size = 128;
	/**
	 * A buffer that can be used to store characters by subclasses that do not
	 * have the same underlying representation.
	 */
	char32_t  local_buffer[static_buffer_size];
	char32_t  slowCharacterLookup(Index n);
	protected:
	/**
	 * Fill in the buffer with the next range.  This is called when the current
	 * cached buffer does not contain the range.  The function returns true if
	 * it can provide at least one character from the index specified by start.
	 * The length of the buffer passed in via the last parameter is provided as
	 * the second argument.
	 *
	 * Implementations of this function may either fill in the provided buffer,
	 * reducing the value passed by the second parameter if there are not
	 * enough characters available to satisfy it, or set the third parameter to
	 * refer to their underlying storage.
	 */
	virtual bool  fillBuffer(Index start, Index &length, char32_t *&b) = 0;
	/**
	 * Returns the size of the buffer.
	 */
	virtual Index size() const = 0;
	virtual ~Input();
};

/**
 * A concrete input class that wraps a vector of 32-bit characters.
 */
class UnicodeVectorInput : public Input
{
	/**
	 * A reference to the vector being used as input.
	 */
	std::shared_ptr<std::vector<char32_t>> &vector;
	public:
	/**
	 * Constructs the wrapper referencing the underlying vector.  The vector
	 * should not be modified for the lifetime of this object, and this object
	 * must outlive any AST nodes created by parsing it.
	 */
	UnicodeVectorInput(std::shared_ptr<std::vector<char32_t>> &v) : vector(v) {}
	/**
	 * Provides direct access to the underlying vector's storage.
	 */
	virtual bool  fillBuffer(Index start, Index &length, char32_t *&b);
	/**
	 * Returns the size of the vector.
	 */
	virtual Index size() const;
};

class AsciiFile : public Input
{
};


///position into the input.
struct pos
{
	///iterator into the input.
	Input::iterator it;

	///line.
	int line;

	///column.
	int col;

	///null constructor.
	pos() {}

	/** constructor from input.
		@param i input.
	 */
	pos(Input &i);
};



/** type of procedure to invoke when a rule is successfully parsed.
	@param b begin position of input.
	@param e end position of input.
	@param d pointer to user data.
 */
typedef std::function<void(const pos&, const pos&, void*)> parse_proc;


///input range.
class input_range
{
public:
	///begin position.
	pos m_begin;

	///end position.
	pos m_end;

	///empty constructor.
	input_range() {}

	/** constructor.
		@param b begin position.
		@param e end position.
	 */
	input_range(const pos &b, const pos &e);
	Input::iterator begin() const { return m_begin.it; };
	Input::iterator end() const { return m_end.it; };
};


///enum with error types.
enum ERROR_TYPE
{
	///syntax error
	ERROR_SYNTAX_ERROR = 1,

	///invalid end of file
	ERROR_INVALID_EOF,

	///first user error
	ERROR_USER = 100
};


///error.
class error : public input_range
{
public:
	///type
	int m_type;

	/** constructor.
		@param b begin position.
		@param e end position.
		@param t type.
	 */
	error(const pos &b, const pos &e, int t);

	/** compare on begin position.
		@param e the other error to compare this with.
		@return true if this comes before the previous error, false otherwise.
	 */
	bool operator < (const error &e) const;
};


///type of error list.
typedef std::list<error> error_list;

class CharacterExpr;
class StringExpr;
typedef std::shared_ptr<CharacterExpr> CharacterExprPtr;
typedef std::shared_ptr<StringExpr> StringExprPtr;

/**
 * A shared pointer to an expression.  All expression tree nodes use shared
 * pointers.  
 */
struct ExprPtr : public std::shared_ptr<Expr>
{
	ExprPtr(Expr *e) : std::shared_ptr<Expr>(e) {}
	ExprPtr(rule &e);
	ExprPtr(const CharacterExprPtr &e) :
		std::shared_ptr<Expr>(std::static_pointer_cast<Expr>(e)) {}
	ExprPtr(const StringExprPtr &e) :
		std::shared_ptr<Expr>(std::static_pointer_cast<Expr>(e)) {}
	ExprPtr(const char *);
	ExprPtr(const char);
};


/** represents a rule.
 */
class rule
{
public:
	/** character terminal constructor.
		@param c character.
	 */
	rule(int c);

	/** null-terminated string terminal constructor.
		@param s null-terminated string.
	 */
	rule(const char *s);

	/** null-terminated wide string terminal constructor.
		@param s null-terminated string.
	 */
	rule(const wchar_t *s);

	/** constructor from expression.
		@param e expression.
	 */
	rule(const ExprPtr e);

	/** constructor from rule.
		@param r rule.
	 */
	rule(rule &r);

	rule(const rule &r) = delete;
	rule(const rule &&r);

	/** sets the parse procedure.
		@param p procedure.
	 */
	void set_parse_proc(parse_proc p);

private:

	//internal expression
	const ExprPtr expr;

	//assignment not allowed
	rule &operator = (rule &) = delete;

	friend class parserlib_context;
};

/**
 * Abstract base class for expressions.
 */
class Expr
{
public:
	/**
	 * Virtual destructor for safe overloading.  Note that generally expression
	 * objects are not meant to be deallocated, as they can be used by multiple
	 * parsers. 
	 */
	virtual ~Expr() { }

	/**
	 * Parse this expression as a non-terminal.  Non-terminals are permitted to
	 * contain whitespace around compound expressions, as defined by the
	 * whitespace rule in the context.
	 */
	virtual bool parse_non_term(parserlib_context &con) const = 0;

	/**
	 * Parse this expression as a terminal.  Terminals are exact matches for
	 * the specified expression, without any whitespace.
	 */
	virtual bool parse_term(parserlib_context &con) const = 0;

	/**
	 * Dump the current rule.  Used for debugging.
	 */
	virtual void dump() const = 0;

};
/** creates a zero-or-more loop out of this expression.
	@return a zero-or-more loop expression.
 */
ExprPtr operator *(const ExprPtr &e);
inline ExprPtr operator *(rule &r)
{
	return *ExprPtr(r);
}

/** creates a one-or-more loop out of this expression.
	@return a one-or-more loop expression.
 */
ExprPtr operator +(const ExprPtr &e);
inline ExprPtr operator +(rule &r)
{
	return +ExprPtr(r);
}

/** creates an optional out of this expression.
	@return an optional expression.
 */
ExprPtr operator -(const ExprPtr &e);
inline ExprPtr operator -(rule &r)
{
	return -ExprPtr(r);
}

/** creates an AND-expression.
	@return an AND-expression.
 */
ExprPtr operator &(const ExprPtr &e);
inline ExprPtr operator &(rule &r)
{
	return &ExprPtr(r);
}

/** creates a NOT-expression.
	@return a NOT-expression.
 */
ExprPtr operator !(const ExprPtr &e);
inline ExprPtr operator !(rule &r)
{
	return !ExprPtr(r);
}


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
	CharacterExpr(int c) : character(c) {}
	virtual bool parse_non_term(parserlib_context &con) const;
	virtual bool parse_term(parserlib_context &con) const;
	virtual void dump() const;
	/**
	 * Returns a range expression that recognises characters in the specified
	 * range.
	 */
	ExprPtr operator-(const CharacterExpr &other);
	ExprPtr operator-(int other);
};

/**
 * String expression.  Matches a sequence of characters.
 */
class StringExpr : public Expr
{
public:
	StringExpr(const char *s) : characters(s, s + strlen(s)) {}
	StringExpr(const char *s, std::size_t length) : characters(s, s + length) {}
	virtual bool parse_non_term(parserlib_context &con) const;
	virtual bool parse_term(parserlib_context &con) const;
	virtual void dump() const;
private:
	/**
	 * The characters that this expression will match.
	 */
	std::vector<int> characters;
};
inline CharacterExprPtr operator "" _E(const char x)
{
	return CharacterExprPtr(new CharacterExpr(x));
}
inline StringExprPtr operator "" _E(const char *x, std::size_t len)
{
	return StringExprPtr(new StringExpr(x, len));
}
inline ExprPtr operator-(const CharacterExprPtr &left, const CharacterExprPtr &right)
{
	return (*left) - (*right);
}
inline ExprPtr operator-(const CharacterExprPtr &left, int right)
{
	return (*left) - right;
}




/** creates a sequence of expressions.
	@param left left operand.
	@param right right operand.
	@return an expression which parses a sequence.
 */
ExprPtr operator >> (const ExprPtr &left, const ExprPtr &right);
inline ExprPtr operator >> (rule &left, const ExprPtr &right)
{
	return ExprPtr(left) >> right;
}


/** creates a choice of expressions.
	@param left left operand.
	@param right right operand.
	@return an expression which parses a choice.
 */
ExprPtr operator | (const ExprPtr &left, const ExprPtr &right);
inline ExprPtr operator | (rule &left, const ExprPtr &right)
{
	return ExprPtr(left) | right;
}


/** converts a parser expression into a terminal.
	@param e expression.
	@return an expression which parses a terminal.
 */
ExprPtr term(const ExprPtr &e);


/** creates a set expression from a null-terminated string.
	@param s null-terminated string with characters of the set.
	@return an expression which parses a single character out of a set.
 */
ExprPtr set(const char *s);


/** creates a set expression from a null-terminated wide string.
	@param s null-terminated string with characters of the set.
	@return an expression which parses a single character out of a set.
 */
ExprPtr set(const wchar_t *s);


/** creates a range expression.
	@param min min character.
	@param max max character.
	@return an expression which parses a single character out of range.
 */
ExprPtr range(int min, int max);


/** creates an expression which increments the line counter
	and resets the column counter when the given expression
	is parsed successfully; used for newline characters.
	@param e expression to wrap into a newline parser.
	@return an expression that handles newlines.
 */
ExprPtr nl(const ExprPtr &e);


/** creates an expression which tests for the end of input.
	@return an expression that handles the end of input.
 */
ExprPtr eof();


/** creates an expression that parses any character.
	@return the appropriate expression.
 */
ExprPtr any();


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
bool parse(Input &i, rule &g, rule &ws, error_list &el, void *d);


/** output the specific input range to the specific stream.
	@param stream stream.
	@param ir input range.
	@return the stream.
 */
template <class T> T &operator << (T &stream, const input_range &ir)
{
	for(Input::iterator it = ir.m_begin.it;
		it != ir.m_end.it;
		++it)
	{
		stream << (typename T::char_type)*it;
	}
	return stream;
}


} //namespace parserlib


#endif //PARSER_HPP
