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


namespace parserlib {


class parserlib_private;
class parserlib_expr;
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
		Input     *buffer;
		/**
		 * The index into the buffer.
		 */
		Index      idx;
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
	Index     buffer_start;
	/**
	 * The index of the end of the buffer.
	 */
	Index     buffer_end;
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
	virtual Index size() = 0;
	virtual ~Input();
};

/**
 * A concrete input class that wraps a vector of 32-bit characters.
 */
class UnicodeVectorInput : public Input
{
	std::vector<char32_t> vector;
	public:
	UnicodeVectorInput(std::vector<char32_t> &v) : vector(v) {}
	virtual bool  fillBuffer(Index start, Index &length, char32_t *&b)
	{
		if (start > vector.size())
		{
			return false;
		}
		length = vector.size() - start;
		b = vector.data() + start;
		return true;
	}
	virtual Index size()
	{
		return vector.size();
	}
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


/** a grammar expression.
 */
class expr {
public:
    /** character terminal constructor.
        @param c character.
     */
    expr(int c);

    /** null-terminated string terminal constructor.
        @param s null-terminated string.
     */
    expr(const char *s);

    /** null-terminated wide string terminal constructor.
        @param s null-terminated string.
     */
    expr(const wchar_t *s);

    /** rule reference constructor.
        @param r rule.
     */
    expr(rule &r);

    /** creates a zero-or-more loop out of this expression.
        @return a zero-or-more loop expression.
     */
    expr operator *() const;

    /** creates a one-or-more loop out of this expression.
        @return a one-or-more loop expression.
     */
    expr operator +() const;

    /** creates an optional out of this expression.
        @return an optional expression.
     */
    expr operator -() const;

    /** creates an AND-expression.
        @return an AND-expression.
     */
    expr operator &() const;

    /** creates a NOT-expression.
        @return a NOT-expression.
     */
    expr operator !() const;

    void dump();

private:
    //internal expression
    parserlib_expr *m_expr;

    //internal constructor from internal expression
    expr(parserlib_expr *e) : m_expr(e) {}

    //assignment not allowed
    expr &operator = (expr &);

    friend class parserlib_private;
};


/** type of procedure to invoke when a rule is successfully parsed.
    @param b begin position of input.
    @param e end position of input.
    @param d pointer to user data.
 */
typedef void (*parse_proc)(const pos &b, const pos &e, void *d);


///input range.
class input_range {
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
};


///enum with error types.
enum ERROR_TYPE {
    ///syntax error
    ERROR_SYNTAX_ERROR = 1,

    ///invalid end of file
    ERROR_INVALID_EOF,

    ///first user error
    ERROR_USER = 100
};


///error.
class error : public input_range {
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


/** represents a rule.
 */
class rule {
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
    rule(const expr &e);

    /** constructor from rule.
        @param r rule.
     */
    rule(rule &r);

    rule(const rule &r) = delete;

    /** deletes the internal object that represents the expression.
     */
    ~rule();

    /** creates a zero-or-more loop out of this rule.
        @return a zero-or-more loop rule.
     */
    expr operator *();

    /** creates a one-or-more loop out of this rule.
        @return a one-or-more loop rule.
     */
    expr operator +();

    /** creates an optional out of this rule.
        @return an optional rule.
     */
    expr operator -();

    /** creates an AND-expression out of this rule.
        @return an AND-expression out of this rule.
     */
    expr operator &();

    /** creates a NOT-expression out of this rule.
        @return a NOT-expression out of this rule.
     */
    expr operator !();

    /** sets the parse procedure.
        @param p procedure.
     */
    void set_parse_proc(parse_proc p);

    /** get the this ptr (since operator & is overloaded).
        @return pointer to this.
     */
    rule *this_ptr() { return this; }

private:
    //mode
    enum _MODE {
        _PARSE,
        _REJECT,
        _ACCEPT
    };

    //state
    struct _state {
        //position in source code, relative to start
        size_t m_pos;

        //mode
        _MODE m_mode;

        //constructor
        _state(size_t pos = -1, _MODE mode = _PARSE) :
            m_pos(pos), m_mode(mode) {}
    };

    //internal expression
    parserlib_expr *m_expr;

    //associated parse procedure.
    parse_proc m_parse_proc;

    //state
    _state m_state;

    //assignment not allowed
    rule &operator = (rule &);

    friend class parserlib_private;
    friend class parserlib_context;
};


/** creates a sequence of expressions.
    @param left left operand.
    @param right right operand.
    @return an expression which parses a sequence.
 */
expr operator >> (const expr &left, const expr &right);


/** creates a choice of expressions.
    @param left left operand.
    @param right right operand.
    @return an expression which parses a choice.
 */
expr operator | (const expr &left, const expr &right);


/** converts a parser expression into a terminal.
    @param e expression.
    @return an expression which parses a terminal.
 */
expr term(const expr &e);


/** creates a set expression from a null-terminated string.
    @param s null-terminated string with characters of the set.
    @return an expression which parses a single character out of a set.
 */
expr set(const char *s);


/** creates a set expression from a null-terminated wide string.
    @param s null-terminated string with characters of the set.
    @return an expression which parses a single character out of a set.
 */
expr set(const wchar_t *s);


/** creates a range expression.
    @param min min character.
    @param max max character.
    @return an expression which parses a single character out of range.
 */
expr range(int min, int max);


/** creates an expression which increments the line counter
    and resets the column counter when the given expression
    is parsed successfully; used for newline characters.
    @param e expression to wrap into a newline parser.
    @return an expression that handles newlines.
 */
expr nl(const expr &e);


/** creates an expression which tests for the end of input.
    @return an expression that handles the end of input.
 */
expr eof();


/** creates a not expression.
    @param e expression.
    @return the appropriate expression.
 */
expr not_(const expr &e);


/** creates an and expression.
    @param e expression.
    @return the appropriate expression.
 */
expr and_(const expr &e);


/** creates an expression that parses any character.
    @return the appropriate expression.
 */
expr any();


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
template <class T> T &operator << (T &stream, const input_range &ir) {
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
