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
#include "parser.hpp"


namespace parserlib {

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
	if (start > vector->size())
	{
		return false;
	}
	length = vector->size() - start;
	b = vector->data() + start;
	return true;
}
Input::Index UnicodeVectorInput::size() const
{
	return vector->size();
}

//internal map from rules to parse procs
typedef std::unordered_map<rule *, parse_proc> parse_proc_map_t;


//the one and only parse proc map.
// FIXME: There should be one of these per delegate!
static parse_proc_map_t *getParseProcMap()
{
	static parse_proc_map_t parse_proc_map;
	return &parse_proc_map;
}

//get the parse proc from the map
static parse_proc _get_parse_proc(rule *r)
{
	parse_proc_map_t *map = getParseProcMap();
	parse_proc_map_t::iterator it = map->find(r);
	if (it == map->end()) return 0;
	return it->second;
}


//internal private class that manages access to the public classes' internals.
class parserlib_private {
public:
    //get the internal expression object from the expression.
    static parserlib_expr *get_expr(const expr &e) {
        return e.m_expr;
    }

    //create new expression from given expression
    static expr construct_expr(parserlib_expr *e) {
        return e;
    }

    //get the internal expression object from the rule.
    static parserlib_expr *get_expr(rule &r) {
        return r.m_expr;
    }

    //get the internal parse proc from the rule.
    static parse_proc get_parse_proc(rule &r) {
        return r.m_parse_proc;
    }
};


class parserlib_context;


//parser state
class _state {
public:
    //position
    pos m_pos;

    //size of match vector
    size_t m_matches;

    //constructor
    _state(parserlib_context &con);
};


//match
class _match {
public:
    //rule matched
    rule *m_rule;

    //begin position
    pos m_begin;

    //end position
    pos m_end;

    //null constructor
    _match() {}

    //constructor from parameters
    _match(rule *r, const pos &b, const pos &e) :
        m_rule(r),
        m_begin(b),
        m_end(e)
    {
    }
};


//match vector
typedef std::vector<_match> _match_vector;


//parsing context
class parserlib_context {
public:
    //rule that parses whitespace
    rule &m_ws;

    //current position
    pos m_pos;

    //error position
    pos m_error_pos;

    //input begin
    Input::iterator m_begin;

    //input end
    Input::iterator m_end;

    //matches
    _match_vector m_matches;
	bool unwinding;

    //constructor
    parserlib_context(Input &i, rule &ws) :
        m_ws(ws),
        m_pos(i),
        m_error_pos(i),
        m_begin(i.begin()),
        m_end(i.end()),
		unwinding(false)
    {
    }

    //check if the end is reached
    bool end() const {
        return m_pos.it == m_end;
    }

    //get the current symbol
    int symbol() const {
        assert(!end());
        return *m_pos.it;
    }

    //set the longest possible error
    void set_error_pos() {
        if (m_pos.it > m_error_pos.it) {
            m_error_pos = m_pos;
        }
    }

    //next column
    void next_col() {
        ++m_pos.it;
        ++m_pos.col;
    }

    //next line
    void next_line() {
        ++m_pos.line;
        m_pos.col = 1;
    }

    //restore the state
    void restore(const _state &st) {
        m_pos = st.m_pos;
        m_matches.resize(st.m_matches);
    }

    //parse non-term rule.
    bool parse_non_term(rule &r);

    //parse term rule.
    bool parse_term(rule &r);

    //parse whitespace terminal
    bool parse_ws() { return parse_term(m_ws); }

    //execute all the parse procs
    void do_parse_procs(void *d) const {
        for(_match_vector::const_iterator it = m_matches.begin();
            it != m_matches.end();
            ++it)
        {
            const _match &m = *it;
            parse_proc p = parserlib_private::get_parse_proc(*m.m_rule);
            p(m.m_begin, m.m_end, d);
        }
    }

private:
	rule *unwind_target;
	bool parse_rule(rule &r, bool (parserlib_context::*parse_func)(rule &));
    //parse non-term rule.
    bool _parse_non_term(rule &r);

    //parse term rule.
    bool _parse_term(rule &r);
};


//base class for expressions
class parserlib_expr {
public:
	//destructor.
	virtual ~parserlib_expr() { }

	//parse with whitespace
	virtual bool parse_non_term(parserlib_context &con) const = 0;

	//parse terminal
	virtual bool parse_term(parserlib_context &con) const = 0;

	virtual void dump() const = 0;
};

void expr::dump() const
{
	m_expr->dump();
}

//single character expression.
class _char : public parserlib_expr {
public:
    //constructor.
    _char(int c) :
        m_char(c)
    {
    }

    //parse with whitespace
    virtual bool parse_non_term(parserlib_context &con) const {
        return _parse(con);
    }

    //parse terminal
    virtual bool parse_term(parserlib_context &con) const {
        return _parse(con);
    }
    virtual void dump() const
	{
		fprintf(stderr, "'%c'", (char)m_char);
	}

private:
    //character
    int m_char;

    //internal parse
    bool _parse(parserlib_context &con) const {
        if (!con.end()) {
            int ch = con.symbol();
            if (ch == m_char) {
                con.next_col();
                return true;
            }
        }
        con.set_error_pos();
        return false;
    }
};


//string expression.
class _string : public parserlib_expr {
public:
    //constructor from ansi string.
    _string(const char *s) :
        m_string(s, s + strlen(s))
    {
    }

    //constructor from wide string.
    _string(const wchar_t *s) :
        m_string(s, s + wcslen(s))
    {
    }

    //parse with whitespace
    virtual bool parse_non_term(parserlib_context &con) const {
        return _parse(con);
    }

    //parse terminal
    virtual bool parse_term(parserlib_context &con) const {
        return _parse(con);
    }

private:
    //string
    std::vector<int> m_string;

    //parse the string
    bool _parse(parserlib_context &con) const {
        for(std::vector<int>::const_iterator it = m_string.begin(),
            end = m_string.end();;)
        {
            if (it == end) return true;
            if (con.end()) break;
            if (con.symbol() != *it) break;
            ++it;
            con.next_col();
        }
        con.set_error_pos();
        return false;
    }

	virtual void dump() const
	{
		fprintf(stderr, "\"");
		for (int c : m_string)
		{
			fprintf(stderr, "%c", (char)c);
		}
		fprintf(stderr, "\"");
	}
};


//set expression.
class _set : public parserlib_expr {
public:
    //constructor from ansi string.
    _set(const char *s) {
        for(; *s; ++s) {
            _add(*s);
        }
    }

    //constructor from wide string.
    _set(const wchar_t *s) {
        for(; *s; ++s) {
            _add(*s);
        }
    }

    //constructor from range.
    _set(int min, int max) {
        assert(min >= 0);
        assert(min <= max);
        m_set.resize((size_t)max + 1U);
        for(; min <= max; ++min) {
            m_set[(size_t)min] = true;
        }
    }

    //parse with whitespace
    virtual bool parse_non_term(parserlib_context &con) const {
        return _parse(con);
    }

    //parse terminal
    virtual bool parse_term(parserlib_context &con) const {
        return _parse(con);
    }

	virtual void dump() const
	{
		fprintf(stderr, "[");
		char c;
		for (bool v : m_set)
		{
			if (v)
				fprintf(stderr, "%c", c);
			c++;
		}
		fprintf(stderr, "]");
	}

private:
    //set is kept as an array of flags, for quick access
    std::vector<bool> m_set;

    //add character
    void _add(size_t i) {
        if (i >= m_set.size()) {
            m_set.resize(i + 1);
        }
        m_set[i] = true;
    }

    //internal parse
    bool _parse(parserlib_context &con) const {
        if (!con.end()) {
            size_t ch = con.symbol();
            if (ch < m_set.size() && m_set[ch]) {
                con.next_col();
                return true;
            }
        }
        con.set_error_pos();
        return false;
    }
};


//base class for unary expressions
class _unary : public parserlib_expr {
public:
    //constructor.
    _unary(parserlib_expr *e) :
        m_expr(e)
    {
    }

    //destructor.
    virtual ~_unary() {
        delete m_expr;
    }

protected:
    //expression
    parserlib_expr *m_expr;
};


//terminal
class _term : public _unary {
public:
    //constructor.
    _term(parserlib_expr *e) :
        _unary(e)
    {
    }

    //parse with whitespace
    virtual bool parse_non_term(parserlib_context &con) const {
        return m_expr->parse_term(con);
    }

    //parse terminal
    virtual bool parse_term(parserlib_context &con) const {
        return m_expr->parse_term(con);
    }

	virtual void dump() const
	{
		m_expr->dump();
	}

};


//loop 0
class _loop0 : public _unary {
public:
    //constructor.
    _loop0(parserlib_expr *e) :
        _unary(e)
    {
    }

    //parse with whitespace
    virtual bool parse_non_term(parserlib_context &con) const {
        //if parsing of the first fails, restore the context and stop
        con.parse_ws();
        _state st(con);
        if (!m_expr->parse_non_term(con)) {
			if (con.unwinding)
			{
				return false;
			}
            con.restore(st);
            return true;
        }

        //parse the rest
        for(;;) {
            con.parse_ws();
            _state st(con);
            if (!m_expr->parse_non_term(con)) {
				if (con.unwinding)
				{
					return false;
				}
                con.restore(st);
                break;
            }
        }

        return true;
    }

    //parse terminal
    virtual bool parse_term(parserlib_context &con) const {
        //if parsing of the first fails, restore the context and stop
        _state st(con);
        if (!m_expr->parse_term(con)) {
			if (con.unwinding)
			{
				return false;
			}
            con.restore(st);
            return true;
        }

        //parse the rest until no more parsing is possible
        for(;;) {
            _state st(con);
            if (!m_expr->parse_term(con)) {
				if (con.unwinding)
				{
					return false;
				}
                con.restore(st);
                break;
            }
        }

        return true;
    }
	virtual void dump() const
	{
		fprintf(stderr, "*( ");
		m_expr->dump();
		fprintf(stderr, " )");
	}
};


//loop 1
class _loop1 : public _unary {
public:
    //constructor.
    _loop1(parserlib_expr *e) :
        _unary(e)
    {
    }

    //parse with whitespace
    virtual bool parse_non_term(parserlib_context &con) const {
        //parse the first; if the first fails, stop
        con.parse_ws();
        if (!m_expr->parse_non_term(con)) return false;

        //parse the rest until no more parsing is possible
        for(;;) {
            con.parse_ws();
            _state st(con);
            if (!m_expr->parse_non_term(con)) {
				if (con.unwinding)
				{
					return false;
				}
                con.restore(st);
                break;
            }
        }

        return true;
    }

    //parse terminal
    virtual bool parse_term(parserlib_context &con) const {
        //parse the first; if the first fails, stop
        if (!m_expr->parse_term(con)) return false;

        //parse the rest until no more parsing is possible
        for(;;) {
            _state st(con);
            if (!m_expr->parse_term(con)) {
				if (con.unwinding)
				{
					return false;
				}
                con.restore(st);
                break;
            }
        }

        return true;
    }

	virtual void dump() const
	{
		fprintf(stderr, "+( ");
		m_expr->dump();
		fprintf(stderr, " )");
	}
};


//optional
class _optional : public _unary {
public:
    //constructor.
    _optional(parserlib_expr *e) :
        _unary(e)
    {
    }

    //parse with whitespace
    virtual bool parse_non_term(parserlib_context &con) const {
        _state st(con);
        if (!m_expr->parse_non_term(con)) con.restore(st);
        return true;
    }

    //parse terminal
    virtual bool parse_term(parserlib_context &con) const {
        _state st(con);
        if (!m_expr->parse_term(con)) con.restore(st);
        return true;
    }

	virtual void dump() const
	{
		fprintf(stderr, "-( ");
		m_expr->dump();
		fprintf(stderr, " )");
	}
};


//and
class _and : public _unary {
public:
    //constructor.
    _and(parserlib_expr *e) :
        _unary(e)
    {
    }

    //parse with whitespace
    virtual bool parse_non_term(parserlib_context &con) const {
        _state st(con);
        bool ok = m_expr->parse_non_term(con);
        con.restore(st);
        return ok;
    }

    //parse terminal
    virtual bool parse_term(parserlib_context &con) const {
        _state st(con);
        bool ok = m_expr->parse_term(con);
        con.restore(st);
        return ok;
    }

	virtual void dump() const
	{
		fprintf(stderr, "&( ");
		m_expr->dump();
		fprintf(stderr, " )");
	}
};


//not
class _not : public _unary {
public:
    //constructor.
    _not(parserlib_expr *e) :
        _unary(e)
    {
    }

    //parse with whitespace
    virtual bool parse_non_term(parserlib_context &con) const {
        _state st(con);
        bool ok = !m_expr->parse_non_term(con);
        con.restore(st);
        return ok;
    }

    //parse terminal
    virtual bool parse_term(parserlib_context &con) const {
        _state st(con);
        bool ok = !m_expr->parse_term(con);
        con.restore(st);
        return ok;
    }

	virtual void dump() const
	{
		fprintf(stderr, "!( ");
		m_expr->dump();
		fprintf(stderr, " )");
	}
};


//newline
class _nl : public _unary {
public:
    //constructor.
    _nl(parserlib_expr *e) :
        _unary(e)
    {
    }

    //parse with whitespace
    virtual bool parse_non_term(parserlib_context &con) const {
        if (!m_expr->parse_non_term(con)) return false;
        con.next_line();
        return true;
    }

    //parse terminal
    virtual bool parse_term(parserlib_context &con) const {
        if (!m_expr->parse_term(con)) return false;
        con.next_line();
        return true;
    }

	virtual void dump() const
	{
		fprintf(stderr, "nl( ");
		m_expr->dump();
		fprintf(stderr, " )");
	}
};


//base class for binary expressions
class _binary : public parserlib_expr {
public:
    //constructor.
    _binary(parserlib_expr *left, parserlib_expr *right) :
        m_left(left), m_right(right)
    {
    }

    //destructor.
    virtual ~_binary() {
        delete m_left;
        delete m_right;
    }

protected:
    //left and right expressions
    parserlib_expr *m_left, *m_right;
};


//sequence
class _seq : public _binary {
public:
    //constructor.
    _seq(parserlib_expr *left, parserlib_expr *right) :
        _binary(left, right)
    {
    }

    //parse with whitespace
    virtual bool parse_non_term(parserlib_context &con) const {
        if (!m_left->parse_non_term(con)) return false;
        con.parse_ws();
        return m_right->parse_non_term(con);
    }

    //parse terminal
    virtual bool parse_term(parserlib_context &con) const {
        if (!m_left->parse_term(con)) return false;
        return m_right->parse_term(con);
    }

	virtual void dump() const
	{
		m_left->dump();
		fprintf(stderr, " >> ");
		m_right->dump();
	}
};


//choice
class _choice : public _binary {
public:
    //constructor.
    _choice(parserlib_expr *left, parserlib_expr *right) :
        _binary(left, right)
    {
    }

    //parse with whitespace
    virtual bool parse_non_term(parserlib_context &con) const {
        _state st(con);
        if (m_left->parse_non_term(con)) return true;
		if (con.unwinding)
		{
			return false;
		}
        con.restore(st);
        return m_right->parse_non_term(con);
    }

    //parse terminal
    virtual bool parse_term(parserlib_context &con) const {
        _state st(con);
        if (m_left->parse_term(con)) return true;
		if (con.unwinding)
		{
			return false;
		}
        con.restore(st);
        return m_right->parse_term(con);
    }

	virtual void dump() const
	{
		m_left->dump();
		fprintf(stderr, " | ");
		m_right->dump();
	}
};


//reference to rule
class _ref : public parserlib_expr {
public:
    //constructor.
    _ref(rule &r) :
        m_rule(r)
    {
    }

    //parse with whitespace
    virtual bool parse_non_term(parserlib_context &con) const {
        return con.parse_non_term(m_rule);
    }

    //parse terminal
    virtual bool parse_term(parserlib_context &con) const {
        return con.parse_term(m_rule);
    }

	virtual void dump() const
	{
		fprintf(stderr, "{Reference to rule}");
	}

private:
    //reference
    rule &m_rule;
};


//eof
class _eof : public parserlib_expr {
public:
    //parse with whitespace
    virtual bool parse_non_term(parserlib_context &con) const {
        return parse_term(con);
    }

    //parse terminal
    virtual bool parse_term(parserlib_context &con) const {
        return con.end();
    }

	virtual void dump() const
	{
		fprintf(stderr, "$eof");
	}
};


//any
class any : public parserlib_expr {
public:
    //parse with whitespace
    virtual bool parse_non_term(parserlib_context &con) const {
        return parse_term(con);
    }

    //parse terminal
    virtual bool parse_term(parserlib_context &con) const {
        if (!con.end()) {
            con.next_col();
            return true;
        }
        con.set_error_pos();
        return false;
    }

	virtual void dump() const
	{
		fprintf(stderr, "$any");
	}
};


//exception thrown when left recursion terminates successfully
struct _lr_ok {
    rule *m_rule;
    _lr_ok(rule *r) : m_rule(r) {}
};


//constructor
_state::_state(parserlib_context &con) :
    m_pos(con.m_pos),
    m_matches(con.m_matches.size())
{
}


//parse non-term rule.
bool parserlib_context::parse_non_term(rule &r)
{
	return parse_rule(r, &parserlib_context::_parse_non_term);
}

bool parserlib_context::parse_rule(rule &r, bool (parserlib_context::*parse_func)(rule &))
{
	if (unwinding) return false;
    //save the state of the rule
    rule::_state old_state = r.m_state;

    //success/failure result
    bool ok;

    //compute the new position
    size_t new_pos = m_pos.it - m_begin;

    //check if we have left recursion
    bool lr = new_pos == r.m_state.m_pos;

    //update the rule's state
    r.m_state.m_pos = new_pos;

    //handle the mode of the rule
    switch (r.m_state.m_mode) {
        //normal parse
        case rule::_PARSE:
            if (lr) {
                //first try to parse the rule by rejecting it, so alternative branches are examined
                r.m_state.m_mode = rule::_REJECT;
                ok = (this->*parse_func)(r);
				if (unwinding)
				{
					return false;
				}

                //if the first try is successful, try accepting the rule,
                //so other elements of the sequence are parsed
                if (ok) {
                    r.m_state.m_mode = rule::_ACCEPT;

                    //loop until no more parsing can be done
                    for(;;) {
                        //store the correct state, in order to backtrack if the call fails
                        _state st(*this);

                        //update the rule position to the current position,
                        //because at this state the rule is resolving the left recursion
                        r.m_state.m_pos = m_pos.it - m_begin;

                        //if parsing fails, restore the last good state and stop
                        if (!(this->*parse_func)(r)) {
                            restore(st);
                            break;
                        }
                    }

                    //since the left recursion was resolved successfully,
                    //return via a non-local exit
                    r.m_state = old_state;
					unwind_target = r.this_ptr();
					unwinding = true;
					return false;
                }
            }
            else {
				ok = (this->*parse_func)(r);
				if (unwinding)
				{
					if (unwind_target == r.this_ptr())
					{
						ok = true;
						unwinding = false;
					}
					else
					{
						r.m_state = old_state;
						return false;
					}
				}
				break;
            }
            break;

        //reject the left recursive rule
        case rule::_REJECT:
            if (lr) {
                ok = false;
            }
            else {
                r.m_state.m_mode = rule::_PARSE;
                ok = (this->*parse_func)(r);
				if (unwinding)
				{
					return false;
				}
                r.m_state.m_mode = rule::_REJECT;
            }
            break;

        //accept the left recursive rule
        case rule::_ACCEPT:
            if (lr) {
                ok = true;
            }
            else {
                r.m_state.m_mode = rule::_PARSE;
                ok = (this->*parse_func)(r);
				if (unwinding)
				{
					return false;
				}
                r.m_state.m_mode = rule::_ACCEPT;
            }
            break;
    }

    //restore the rule's state
    r.m_state = old_state;

    return ok;
}



//parse term rule.
bool parserlib_context::parse_term(rule &r) 
{
	return parse_rule(r, &parserlib_context::_parse_term);
}
const bool debug_parsing = false;


//parse non-term rule internal.
bool parserlib_context::_parse_non_term(rule &r) {
    bool ok;
    if (parserlib_private::get_parse_proc(r)) {
        pos b = m_pos;
        ok = parserlib_private::get_expr(r)->parse_non_term(*this);
		if (debug_parsing)
		{
			parserlib_private::get_expr(r)->dump();
			fprintf(stderr, "\n");
		}
        if (ok) {
            m_matches.push_back(_match(r.this_ptr(), b, m_pos));
        }
    }
    else {
        ok = parserlib_private::get_expr(r)->parse_non_term(*this);
    }
    return ok;
}


//parse term rule internal.
bool parserlib_context::_parse_term(rule &r) {
    bool ok;
    if (parserlib_private::get_parse_proc(r)) {
        pos b = m_pos;
        ok = parserlib_private::get_expr(r)->parse_term(*this);
        if (ok) {
            m_matches.push_back(_match(r.this_ptr(), b, m_pos));
        }
    }
    else {
        ok = parserlib_private::get_expr(r)->parse_term(*this);
    }
    return ok;
}


//get the next position
static pos _next_pos(const pos &p) {
    pos r = p;
    ++r.it;
    ++r.col;
    return r;
}


//get syntax error
static error _syntax_error(parserlib_context &con) {
    std::wstring str = L"syntax error: ";
    str += (wchar_t)*con.m_error_pos.it;
    return error(con.m_error_pos, _next_pos(con.m_error_pos), ERROR_SYNTAX_ERROR);
}


//get eof error
static error _eof_error(parserlib_context &con) {
    return error(con.m_error_pos, con.m_error_pos, ERROR_INVALID_EOF);
}


/** constructor from input.
    @param i input.
 */
pos::pos(Input &i) :
    it(i.begin()),
    line(1),
    col(1)
{
}


/** character terminal constructor.
    @param c character.
 */
expr::expr(int c) :
    m_expr(new _char(c))
{
}


/** null-terminated string terminal constructor.
    @param s null-terminated string.
 */
expr::expr(const char *s) :
    m_expr(new _string(s))
{
}


/** null-terminated wide string terminal constructor.
    @param s null-terminated string.
 */
expr::expr(const wchar_t *s) :
    m_expr(new _string(s))
{
}


/** rule reference constructor.
    @param r rule.
 */
expr::expr(rule &r) :
    m_expr(new _ref(r))
{
}


/** creates a zero-or-more loop out of this expression.
    @return a zero-or-more loop expression.
 */
expr expr::operator *() const {
    return parserlib_private::construct_expr(new _loop0(m_expr));
}


/** creates a one-or-more loop out of this expression.
    @return a one-or-more loop expression.
 */
expr expr::operator +() const {
    return parserlib_private::construct_expr(new _loop1(m_expr));
}


/** creates an optional out of this expression.
    @return an optional expression.
 */
expr expr::operator -() const {
    return parserlib_private::construct_expr(new _optional(m_expr));
}


/** creates an AND-expression.
    @return an AND-expression.
 */
expr expr::operator &() const {
    return parserlib_private::construct_expr((new _and(m_expr)));
}


/** creates a NOT-expression.
    @return a NOT-expression.
 */
expr expr::operator !() const {
    return parserlib_private::construct_expr(new _not(m_expr));
}


/** constructor.
    @param b begin position.
    @param e end position.
 */
input_range::input_range(const pos &b, const pos &e) :
    m_begin(b),
    m_end(e)
{
}


/** constructor.
    @param b begin position.
    @param e end position.
    @param t error type.
 */
error::error(const pos &b, const pos &e, int t) :
    input_range(b, e),
    m_type(t)
{
}


/** compare on begin position.
    @param e the other error to compare this with.
    @return true if this comes before the previous error, false otherwise.
 */
bool error::operator < (const error &e) const {
    return m_begin.it < e.m_begin.it;
}


/** character terminal constructor.
    @param c character.
 */
rule::rule(int c) :
    m_expr(new _char(c))
{
    m_parse_proc = _get_parse_proc(this);
}


/** null-terminated string terminal constructor.
    @param s null-terminated string.
 */
rule::rule(const char *s) :
    m_expr(new _string(s))
{
    m_parse_proc = _get_parse_proc(this);
}


/** null-terminated wide string terminal constructor.
    @param s null-terminated string.
 */
rule::rule(const wchar_t *s) :
    m_expr(new _string(s))
{
    m_parse_proc = _get_parse_proc(this);
}


/** constructor from expression.
    @param e expression.
 */
rule::rule(const expr &e) :
    m_expr(parserlib_private::get_expr(e))
{
    m_parse_proc = _get_parse_proc(this);
}


/** constructor from rule.
    @param r rule.
 */
rule::rule(rule &r) :
    m_expr(new _ref(r)),
    m_parse_proc(0)
{
    m_parse_proc = _get_parse_proc(this);
}

/** deletes the internal object that represents the expression.
 */
rule::~rule() {
    delete m_expr;
}


/** creates a zero-or-more loop out of this rule.
    @return a zero-or-more loop rule.
 */
expr rule::operator *() {
    return parserlib_private::construct_expr(new _loop0(new _ref(*this)));
}


/** creates a one-or-more loop out of this rule.
    @return a one-or-more loop rule.
 */
expr rule::operator +() {
    return parserlib_private::construct_expr(new _loop1(new _ref(*this)));
}


/** creates an optional out of this rule.
    @return an optional rule.
 */
expr rule::operator -() {
    return parserlib_private::construct_expr(new _optional(new _ref(*this)));
}


/** creates an AND-expression out of this rule.
    @return an AND-expression out of this rule.
 */
expr rule::operator &() {
    return parserlib_private::construct_expr(new _and(new _ref(*this)));
}


/** creates a NOT-expression out of this rule.
    @return a NOT-expression out of this rule.
 */
expr rule::operator !() {
    return parserlib_private::construct_expr(new _not(new _ref(*this)));
}


/** sets the parse procedure.
    @param p procedure.
 */
void rule::set_parse_proc(parse_proc p) {
    assert(p);
    m_parse_proc = p;
    (*getParseProcMap())[this] = p;
}


/** creates a sequence of expressions.
    @param left left operand.
    @param right right operand.
    @return an expression which parses a sequence.
 */
expr operator >> (const expr &left, const expr &right) {
    return parserlib_private::construct_expr(
        new _seq(parserlib_private::get_expr(left), parserlib_private::get_expr(right)));
}


/** creates a choice of expressions.
    @param left left operand.
    @param right right operand.
    @return an expression which parses a choice.
 */
expr operator | (const expr &left, const expr &right) {
    return parserlib_private::construct_expr(
        new _choice(parserlib_private::get_expr(left), parserlib_private::get_expr(right)));
}


/** converts a parser expression into a terminal.
    @param e expression.
    @return an expression which parses a terminal.
 */
expr term(const expr &e) {
    return parserlib_private::construct_expr(
        new _term(parserlib_private::get_expr(e)));
}


/** creates a set expression from a null-terminated string.
    @param s null-terminated string with characters of the set.
    @return an expression which parses a single character out of a set.
 */
expr set(const char *s) {
    return parserlib_private::construct_expr(new _set(s));
}


/** creates a set expression from a null-terminated wide string.
    @param s null-terminated string with characters of the set.
    @return an expression which parses a single character out of a set.
 */
expr set(const wchar_t *s) {
    return parserlib_private::construct_expr(new _set(s));
}


/** creates a range expression.
    @param min min character.
    @param max max character.
    @return an expression which parses a single character out of range.
 */
expr range(int min, int max) {
    return parserlib_private::construct_expr(new _set(min, max));
}


/** creates an expression which increments the line counter
    and resets the column counter when the given expression
    is parsed successfully; used for newline characters.
    @param e expression to wrap into a newline parser.
    @return an expression that handles newlines.
 */
expr nl(const expr &e) {
    return parserlib_private::construct_expr(new _nl(parserlib_private::get_expr(e)));
}


/** creates an expression which tests for the end of input.
    @return an expression that handles the end of input.
 */
expr eof() {
    return parserlib_private::construct_expr(new _eof());
}


/** creates a not expression.
    @param e expression.
    @return the appropriate expression.
 */
expr not_(const expr &e) {
    return !e;
}


/** creates an and expression.
    @param e expression.
    @return the appropriate expression.
 */
expr and_(const expr &e) {
    return &e;
}


/** creates an expression that parses any character.
    @return the appropriate expression.
 */
expr any() {
    return parserlib_private::construct_expr(new class any());
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
bool parse(Input &i, rule &g, rule &ws, error_list &el, void *d) {
    //prepare context
    parserlib_context con(i, ws);

    //parse initial whitespace
    con.parse_term(con.m_ws);

    //parse grammar
    if (!con.parse_non_term(g)) {
        el.push_back(_syntax_error(con));
        return false;
    }

    //parse whitespace at the end
    con.parse_term(con.m_ws);

    //if end is not reached, there was an error
    if (!con.end()) {
        if (con.m_error_pos.it < con.m_end) {
            el.push_back(_syntax_error(con));
        }
        else {
            el.push_back(_eof_error(con));
        }
        return false;
    }

    //success; execute the parse procedures
    con.do_parse_procs(d);
    return true;
}


} //namespace parserlib
