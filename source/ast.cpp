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
#include <cassert>
#include "ast.hpp"


namespace {
/**
 * The current AST container.  When constructing an object, this is set and
 * then the constructors for the fields run, accessing it to detect their
 * parents.
 */
// FIXME: Should be thread_local, but that doesn't seem to work on OS X for
// some reason (__thread does)
__thread parserlib::ast_container *current = 0;
/**
 * The current parser delegate.  When constructing an object, this is set and
 * then the constructors for the fields run, accessing it to detect their
 * parents.
 */
__thread parserlib::ASTParserDelegate *currentParserDelegate;
}

namespace parserlib {

/** sets the container under construction to be this.
 */
ast_container::ast_container() {
    current = this;
}


/** sets the container under construction to be this.
    @param src source object.
 */
ast_container::ast_container(const ast_container &src) {
    current = this;
}

    
/** Asks all members to construct themselves from the stack.
    The members are asked to construct themselves in reverse order.
    from a node stack.
    @param st stack.
 */
void ast_container::construct(const input_range &r, ast_stack &st) {
	for(auto it = m_members.rbegin(); it != m_members.rend(); ++it)
	{
		ast_member *member = *it;
		member->construct(r, st);
	}
}


//register the AST member to the current container.
void ast_member::_init() {
    assert(current);
    m_container = current;
    current->m_members.push_back(this);
}


ASTParserDelegate::ASTParserDelegate()
{
	currentParserDelegate = this;
}

void ASTParserDelegate::set_parse_proc(rule &r, parse_proc p)
{
	handlers[std::addressof(r)] = p;
}
void ASTParserDelegate::bind_parse_proc(rule &r, parse_proc p)
{
	currentParserDelegate->set_parse_proc(r, p);
}
parse_proc ASTParserDelegate::get_parse_proc(rule &r) const
{
	auto it = handlers.find(std::addressof(r));
	if (it == handlers.end()) return 0;
	return it->second;
}

/** parses the given input.
    @param i input.
    @param g root rule of grammar.
    @param ws whitespace rule.
    @param el list of errors.
    @param d user data, passed to the parse procedures.
    @return pointer to ast node created, or null if there was an error.
        The return object must be deleted by the caller.
 */
ast_node *parse(Input &i, rule &g, rule &ws, error_list &el, const ParserDelegate &d) {
    ast_stack st;
    if (!parse(i, g, ws, el, d, &st)) return 0;
    assert(st.size() == 1);
    return st[0];
}


} //namespace parserlib
