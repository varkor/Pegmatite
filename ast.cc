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
#include <cassert>
#include "ast.hh"


namespace {
/**
 * The current AST container.  When constructing an object, this is set and
 * then the constructors for the fields run, accessing it to detect their
 * parents.
 */
// FIXME: Should be thread_local, but that doesn't seem to work on OS X for
// some reason (__thread does)
__thread pegmatite::ASTContainer *current = 0;
/**
 * The current parser delegate.  When constructing an object, this is set and
 * then the constructors for the fields run, accessing it to detect their
 * parents.
 */
__thread pegmatite::ASTParserDelegate *currentParserDelegate;
}

namespace pegmatite {

/** sets the container under construction to be this.
 */
ASTContainer::ASTContainer()
{
	current = this;
}


/** Asks all members to construct themselves from the stack.
	The members are asked to construct themselves in reverse order.
	from a node stack.
	@param st stack.
 */
void ASTContainer::construct(const InputRange &r, ASTStack &st)
{
	for(auto it = members.rbegin(); it != members.rend(); ++it)
	{
		ASTMember *member = *it;
		member->construct(r, st);
	}
	// We don't need the members vector anymore, so clean up the storage it
	// uses.
	ASTMember_vector().swap(members);
}

ASTMember::ASTMember()
{
	assert(current);
	container_node = current;
	current->members.push_back(this);
}
ASTMember::~ASTMember() {}

ASTParserDelegate::ASTParserDelegate()
{
	currentParserDelegate = this;
}

void ASTParserDelegate::set_parse_proc(const Rule &r, parse_proc p)
{
	handlers[std::addressof(r)] = p;
}
void ASTParserDelegate::bind_parse_proc(const Rule &r, parse_proc p)
{
	currentParserDelegate->set_parse_proc(r, p);
}
parse_proc ASTParserDelegate::get_parse_proc(const Rule &r) const
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
	@return pointer to AST node created, or null if there was an Error.
		The return object must be deleted by the caller.
 */
std::unique_ptr<ASTNode> parse(Input &input, const Rule &g, const Rule &ws,
                               ErrorList &el, const ParserDelegate &d)
{
	ASTStack st;
	if (!parse(input, g, ws, el, d, &st)) return 0;
	if (st.size() > 1)
	{
		int i = 0;
		for (auto &I : st)
		{
			fprintf(stderr, "[%d] %s\n", i++, typeid(*I.second.get()).name());
		}
	}
	assert(st.size() == 1);
	return std::move(st[0].second);
}

} //namespace pegmatite
