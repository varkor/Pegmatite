#include <cassert>
#include "ast.hpp"

namespace parserlib {

namespace {
/**
 * The current AST container.  When constructing an object, this is set and
 * then the constructors for the fields run, accessing it to detect their
 * parents.
 */
// FIXME: Should be thread_local, but that doesn't seem to work on OS X for some reason (__thread does)
__thread ast_container *current = 0;
}

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
void ast_container::construct(ast_stack &st) {
    for(ast_member_vector::reverse_iterator it = m_members.rbegin();
        it != m_members.rend();
        ++it)
    {
        ast_member *member = *it;
        member->construct(st);
    }
}


//register the AST member to the current container.
void ast_member::_init() {
    assert(current);
    m_container = current;
    current->m_members.push_back(this);
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
ast_node *parse(input &i, rule &g, rule &ws, error_list &el) {
    ast_stack st;
    if (!parse(i, g, ws, el, &st)) return 0;
    assert(st.size() == 1);
    return st[0];
}


} //namespace parserlib
