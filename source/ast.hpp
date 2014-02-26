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
#ifndef PEGMATITE_AST_HPP
#define PEGMATITE_AST_HPP


#include <cassert>
#include <list>
#include <unordered_map>
#include <memory>
#include "parser.hpp"


namespace pegmatite {


class ASTNode;
template <class T, bool OPT> class ASTPtr;
template <class T> class ASTList;
template <class T> class BindAST;


/** type of AST node stack.
 */
typedef std::vector<std::unique_ptr<ASTNode>> ASTStack;

#ifdef USE_RTTI
#define PARSELIB_RTTI(thisclass, superclass)
#else
/**
 * Define the methods required for pegmatite's lightweight RTTI replacement to
 * work.  This should be used at the end of the class definition and will
 * provide support for safe downcasting.
 */
#define PARSELIB_RTTI(thisclass, superclass)             \
	friend ASTNode;                                      \
protected:                                               \
	virtual char *kind()                                 \
	{                                                    \
		return thisclass::classKind();                   \
	}                                                    \
	static char *classKind()                             \
	{                                                    \
		static char thisclass ## id;                     \
		return &thisclass ## id;                         \
	}                                                    \
public:                                                  \
	virtual bool isa(char *x)                            \
	{                                                    \
		return (x == kind()) ||                          \
				(superclass::isa(x));                    \
	}
#endif


/** Base class for AST nodes.
 */
class ASTNode
{
public:
	///constructor.
	ASTNode() : parent_node(0) {}
	
	/** copy constructor.
		@param n source object.
	 */
	ASTNode(const ASTNode &n) : parent_node(0) {}

	///destructor.
	virtual ~ASTNode() {}
	
	/** assignment operator.
		@param n source object.
		@return reference to this.
	 */
	ASTNode &operator = (const ASTNode &n) { return *this; }
	
	/** get the parent node.
		@return the parent node, if there is one.
	 */
	ASTNode *parent() const { return parent_node; }
	
	/** interface for filling the contents of the node
		from a node stack.
		@param st stack.
	 */
	virtual void construct(const InputRange &r, ASTStack &st) {}
	
private:
	//parent
	ASTNode *parent_node;	
	
	template <class T, bool OPT> friend class ASTPtr;
	template <class T> friend class ASTList;
	template <class T> friend class BindAST;

#ifndef USE_RTTI
protected:
	virtual char *kind() { return classKind(); }
	static char *classKind()
	{
		static char ASTNodeid;
		return &ASTNodeid;
	}
public:
	virtual bool isa(char *x)
	{
		return x == classKind();
	}
	template <class T> bool isa()
	{
		return isa(T::classKind());
	}
	template <class T> T* get_as()
	{
		return isa<T>() ? static_cast<T*>(this) : 0;
	}
#else
public:
	template <class T> T* get_as()
	{
		return dynamic_cast<T*>(this);
	}
#endif
};


class ASTMember;


/** type of ast member vector.
 */
typedef std::vector<ASTMember *> ASTMember_vector;


/** base class for AST nodes with children.
 */
class ASTContainer : public ASTNode
{
public:
	/** sets the container under construction to be this.
	 */
	ASTContainer();

	/** sets the container under construction to be this.
		Members are not copied.
		@param src source object.
	 */
	ASTContainer(const ASTContainer &src);

	/** the assignment operator.
		The members are not copied.
		@param src source object.
		@return reference to this.
	 */
	ASTContainer &operator = (const ASTContainer &src)
	{
		return *this;
	}

	/** Asks all members to construct themselves from the stack.
		The members are asked to construct themselves in reverse order.
		from a node stack.
		@param st stack.
	 */
	virtual void construct(const InputRange &r, ASTStack &st);

private:
	ASTMember_vector members;

	friend class ASTMember;
	PARSELIB_RTTI(ASTContainer, ASTNode)
};


/** Base class for children of ASTContainer.
 */
class ASTMember
{
public:
	/** automatically registers itself to the container under construction.
	 */
	ASTMember() { _init(); }

	/** automatically registers itself to the container under construction.
		@param src source object.
	 */
	ASTMember(const ASTMember &src) { _init(); }

	/** the assignment operator.
		@param src source object.
		@return reference to this.
	 */
	ASTMember &operator = (const ASTMember &src)
	{
		return *this;
	}
	
	/** returns the container this belongs to.
		@return the container this belongs to.
	 */
	ASTContainer *container() const { return container_node; }

	/** interface for filling the the member from a node stack.
		@param st stack.
	 */
	virtual void construct(const InputRange &r, ASTStack &st) = 0;

private:
	//the container this belongs to.
	ASTContainer *container_node;

	//register the AST member to the current container.
	void _init();
};


/** pointer to an AST object.
	It assumes ownership of the object.
	It pops an object of the given type from the stack.
	@param T type of object to control.
	@param OPT if true, the object becomes optional.
 */
template <class T, bool OPT = false> class ASTPtr : public ASTMember
{
public:
	/** the default constructor.
		@param obj object.
	 */
	ASTPtr(T *obj = 0) : ptr(obj)
	{
		_set_parent();
	}

	/** copies the given object.
		The old object is deleted.
		@param obj new object.
		@return reference to this.
	 */
	ASTPtr<T, OPT> &operator = (const T *obj)
	{
		delete ptr;
		ptr = obj ? new T(*obj) : 0;
		_set_parent();
		return *this;
	}

	/** gets the underlying ptr value.
		@return the underlying ptr value.
	 */
	T *get() const
	{
		return ptr.get();
	}

	/** auto conversion to the underlying object ptr.
		@return the underlying ptr value.
	 */
	operator T *() const
	{
		return ptr;
	}

	/** member access.
		@return the underlying ptr value.
	 */
	T *operator ->() const
	{
		assert(ptr);
		return ptr;
	}

	/** Pops a node from the stack.
		@param st stack.
		@exception std::logic_error thrown if the node is not of the appropriate type;
			thrown only if OPT == false or if the stack is empty.
	 */
	virtual void construct(const InputRange &r, ASTStack &st)
	{
		//check the stack node
		//if (st.empty()) throw std::logic_error("empty AST stack");
	
		//get the node
		ASTNode *node = st.back().get();
		
		//get the object
		T *obj = node->get_as<T>();
		
		//if the object is optional, simply return
		if (OPT)
		{
			if (!obj) return;
		}
		
		//else if the object is mandatory, throw an exception
		else
		{
			//if (!obj) throw std::logic_error("invalid AST node");
		}
		
		
		//set the new object
		ptr.reset(obj);
		//pop the node from the stack
		st.back().release();
		st.pop_back();
		_set_parent();
	}

private:
	//ptr
	std::unique_ptr<T> ptr;
	
	//set parent of object
	void _set_parent()
	{
		if (ptr) ptr->parent_node = container();
	}
};


/** A list of objects.
	It pops objects of the given type from the ast stack, until no more objects can be popped.
	It assumes ownership of objects.
	@param T type of object to control.
 */
template <class T> class ASTList : public ASTMember
{
public:
	///list type.
	typedef std::list<std::unique_ptr<T>> container;

	///the default constructor.
	ASTList() {}

	/** duplicates the objects of the given list.
		@param src source object.
	 */
	ASTList(const ASTList<T> &src)
	{
		_dup(src);
	}

	/** returns the container of objects.
		@return the container of objects.
	 */
	const container &objects() const
	{
		return child_objects;
	}

	/** Pops objects of type T from the stack until no more objects can be popped.
		@param st stack.
	 */
	virtual void construct(const InputRange &r, ASTStack &st)
	{
		for(;;)
		{
			//if the stack is empty
			if (st.empty()) break;
			
			//get the node
			ASTNode *node = st.back().get();
			
			//get the object
			T *obj = node->get_as<T>();
			
			//if the object was not not of the appropriate type,
			//end the list parsing
			if (!obj) return;
			
			//remove the node from the stack
			st.back().release();
			st.pop_back();
			
			//insert the object in the list, in reverse order
			child_objects.push_front(std::unique_ptr<T>(obj));
			
			//set the object's parent
			obj->parent_node = ASTMember::container();
		}
	}

private:
	//objects
	container child_objects;

	//duplicate the given list.
	void _dup(const ASTList<T> &src)
	{
		for (auto child : src.child_objects)
		{
			T *obj = new T(child.get());
			child_objects.push_back(obj);
			obj->parent_node = ASTMember::container();
		}
	}
};

/** parses the given input.
	@param i input.
	@param g root rule of grammar.
	@param ws whitespace rule.
	@param el list of errors.
	@param d user data, passed to the parse procedures.
	@return pointer to ast node created, or null if there was an error.
		The return object must be deleted by the caller.
 */
std::unique_ptr<ASTNode> parse(Input &i, Rule &g, Rule &ws, ErrorList &el,
                               const ParserDelegate &d);

class ASTParserDelegate : ParserDelegate
{
	std::unordered_map<Rule*, parse_proc> handlers;
	void set_parse_proc(Rule &r, parse_proc p);
	public:
	ASTParserDelegate();
	virtual parse_proc get_parse_proc(Rule &) const;
	static void bind_parse_proc(Rule &r, parse_proc p);
	template <class T> bool parse(Input &i, Rule &g, Rule &ws, ErrorList &el,
	                              std::unique_ptr<T> &ast) const
	{
		std::unique_ptr<ASTNode> node = pegmatite::parse(i, g, ws, el, *this);
		T *n = node->get_as<T>();
		if (n)
		{
			node.release();
			ast.reset(n);
			return true;
		}
		return false;
	}
};

/** AST function which creates an object of type T
	and pushes it to the node stack.
 */
template <class T> class BindAST
{
public:
	/** constructor.
		@param r Rule to attach the AST function to.
	 */
	BindAST(Rule &r)
	{
		ASTParserDelegate::bind_parse_proc(r, [](const ParserPosition &b,
		                                         const ParserPosition &e, void *d)
			{
				ASTStack *st = reinterpret_cast<ASTStack *>(d);
				T *obj = new T();
				obj->construct(InputRange(b, e), *st);
				st->push_back(std::unique_ptr<ASTNode>(obj));
			});
	}
};


} //namespace pegmatite


#endif //PEGMATITE_AST_HPP
