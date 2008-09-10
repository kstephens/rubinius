#ifndef RBX_PRELUDE_HPP
#define RBX_PRELUDE_HPP

/**
 *  @file   prelude.hpp
 *
 *  Notably here, Symbols, Fixnums and true/false/nil are actually
 *  stored directly in the pointer value (and distinguished by the
 *  tag, see oop.hpp) but provided we do not attempt to dereference
 *  it, we can ALSO define a class and treat the pointer values as
 *  if they were real pointers to real objects of that class where
 *  typing is concerned. In the "instance methods", the this pointer
 *  is still the correct pointer value and can thus be used for the
 *  calculations needed.
 *
 *  @see   oop.hpp
 */

#include <stdint.h>
#include <vector>

namespace rubinius {

  /** Platform-dependent integer type large enough for pointers too. */
  typedef intptr_t native_int;

  /* Forward declaration. */
  class VM;

  class Object;
  typedef Object* OBJECT;
  typedef std::vector<OBJECT> ObjectArray;

  class Symbol;
  typedef Symbol* SYMBOL;

  class Fixnum;
  typedef Fixnum* FIXNUM;

  class Integer;
  typedef Integer* INTEGER;

#define FALSE false
#define TRUE true
#define Qfalse ((OBJECT)6L)
#define Qnil   ((OBJECT)14L)
#define Qtrue  ((OBJECT)10L)
#define Qundef ((OBJECT)18L)
#define STATE rubinius::VM *state
#define G(whatever) state->globals.whatever.get()
#define GO(whatever) state->globals.whatever

};

#endif
