#ifndef RBX_OOP_HPP
#define RBX_OOP_HPP

#include <stdint.h>
#include <sys/types.h>
#include <sstream>
#include <cstring>
#include <cassert>
#include <vector>

#include "prelude.hpp"
#include "builtin/object.hpp"
#include "exception.hpp"

/**
 *  @file   object.hpp
 *
 *  Defines all the most common operations for dealing with
 *  objects, such as type checking and casting.
 */

namespace rubinius {

  /**
   *  Ruby type system subtype check.
   *
   *  Given builtin-class +T+, return true if +obj+ is of class +T+
   */
  template <class T>
    bool kind_of(OBJECT obj) {
      if(obj->reference_p()) {
        return obj->obj_type == T::type;
      }
      return false;
    }

  template <> bool kind_of<Object>(OBJECT obj);

  /**
   *  Ruby type system class check.
   *
   *  Another version of kind_of that shouldn't be specialized for subtype
   *  compatibility. I.e., only whether object's class is this specific
   *  one.
   */
  template <class T>
    bool instance_of(OBJECT obj) {
      if(obj->reference_p()) {
        return obj->obj_type == T::type;
      }
      return false;
    }

  template <> bool instance_of<Object>(OBJECT obj);

#define sassert(cond) if (!(cond)) Assertion::raise(#cond)

  /*
   *  There is NO reason why as() or try_as() should be getting
   *  NULL arguments. That means something has not been properly
   *  initialised (to Qnil if nothing else.)
   */

  /**
   *  Cast Object* into builtin T*.
   *
   *  Given builtin class +T+, return +obj+ cast as type +T*+. If
   *  +obj+ is not of type +T+, throw's a TypeError exception.
   *
   *  @see  builtin/object.cpp has specialised versions.
   */
  template <class T>
    T* as(OBJECT obj) {
      sassert(NULL != obj);

      if(!kind_of<T>(obj))
        TypeError::raise(T::type, obj);

      return static_cast<T*>(obj);
    }

  /** Specialised cast to always succeed if not NULL.
   *  
   *  Implemented in builtin/object.cpp.
   */
  template <> Object* as<Object>(OBJECT obj);

  /**
   *  Non-raising version of as().
   *
   *  Similar to as<>, but returns NULL if the type is invalid. ONLY
   *  use this when doing a conditional cast.
   *
   *  @see  builtin/object.cpp has specialised versions.
   */
  template <class T>
    T* try_as(OBJECT obj) {
      sassert(NULL != obj);

      if(!kind_of<T>(obj))
        return NULL;

      return static_cast<T*>(obj);
    }

  /** Specialised cast to always succeed if not NULL.
   *  
   *  Implemented in builtin/object.cpp.
   */
  template <> Object* try_as<Object>(OBJECT obj);

  void type_assert(OBJECT obj, object_type type, const char* reason);

  /*
   * A rubinius object can be followed by:
   * - a series of fields, possibly including an ivar
   * - a series of bytes (ByteArray)
   * - a fast_context pointer
   */

  /* HACK: refactor this to use the state_setup_type code path. */
  struct wraps_struct {
    void *ptr;
    void (*mark)(void*);
    void (*free)(void*);
  };

#define MARK_WRAPPED_STRUCT(obj) do { \
  struct wraps_struct *s = (struct wraps_struct *)BYTES_OF(obj); \
  if(s->mark != NULL) { s->mark(s->ptr); } \
} while (0)

#define FREE_WRAPPED_STRUCT(obj) do { \
  struct wraps_struct *s = (struct wraps_struct *)BYTES_OF(obj); \
  if(s->free != NULL) { s->free(s->ptr); } \
} while (0)

};

#endif