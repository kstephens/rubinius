
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/stat.h>
#include <dlfcn.h>

#include "prelude.hpp"
#include "object.hpp"
#include "vm.hpp"
#include "objectmemory.hpp"

#include "builtin/array.hpp"
#include "builtin/class.hpp"
#include "builtin/fixnum.hpp"
#include "builtin/float.hpp"
#include "builtin/memorypointer.hpp"
#include "builtin/nativefunction.hpp"
#include "builtin/string.hpp"
#include "builtin/symbol.hpp"

#include "ffi.hpp"
#include "message.hpp"

extern "C" {
  #include "strlcpy.h"
  #include "strlcat.h"
}

namespace rubinius {

  void MemoryPointer::init(STATE) {
    GO(memory_pointer).set(state->new_class("MemoryPointer"));
    G(memory_pointer)->set_object_type(MemPtrType);
  }

  MemoryPointer* MemoryPointer::create(STATE, void* ptr) {
    MemoryPointer* obj = (MemoryPointer*)state->new_struct(G(memory_pointer),
        sizeof(MemoryPointer));
    obj->pointer = ptr;
    obj->autorelease = false;
    return obj;
  }

  Integer* MemoryPointer::get_address(STATE) {
    return Integer::from(state, (size_t)pointer);
  }

  MemoryPointer* MemoryPointer::add(STATE, Integer* amount) {
    return MemoryPointer::create(state, (char*)pointer + amount->to_native());
  }

  OBJECT MemoryPointer::set_autorelease(STATE, OBJECT val) {
    autorelease = val->true_p() ? true : false;

    return val;
  }

  String* MemoryPointer::read_string(STATE, FIXNUM len) {
    // HM. This is pretty dangerous. Should we figure out how to
    // protect this?
    return String::create(state, (char*)pointer, len->to_native());
  }

  String* MemoryPointer::read_string_to_null(STATE) {
    // Danger!
    // This operation might be too dangerous! You can read into any
    // memory using it!
    return String::create(state, (char*)pointer);
  }

  MemoryPointer* MemoryPointer::write_string(STATE, String* str, FIXNUM len) {
    memcpy(pointer, (void*)str->byte_address(), len->to_native());
    return this;
  }

  Integer* MemoryPointer::write_int(STATE, Integer* val) {
    *(int*)pointer = val->to_native();
    return val;
  }

  Integer* MemoryPointer::read_int(STATE) {
    return Integer::from(state, *(int*)pointer);
  }

  Integer* MemoryPointer::write_long(STATE, Integer* val) {
    *(long*)pointer = val->to_native();
    return val;
  }

  Integer* MemoryPointer::read_long(STATE) {
    return Integer::from(state, *(long*)pointer);
  }

  Float* MemoryPointer::write_float(STATE, Float* flt) {
    *(double*)pointer = flt->val;
    return flt;
  }

  Float* MemoryPointer::read_float(STATE) {
    return Float::create(state, *(double*)pointer);
  }

  MemoryPointer* MemoryPointer::read_pointer(STATE) {
    return MemoryPointer::create(state, *(void**)pointer);
  }

  OBJECT MemoryPointer::get_at_offset(STATE, FIXNUM offset, FIXNUM type) {
    return get_field(state, offset->to_native(), type->to_native());
  }

  OBJECT MemoryPointer::get_field(STATE, int offset, int type) {
    OBJECT ret;
    char* ptr = (char*)pointer;

    ptr += offset;

#define READ(type) (*((type*)(ptr)))

    switch(type) {
    case RBX_FFI_TYPE_CHAR:
      ret = Fixnum::from((int)(READ(char)));
      break;
    case RBX_FFI_TYPE_UCHAR:
      ret = Fixnum::from((unsigned int)(READ(unsigned char)));
      break;
    case RBX_FFI_TYPE_SHORT:
      ret = Fixnum::from((int)(READ(short)));
      break;
    case RBX_FFI_TYPE_USHORT:
      ret = Fixnum::from((unsigned int)(READ(unsigned short)));
      break;
    case RBX_FFI_TYPE_INT:
      ret = Fixnum::from(READ(int));
      break;
    case RBX_FFI_TYPE_UINT:
      ret = Fixnum::from(READ(unsigned int));
      break;
    case RBX_FFI_TYPE_LONG:
      ret = Fixnum::from(READ(long));
      break;
    case RBX_FFI_TYPE_ULONG:
      ret = Fixnum::from(READ(unsigned long));
      break;
    case RBX_FFI_TYPE_FLOAT:
      ret = Float::create(state, (double)READ(float));
      break;
    case RBX_FFI_TYPE_DOUBLE:
      ret = Float::create(state, READ(double));
      break;
    case RBX_FFI_TYPE_LONG_LONG:
      ret = Fixnum::from(READ(long long));
      break;
    case RBX_FFI_TYPE_ULONG_LONG:
      ret = Fixnum::from(READ(unsigned long long));
      break;
    case RBX_FFI_TYPE_OBJECT:
      ret = READ(OBJECT);
      break;
    case RBX_FFI_TYPE_PTR: {
      void *lptr = READ(void*);
      if(!lptr) {
        ret = Qnil;
      } else {
        ret = MemoryPointer::create(state, lptr);
      }
      break;
    }
    case RBX_FFI_TYPE_STRING: {
      char* result = READ(char*);
      if(result == NULL) {
        ret = Qnil;
      } else {
        ret = String::create(state, result);
      }
      break;
    }
    case RBX_FFI_TYPE_STRPTR: {
      char* result;
      OBJECT s, p;

      result = READ(char*);

      if(result == NULL) {
        s = p = Qnil;
      } else {
        s = String::create(state, result);
        p = MemoryPointer::create(state, result);
      }

      Array* ary = Array::create(state, 2);
      ary->set(state, 0, s);
      ary->set(state, 1, p);
      ret = ary;
      break;
    }
    default:
    case RBX_FFI_TYPE_VOID:
      ret = Qnil;
      break;
    }

    return ret;
  }

  OBJECT MemoryPointer::set_at_offset(STATE, FIXNUM offset, FIXNUM type, OBJECT val) {
    set_field(state, offset->to_native(), type->to_native(), val);
    return val;
  }

  void MemoryPointer::set_field(STATE, int offset, int type, OBJECT val) {
    char* ptr = (char*)pointer;

    ptr += offset;

#define WRITE(type, val) *((type*)ptr) = (type)val

    switch(type) {
    case RBX_FFI_TYPE_CHAR:
      type_assert(val, FixnumType, "converting to char");
      WRITE(char, as<Fixnum>(val)->to_native());
      break;
    case RBX_FFI_TYPE_UCHAR:
      type_assert(val, FixnumType, "converting to unsigned char");
      WRITE(unsigned char, as<Fixnum>(val)->to_native());
      break;
    case RBX_FFI_TYPE_SHORT:
      type_assert(val, FixnumType, "converting to short");
      WRITE(short, as<Fixnum>(val)->to_native());
      break;
    case RBX_FFI_TYPE_USHORT:
      type_assert(val, FixnumType, "converting to unsigned short");
      WRITE(unsigned short, as<Fixnum>(val)->to_native());
      break;
    case RBX_FFI_TYPE_INT:
      if(FIXNUM_P(val)) {
        WRITE(int, as<Fixnum>(val)->to_int());
      } else {
        type_assert(val, BignumType, "converting to int");
        WRITE(int, as<Bignum>(val)->to_int());
      }
      break;
    case RBX_FFI_TYPE_UINT:
      if(FIXNUM_P(val)) {
        WRITE(unsigned int, as<Fixnum>(val)->to_uint());
      } else {
        type_assert(val, BignumType, "converting to unsigned int");
        WRITE(unsigned int, as<Bignum>(val)->to_uint());
      }
      break;
    case RBX_FFI_TYPE_LONG:
      if(FIXNUM_P(val)) {
        WRITE(long, as<Fixnum>(val)->to_long());
      } else {
        type_assert(val, BignumType, "converting to long");
        WRITE(long, as<Bignum>(val)->to_long());
      }
      break;
    case RBX_FFI_TYPE_ULONG:
      if(FIXNUM_P(val)) {
        WRITE(unsigned long, as<Fixnum>(val)->to_ulong());
      } else {
        type_assert(val, BignumType, "converting to unsigned long");
        WRITE(unsigned long, as<Bignum>(val)->to_ulong());
      }
      break;
    case RBX_FFI_TYPE_FLOAT: {
      Float* flt = as<Float>(val);
      type_assert(val, FloatType, "converting to float");
      WRITE(float, flt->to_double(state));
      break;
    }
    case RBX_FFI_TYPE_DOUBLE: {
      Float* flt = as<Float>(val);
      type_assert(val, FloatType, "converting to double");
      WRITE(double, flt->to_double(state));
      break;
    }
    case RBX_FFI_TYPE_LONG_LONG:
      if(FIXNUM_P(val)) {
        WRITE(long long, as<Fixnum>(val)->to_long_long());
      } else {
        type_assert(val, BignumType, "converting to long long");
        WRITE(long long, as<Bignum>(val)->to_long_long());
      }
      break;
    case RBX_FFI_TYPE_ULONG_LONG:
      if(FIXNUM_P(val)) {
        WRITE(unsigned long long, as<Fixnum>(val)->to_ulong_long());
      } else {
        type_assert(val, BignumType, "converting to unsigned long long");
        WRITE(unsigned long long, as<Bignum>(val)->to_ulong_long());
      }
      break;
    case RBX_FFI_TYPE_OBJECT:
      WRITE(OBJECT, val);
      break;
    case RBX_FFI_TYPE_PTR:
      if(NIL_P(val)) {
        WRITE(void*, NULL);
      } else {
        MemoryPointer *mp = as<MemoryPointer>(val);
        type_assert(val, MemPtrType, "converting to pointer");
        WRITE(void*, mp->pointer);
      }
      break;
    case RBX_FFI_TYPE_STRING: {
      char* result;
      if(NIL_P(val)) {
        result = NULL;
      } else {
        String* str = as<String>(val);
        /* TODO this is probably not correct. Saving away an 
         * internal pointer to the string means that when the string
         * moves, the data will point at the wrong place. Probably need to
         * copy the string data instead */
        result = str->c_str();
      }
      WRITE(char*, result);
      break;
    }
    default:
      sassert(0);
    }
  }

  void MemoryPointer::Info::mark(OBJECT obj, ObjectMark& mark) {
    // TODO: implement
  }
}