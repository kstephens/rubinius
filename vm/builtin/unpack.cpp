
/* This file was generated by Ragel. Your edits will be lost.
 *
 * This is a state machine implementation of String#unpack.
 *
 * vim: filetype=cpp
 */

#include "vm.hpp"
#include "object_utils.hpp"

#include "builtin/array.hpp"
#include "builtin/exception.hpp"
#include "builtin/string.hpp"

namespace rubinius {

#define UNPACK_ELEMENTS(format)                   \
  for(; index < stop; count--, index += width) {  \
    array->append(state, format(bytes + index));  \
  }

#define BYTE(p)  (Fixnum::from(*((signed char*)p)))

#define UBYTE(p) (Fixnum::from(*((unsigned char*)p)))

  Array* String::unpack(STATE, String* directives) {
    // Ragel-specific variables
    const char *p  = directives->c_str();
    const char *pe = p + directives->size();
    const char *eof = pe;
    int cs;

    // pack-specific variables
    uint8_t* bytes = byte_address();
    size_t index = 0;
    size_t stop = 0;
    size_t width = 0;
    int count = 0;
    bool rest = false;
    Array* array = Array::create(state, 0);


static const char _unpack_eof_actions[] = {
	0, 0, 2, 7, 8, 8, 11, 8, 
	2, 15, 2
};

static const int unpack_start = 1;
static const int unpack_first_final = 2;
static const int unpack_error = 0;

static const int unpack_en_main = 1;


	{
	cs = unpack_start;
	}

	{
	if ( p == pe )
		goto _test_eof;
	if ( cs == 0 )
		goto _out;
_resume:
	switch ( cs ) {
case 1:
	switch( (*p) ) {
		case 67: goto tr0;
		case 99: goto tr2;
	}
	goto tr1;
case 0:
	goto _out;
case 2:
	switch( (*p) ) {
		case 0: goto tr3;
		case 32: goto tr3;
		case 33: goto tr4;
		case 42: goto tr5;
		case 67: goto tr7;
		case 95: goto tr4;
		case 99: goto tr8;
	}
	if ( (*p) > 13 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto tr6;
	} else if ( (*p) >= 9 )
		goto tr3;
	goto tr1;
case 3:
	switch( (*p) ) {
		case 0: goto tr9;
		case 32: goto tr9;
		case 67: goto tr0;
		case 99: goto tr2;
	}
	if ( 9 <= (*p) && (*p) <= 13 )
		goto tr9;
	goto tr1;
case 4:
	switch( (*p) ) {
		case 0: goto tr10;
		case 32: goto tr10;
		case 33: goto tr11;
		case 42: goto tr12;
		case 67: goto tr14;
		case 95: goto tr11;
		case 99: goto tr15;
	}
	if ( (*p) > 13 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto tr13;
	} else if ( (*p) >= 9 )
		goto tr10;
	goto tr1;
case 5:
	switch( (*p) ) {
		case 0: goto tr10;
		case 32: goto tr10;
		case 67: goto tr14;
		case 99: goto tr15;
	}
	if ( 9 <= (*p) && (*p) <= 13 )
		goto tr10;
	goto tr1;
case 6:
	switch( (*p) ) {
		case 0: goto tr16;
		case 32: goto tr16;
		case 67: goto tr17;
		case 99: goto tr18;
	}
	if ( 9 <= (*p) && (*p) <= 13 )
		goto tr16;
	goto tr1;
case 7:
	switch( (*p) ) {
		case 0: goto tr10;
		case 32: goto tr10;
		case 67: goto tr14;
		case 99: goto tr15;
	}
	if ( (*p) > 13 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto tr19;
	} else if ( (*p) >= 9 )
		goto tr10;
	goto tr1;
case 8:
	switch( (*p) ) {
		case 0: goto tr3;
		case 32: goto tr3;
		case 67: goto tr7;
		case 99: goto tr8;
	}
	if ( 9 <= (*p) && (*p) <= 13 )
		goto tr3;
	goto tr1;
case 9:
	switch( (*p) ) {
		case 0: goto tr20;
		case 32: goto tr20;
		case 67: goto tr21;
		case 99: goto tr22;
	}
	if ( 9 <= (*p) && (*p) <= 13 )
		goto tr20;
	goto tr1;
case 10:
	switch( (*p) ) {
		case 0: goto tr3;
		case 32: goto tr3;
		case 67: goto tr7;
		case 99: goto tr8;
	}
	if ( (*p) > 13 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto tr23;
	} else if ( (*p) >= 9 )
		goto tr3;
	goto tr1;
	}

	tr1: cs = 0; goto _again;
	tr0: cs = 2; goto f0;
	tr7: cs = 2; goto f5;
	tr14: cs = 2; goto f9;
	tr17: cs = 2; goto f12;
	tr21: cs = 2; goto f16;
	tr9: cs = 3; goto _again;
	tr3: cs = 3; goto f2;
	tr10: cs = 3; goto f8;
	tr16: cs = 3; goto f11;
	tr20: cs = 3; goto f15;
	tr2: cs = 4; goto f0;
	tr8: cs = 4; goto f5;
	tr15: cs = 4; goto f9;
	tr18: cs = 4; goto f12;
	tr22: cs = 4; goto f16;
	tr11: cs = 5; goto f3;
	tr12: cs = 6; goto _again;
	tr13: cs = 7; goto f4;
	tr19: cs = 7; goto f13;
	tr4: cs = 8; goto f3;
	tr5: cs = 9; goto _again;
	tr6: cs = 10; goto f4;
	tr23: cs = 10; goto f13;

f0:
	{
    count = 1;
    rest = false;
  }
	goto _again;
f4:
	{
    count = (*p) - '0';
  }
	goto _again;
f13:
	{
    count = count * 10 + ((*p) - '0');
  }
	goto _again;
f3:
	{
#define NON_NATIVE_ERROR_SIZE 36

    char non_native_msg[NON_NATIVE_ERROR_SIZE];
    snprintf(non_native_msg, NON_NATIVE_ERROR_SIZE,
             "'%c' allowed only after types sSiIlL", *p);
    Exception::argument_error(state, non_native_msg);
  }
	goto _again;
f2:
	{
    width = 1;
  }
	{
    stop = rest ? size() + 1 : index + width * count;
    if(stop > size()) {
      stop = index + ((size() - index) / width) * width;
    }
  }
	{
    UNPACK_ELEMENTS(UBYTE);
  }
	{
    while(count > 0) {
      array->append(state, Qnil);
      count--;
    }
  }
	goto _again;
f8:
	{
    width = 1;
  }
	{
    stop = rest ? size() + 1 : index + width * count;
    if(stop > size()) {
      stop = index + ((size() - index) / width) * width;
    }
  }
	{
    UNPACK_ELEMENTS(BYTE);
  }
	{
    while(count > 0) {
      array->append(state, Qnil);
      count--;
    }
  }
	goto _again;
f15:
	{
    count = 0;
    rest = true;
  }
	{
    width = 1;
  }
	{
    stop = rest ? size() + 1 : index + width * count;
    if(stop > size()) {
      stop = index + ((size() - index) / width) * width;
    }
  }
	{
    UNPACK_ELEMENTS(UBYTE);
  }
	{
    while(count > 0) {
      array->append(state, Qnil);
      count--;
    }
  }
	goto _again;
f11:
	{
    count = 0;
    rest = true;
  }
	{
    width = 1;
  }
	{
    stop = rest ? size() + 1 : index + width * count;
    if(stop > size()) {
      stop = index + ((size() - index) / width) * width;
    }
  }
	{
    UNPACK_ELEMENTS(BYTE);
  }
	{
    while(count > 0) {
      array->append(state, Qnil);
      count--;
    }
  }
	goto _again;
f5:
	{
    width = 1;
  }
	{
    stop = rest ? size() + 1 : index + width * count;
    if(stop > size()) {
      stop = index + ((size() - index) / width) * width;
    }
  }
	{
    UNPACK_ELEMENTS(UBYTE);
  }
	{
    while(count > 0) {
      array->append(state, Qnil);
      count--;
    }
  }
	{
    count = 1;
    rest = false;
  }
	goto _again;
f9:
	{
    width = 1;
  }
	{
    stop = rest ? size() + 1 : index + width * count;
    if(stop > size()) {
      stop = index + ((size() - index) / width) * width;
    }
  }
	{
    UNPACK_ELEMENTS(BYTE);
  }
	{
    while(count > 0) {
      array->append(state, Qnil);
      count--;
    }
  }
	{
    count = 1;
    rest = false;
  }
	goto _again;
f16:
	{
    count = 0;
    rest = true;
  }
	{
    width = 1;
  }
	{
    stop = rest ? size() + 1 : index + width * count;
    if(stop > size()) {
      stop = index + ((size() - index) / width) * width;
    }
  }
	{
    UNPACK_ELEMENTS(UBYTE);
  }
	{
    while(count > 0) {
      array->append(state, Qnil);
      count--;
    }
  }
	{
    count = 1;
    rest = false;
  }
	goto _again;
f12:
	{
    count = 0;
    rest = true;
  }
	{
    width = 1;
  }
	{
    stop = rest ? size() + 1 : index + width * count;
    if(stop > size()) {
      stop = index + ((size() - index) / width) * width;
    }
  }
	{
    UNPACK_ELEMENTS(BYTE);
  }
	{
    while(count > 0) {
      array->append(state, Qnil);
      count--;
    }
  }
	{
    count = 1;
    rest = false;
  }
	goto _again;

_again:
	if ( cs == 0 )
		goto _out;
	if ( ++p != pe )
		goto _resume;
	_test_eof: {}
	if ( p == eof )
	{
	switch ( _unpack_eof_actions[cs] ) {
	case 7:
	{
    return array;
  }
	break;
	case 2:
	{
    width = 1;
  }
	{
    stop = rest ? size() + 1 : index + width * count;
    if(stop > size()) {
      stop = index + ((size() - index) / width) * width;
    }
  }
	{
    UNPACK_ELEMENTS(UBYTE);
  }
	{
    while(count > 0) {
      array->append(state, Qnil);
      count--;
    }
  }
	{
    return array;
  }
	break;
	case 8:
	{
    width = 1;
  }
	{
    stop = rest ? size() + 1 : index + width * count;
    if(stop > size()) {
      stop = index + ((size() - index) / width) * width;
    }
  }
	{
    UNPACK_ELEMENTS(BYTE);
  }
	{
    while(count > 0) {
      array->append(state, Qnil);
      count--;
    }
  }
	{
    return array;
  }
	break;
	case 15:
	{
    count = 0;
    rest = true;
  }
	{
    width = 1;
  }
	{
    stop = rest ? size() + 1 : index + width * count;
    if(stop > size()) {
      stop = index + ((size() - index) / width) * width;
    }
  }
	{
    UNPACK_ELEMENTS(UBYTE);
  }
	{
    while(count > 0) {
      array->append(state, Qnil);
      count--;
    }
  }
	{
    return array;
  }
	break;
	case 11:
	{
    count = 0;
    rest = true;
  }
	{
    width = 1;
  }
	{
    stop = rest ? size() + 1 : index + width * count;
    if(stop > size()) {
      stop = index + ((size() - index) / width) * width;
    }
  }
	{
    UNPACK_ELEMENTS(BYTE);
  }
	{
    while(count > 0) {
      array->append(state, Qnil);
      count--;
    }
  }
	{
    return array;
  }
	break;
	}
	}

	_out: {}
	}



    if(unpack_first_final && unpack_error && unpack_en_main) {
      // do nothing
    }

    return force_as<Array>(Primitives::failure());
  }
}
