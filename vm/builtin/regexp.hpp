#ifndef RBX_REGEXP_HPP
#define RBX_REGEXP_HPP

#include "builtin/object.hpp"
#include "type_info.hpp"

// HACK gross.
// Forward declare ONLY if we haven't already included onig.h
// We do this because onigurama seems to have regex_t be a weird
// typedef. It's easier to just not bother with trying to duplicate
// what it does and do this.
#ifndef ONIGURUMA_H
struct regex_t;
#endif

namespace rubinius {
  class String;
  class Tuple;
  class LookupTable;

  class Regexp : public Object {
    public:
    const static size_t fields = 3;
    const static object_type type = RegexpType;

    String* source; // slot
    LookupTable* names; // slot
    regex_t* onig_data;

    static void cleanup(STATE, OBJECT data);
    static void init(STATE);
    static Regexp* create(STATE, String* pattern, INTEGER options, char* err_buf = NULL);
    static char*  version(STATE);

    // Ruby.primitive :regexp_options
    OBJECT options(STATE);

    // Ruby.primitive :regexp_search_region
    OBJECT match_region(STATE, String* string, INTEGER start, INTEGER end, OBJECT forward);

    // Ruby.primitive :regexp_match_start
    OBJECT match_start(STATE, String* string, INTEGER start);

    // Ruby.primitive :regexp_new
    static Regexp* new_expression(STATE, OBJECT self, String* pattern, INTEGER options);

    class Info : public TypeInfo {
    public:
      BASIC_TYPEINFO(TypeInfo)
      virtual void cleanup(OBJECT obj);
    };

  };

  class MatchData : public Object {
    public:
    const static size_t fields = 4;
    const static object_type type = MatchDataType;

    String* source; // slot
    Regexp* regexp; // slot
    Tuple* full; // slot
    Tuple* region; // slot

    class Info : public TypeInfo {
    public:
      BASIC_TYPEINFO(TypeInfo)
    };
  };

}

#endif