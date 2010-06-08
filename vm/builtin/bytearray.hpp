#ifndef RBX_BUILTIN_BYTEARRAY_HPP
#define RBX_BUILTIN_BYTEARRAY_HPP

#include "builtin/object.hpp"
#include "type_info.hpp"

#include <ctype.h>

namespace rubinius {
  class ByteArray : public Object {
  public:
    const static object_type type = ByteArrayType;

  private:
    native_int full_size_;

    // Body access
    uint8_t bytes[0];

  public:
    static void init(STATE);
    static ByteArray* create(STATE, size_t bytes);
    static ByteArray* create_pinned(STATE, size_t bytes);

    template <typename Any>
      static ByteArray* from_body(Any obj) {
        ByteArray* ba = reinterpret_cast<ByteArray*>(obj);
        return ba - 1; // move back up to the pointer to the header
      }

    // Ruby.primitive :bytearray_allocate
    static ByteArray* allocate(STATE, Integer* bytes);

    // Ruby.primitive :bytearray_size
    Integer* size(STATE);

    // Return the number of bytes this ByteArray contains
    size_t size() {
      return full_size_ - sizeof(ByteArray);
    }

    uint8_t* raw_bytes() {
      return bytes;
    }

    // Ruby.primitive :bytearray_get_byte
    Fixnum* get_byte(STATE, Integer* index);

    // Ruby.primitive :bytearray_set_byte
    Fixnum* set_byte(STATE, Integer* index, Fixnum* value);

    // Ruby.primitive :bytearray_move_bytes
    Integer* move_bytes(STATE, Integer* start, Integer* count, Integer* dest);

    // Ruby.primitive :bytearray_fetch_bytes
    ByteArray* fetch_bytes(STATE, Integer* start, Integer* count);

    // Ruby.primitive :bytearray_compare_bytes
    Fixnum* compare_bytes(STATE, ByteArray* other, Integer* a, Integer* b);

    /* ::locate searches for +pattern+ in the ByteArray. Returns the
     * number of characters from the front of the ByteArray to the end
     * of the pattern if a match is found. Returns Qnil if a match is
     * not found. Starts searching at index +start+.
     */

    // Ruby.primitive :bytearray_locate
    Object* locate(STATE, String* pattern, Fixnum* start, Fixnum* max);

    // Ruby.primitive :bytearray_prepend
    ByteArray* prepend(STATE, String* other);

    // Ruby.primitive :bytearray_get_utf8_char
    Object* get_utf8_char(STATE, Fixnum* offset);

    char* to_chars(STATE);

    class Info : public TypeInfo {
    public:
      Info(object_type type, bool cleanup = false)
        : TypeInfo(type, cleanup)
      {
        allow_user_allocate = false;
      }

      virtual void mark(Object* t, ObjectMark& mark);
      virtual void auto_mark(Object* obj, ObjectMark& mark) {}
      virtual size_t object_size(const ObjectHeader* object);
    };

    friend class Info;
  };
};

#endif
