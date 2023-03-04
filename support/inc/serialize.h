

#include <stdint.h>


inline void pack_uint8 (uint8_t *a, uint16_t i, uint8_t v) {
    a[i] = v;
}

inline uint8_t unpack_uint8 (uint8_t *a, uint16_t i) {
    return a[i];
}





inline void pack_int8 (uint8_t *a, uint16_t i, int8_t v) {
    pack_uint8(a, i, (uint8_t) v);
}

inline int8_t unpack_int8 (uint8_t *a, uint16_t i) {
    return (int8_t) unpack_uint8(a, i);
}



pack_uint16_be
unpack_uint16_be

pack_uint16_le
unpack_uint16_le
