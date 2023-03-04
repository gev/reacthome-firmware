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



inline void pack_uint16_be (uint8_t *a, uint16_t i, int16_t v) {
    a[i]   = v >> 8;
    a[i+1] = v;
}

inline uint16_t unpack_uint16_be (uint8_t *a, uint16_t i) {
    return (uint16_t) ((a[i] << 8) | a[i+1]);
}

inline void pack_uint16_le (uint8_t *a, uint16_t i, int16_t v) {
    a[i]   = v;
    a[i+1] = v >> 8;
}

inline uint16_t unpack_uint16_le (uint8_t *a, uint16_t i) {
    return (uint16_t) ((a[i+1] << 8) | a[i]);
}


inline void pack_int16_be (uint8_t *a, uint16_t i, int16_t v) {
    pack_uint16_be(a, i, v);
}

inline uint16_t unpack_int16_be (uint8_t *a, uint16_t i) {
    return (uint16_t) unpack_uint16_be (a, i);
}

inline void pack_int16_le (uint8_t *a, uint16_t i, int16_t v) {
    pack_uint16_le(a, i, v);
}

inline uint16_t unpack_int16_le (uint8_t *a, uint16_t i) {
    return (uint16_t) unpack_uint16_le (a, i);
}



inline void pack_uint32_be (uint8_t *a, uint16_t i, int32_t v) {
    a[i]   = v >> 24;
    a[i+1] = v >> 16;
    a[i+2] = v >> 8;
    a[i+3] = v;
}

inline uint32_t unpack_uint32_be (uint8_t *a, uint16_t i) {
    return (uint32_t) ((a[i] << 24) | (a[i+1] << 16) | (a[i+2] << 8) | a[i+3]);
}

inline void pack_uint32_le (uint8_t *a, uint16_t i, int32_t v) {
    a[i]   = v;
    a[i+1] = v >> 8;
    a[i+2] = v >> 16;
    a[i+3] = v >> 24;
}

inline uint32_t unpack_uint32_le (uint8_t *a, uint16_t i) {
    return (uint32_t) ((a[i] | (a[i+1] << 8) | (a[i+2] << 16) | a[i+3] << 24));
}


inline void pack_int32_be (uint8_t *a, uint16_t i, int32_t v) {
    pack_uint32_be(a, i, v);
}

inline uint32_t unpack_int32_be (uint8_t *a, uint16_t i) {
    return (uint32_t) unpack_uint32_be (a, i);
}

inline void pack_int32_le (uint8_t *a, uint16_t i, int32_t v) {
    pack_uint32_le (a, i, v);
}

inline uint32_t unpack_int32_le (uint8_t *a, uint16_t i) {
    return (uint32_t) unpack_uint32_le (a, i);
}