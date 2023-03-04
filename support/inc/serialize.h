#ifndef __SERIALIZE_H_
#define __SERIALIZE_H_

#include <stdint.h>


static inline void pack_uint8 (uint8_t *a, int32_t i, uint8_t v) {
    a[i] = v;
}

static inline uint8_t unpack_uint8 (uint8_t *a, int32_t i) {
    return a[i];
}


static inline void pack_sint8 (uint8_t *a, int32_t i, int8_t v) {
    pack_uint8(a, i, (uint8_t) v);
}

static inline int8_t unpack_sint8 (uint8_t *a, int32_t i) {
    return (int8_t) unpack_uint8(a, i);
}



static inline void pack_uint16_be (uint8_t *a, int32_t i, uint16_t v) {
    a[i]   = v >> 8;
    a[i+1] = v;
}

static inline uint16_t unpack_uint16_be (uint8_t *a, int32_t i) {
    uint16_t b = 0;
    b = a[i];
    b <<= 8;
    b |= a[i+1];
    return b;
}

static inline void pack_uint16_le (uint8_t *a, int32_t i, uint16_t v) {
    a[i]   = v;
    a[i+1] = v >> 8;
}

static inline uint16_t unpack_uint16_le (uint8_t *a, int32_t i) {
    uint16_t b = 0;
    b = a[i+1];
    b <<= 8;
    b |= a[i];
    return b;
}


static inline void pack_sint16_be (uint8_t *a, int32_t i, int16_t v) {
    pack_uint16_be(a, (uint16_t)i, v);
}

static inline uint16_t unpack_sint16_be (uint8_t *a, int32_t i) {
    return (uint16_t) unpack_uint16_be (a, i);
}

static inline void pack_sint16_le (uint8_t *a, int32_t i, int16_t v) {
    pack_uint16_le(a, (uint16_t)i, v);
}

static inline uint16_t unpack_sint16_le (uint8_t *a, int32_t i) {
    return (uint16_t) unpack_uint16_le (a, i);
}



static inline void pack_uint32_be (uint8_t *a, int32_t i, uint32_t v) {
    a[i]   = v >> 24;
    a[i+1] = v >> 16;
    a[i+2] = v >> 8;
    a[i+3] = v;
}

static inline uint32_t unpack_uint32_be (uint8_t *a, int32_t i) {
    uint32_t b = 0;
    b = a[i];
    b <<= 8;
    b |= a[i+1];
    b <<= 8;
    b |= a[i+2];
    b <<= 8;
    b |= a[i+3];
    return b;
}

static inline void pack_uint32_le (uint8_t *a, int32_t i, uint32_t v) {
    a[i]   = v;
    a[i+1] = v >> 8;
    a[i+2] = v >> 16;
    a[i+3] = v >> 24;
}

static inline uint32_t unpack_uint32_le (uint8_t *a, int32_t i) {
    uint32_t b = 0;
    b = a[i+3];
    b <<= 8;
    b |= a[i+2];
    b <<= 8;
    b |= a[i+1];
    b <<= 8;
    b |= a[i];
    return b;
}


static inline void pack_sint32_be (uint8_t *a, int32_t i, int32_t v) {
    pack_uint32_be(a, (uint32_t) i, v);
}

static inline int32_t unpack_sint32_be (uint8_t *a, int32_t i) {
    return (int32_t) unpack_uint32_be (a, i);
}

static inline void pack_sint32_le (uint8_t *a, int32_t i, int32_t v) {
    pack_uint32_le (a, (uint16_t) i, v);
}

static inline int32_t unpack_sint32_le (uint8_t *a, int32_t i) {
    return (int32_t) unpack_uint32_le (a, i);
}

#endif
