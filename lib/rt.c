#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

typedef void (*TraceFn)(void*);

struct TypeInfo {
    TraceFn fn;
    size_t size;
};

struct ObjectHeader {
    struct TypeInfo* info;
};

struct Object {
    struct ObjectHeader header;
    struct Object* foo;
    struct Object* bar;
};

void* limin_create_object(struct TypeInfo* type) {
    struct ObjectHeader* obj = malloc(sizeof(struct ObjectHeader) + type->size);
    obj->info = type;
    return obj;
}

uint32_t test_print() {
    printf("hey\n");
    return 0;
}