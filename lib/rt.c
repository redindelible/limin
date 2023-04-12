#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

// A trace function should move all objects inside to the gray set
typedef void (*ObjTraceFn)(void*);

typedef void (*FrameTraceFn)(void*);

struct TypeInfo {
    ObjTraceFn trace;
    uint64_t size;
};

struct ObjectHeader {
    struct TypeInfo* info;
    struct ObjectHeader* next;
    uint8_t mark;
};

struct Frame {
    struct Frame* parent;
    FrameTraceFn trace;
    uint64_t stack_size;
    uint8_t frame[];
};

struct GCState {
    uint8_t current_black;
    struct ObjectHeader* white;
    struct ObjectHeader* gray;
    struct ObjectHeader* black;
};

struct GCState gc_state;


void limin_mark_object(struct ObjectHeader* obj) {
    if (obj == NULL || obj->mark == 2 || obj->mark == gc_state.current_black) {
        // already marked, continue
    } else {
        obj->mark = 2;
        obj->next = gc_state.gray;
        gc_state.gray = obj;
    }
}

void* limin_create_object(struct TypeInfo* type, struct Frame* frame) {
    struct ObjectHeader* obj = malloc(sizeof(struct ObjectHeader) + type->size);
    obj->info = type;
    obj->mark = 2;
    obj->next = gc_state.gray;
    gc_state.gray = obj;
    return obj;
}

void limin_trace_stack(struct ObjectHeader* stack, uint64_t count) {
    for (uint64_t i = 0; i < count; i++) {
        limin_mark_object(&stack[i]);
    }
}

static void process_next() {
    if (gc_state.gray == NULL) {
        // start freeing
        return;
    }
    struct ObjectHeader* next = gc_state.gray;
    gc_state.gray = next->next;
    next->info->trace(next);

    next->mark = gc_state.current_black;
    next->next = gc_state.black;
    gc_state.black = next;
}
