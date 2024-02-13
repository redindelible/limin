#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

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
    uint8_t object[];
};

struct Frame {
    struct Frame* parent;
    FrameTraceFn trace;
    uint32_t frame_state;
    uint8_t frame[];
};

struct GCState {
    uint8_t current_black;
    struct ObjectHeader* white;
    struct ObjectHeader* gray;
    struct ObjectHeader* black;
};

struct GCState gc_state;


#define TO_HEADER(ptr) ((struct ObjectHeader*) ((void*)(ptr) - sizeof(struct ObjectHeader)))
#define TO_OBJECT(ptr) ((void*)(ptr) + sizeof(struct ObjectHeader))

void limin_mark_object(void* obj_ptr) {
    struct ObjectHeader* obj = TO_HEADER(obj_ptr);
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
    memset(obj, 0, sizeof(struct ObjectHeader) + type->size);
    obj->info = type;
    obj->mark = 2;
    obj->next = gc_state.gray;
    gc_state.gray = obj;
    return TO_OBJECT(obj);
}

static void process_next() {
    if (gc_state.gray == NULL) {
        // start freeing
        return;
    }
    struct ObjectHeader* next = gc_state.gray;
    gc_state.gray = next->next;
    next->info->trace(next->object);

    next->mark = gc_state.current_black;
    next->next = gc_state.black;
    gc_state.black = next;
}

void limin_trace_none(void* obj) {

}

void limin_trace_gc(void** obj_ptr) {
    void* obj = *obj_ptr;
    struct ObjectHeader* header = TO_HEADER(obj);
    header->info->trace(obj);
}