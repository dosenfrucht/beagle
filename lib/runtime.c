#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

typedef void (*__fun)();

struct __val;

struct __val {
    uint8_t num_args;
    void *data;
    struct __node *args;
};

struct __node {
    struct __val v;
    struct __node *next;
};




static struct __val __val_stack[256];

static uint8_t __ip = 0;

struct __node *__create_node(struct __val a, struct __node *next)
{
    struct __node *n = malloc(sizeof(struct __node));
    n->v = a;
    n->next = next;
    return n;
}

uint8_t __num_args(struct __node *r)
{
    uint8_t n = 0;
    while (r != NULL) {
        n++;
        r = r->next;
    }
    return n;
}

struct __val __val_create_num(uint32_t n)
{
    struct __val v;
    v.num_args = 0;
    v.data = (void *) n;
    return v;
}

struct __val __val_create_str(char *s)
{
    struct __val v;
    v.num_args = 0;
    v.data = (void *) s;
    return v;
}

void __push_val(struct __val v)
{
    __val_stack[__ip] = v;
    __ip++;
}

void __push_num(uint32_t val)
{
    __push_val(__val_create_num(val));
}

void __push_str(char *s)
{
    __push_val(__val_create_str(s));
}

struct __val __read_val(uint8_t o)
{
    return __val_stack[__ip - (o + 1)];
}

void __pop()
{
    __ip--;
}

uint32_t __pop_num(uint8_t o)
{
    uint32_t val = (uint32_t) __read_val(0).data;
    __pop();
    return val;
}

void __args_to_stack(struct __node *n)
{
    while (n != NULL) {
        __push_val(n->v);
        n = n->next;
    }
    return;
}

void __call_once()
{
    struct __val v = __read_val(0);
    struct __val a = __read_val(1);
    v.args = __create_node(a, v.args);
    __pop();
    __pop();
    __push_val(v);
    if (__num_args(v.args) == v.num_args) {
        // move args to stack
        __pop();
        __args_to_stack(v.args);
        ((__fun) (v.data))();
        // free the args?

        // i think i have to ask the nonexistent garbage collector first
    }
}

void __call(uint8_t n)
{
    while (n > 0) {
        __call_once();
        n--;
    }
}

void __unwind(uint8_t o)
{
    struct __val v = __read_val(0);
    __pop();
    while (o > 0) {
        __pop();
        o--;
    }
    __push_val(v);
}





void __add__()
{
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint32_t) a.data + (uint32_t) b.data);
    __unwind(2);
    return;
}

struct __val builtin__add__builtin()
{
    struct __val v = {};
    v.num_args = 2;
    v.data = __add__;
    return v;
}


void __sub__()
{
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint32_t) a.data - (uint32_t) b.data);
    __unwind(2);
    return;
}

struct __val builtin__sub__builtin()
{
    struct __val v = {};
    v.num_args = 2;
    v.data = __sub__;
    return v;
}


void __mult__()
{
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint32_t) a.data * (uint32_t) b.data);
    __unwind(2);
    return;
}

struct __val builtin__mult__builtin()
{
    struct __val v = {};
    v.num_args = 2;
    v.data = __mult__;
    return v;
}


void __div__()
{
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint32_t) a.data / (uint32_t) b.data);
    __unwind(2);
    return;
}

struct __val builtin__div__builtin()
{
    struct __val v = {};
    v.num_args = 2;
    v.data = __div__;
    return v;
}


void __mod__()
{
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint32_t) a.data % (uint32_t) b.data);
    __unwind(2);
    return;
}

struct __val builtin__mod__builtin()
{
    struct __val v = {};
    v.num_args = 2;
    v.data = __mod__;
    return v;
}


void __equal__()
{
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint32_t) a.data == (uint32_t) b.data);
    __unwind(2);
    return;
}

struct __val builtin__equal__builtin()
{
    struct __val v = {};
    v.num_args = 2;
    v.data = __equal__;
    return v;
}


void __notequal__()
{
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint32_t) a.data != (uint32_t) b.data);
    __unwind(2);
    return;
}

struct __val builtin__notequal__builtin()
{
    struct __val v = {};
    v.num_args = 2;
    v.data = __notequal__;
    return v;
}


void __shiftl__()
{
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint32_t) a.data << (uint32_t) b.data);
    __unwind(2);
    return;
}

struct __val builtin__shiftl__builtin()
{
    struct __val v = {};
    v.num_args = 2;
    v.data = __shiftl__;
    return v;
}


void __shiftr__()
{
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint32_t) a.data >> (uint32_t) b.data);
    __unwind(2);
    return;
}

struct __val builtin__shiftr__builtin()
{
    struct __val v = {};
    v.num_args = 2;
    v.data = __shiftr__;
    return v;
}


void __and__()
{
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint32_t) a.data & (uint32_t) b.data);
    __unwind(2);
    return;
}

struct __val builtin__and__builtin()
{
    struct __val v = {};
    v.num_args = 2;
    v.data = __and__;
    return v;
}


void __or__()
{
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint32_t) a.data | (uint32_t) b.data);
    __unwind(2);
    return;
}

struct __val builtin__or__builtin()
{
    struct __val v = {};
    v.num_args = 2;
    v.data = __or__;
    return v;
}


void __xor__()
{
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint32_t) a.data ^ (uint32_t) b.data);
    __unwind(2);
    return;
}

struct __val builtin__xor__builtin()
{
    struct __val v = {};
    v.num_args = 2;
    v.data = __xor__;
    return v;
}


void __lower__()
{
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint32_t) a.data < (uint32_t) b.data);
    __unwind(2);
    return;
}

struct __val builtin__lower__builtin()
{
    struct __val v = {};
    v.num_args = 2;
    v.data = __lower__;
    return v;
}


void __greater__()
{
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint32_t) a.data < (uint32_t) b.data);
    __unwind(2);
    return;
}

struct __val builtin__greater__builtin()
{
    struct __val v = {};
    v.num_args = 2;
    v.data = __greater__;
    return v;
}
