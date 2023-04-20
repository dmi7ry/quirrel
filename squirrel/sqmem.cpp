/*
    see copyright notice in squirrel.h
*/
#include "sqpcheader.h"
#ifndef SQ_EXCLUDE_DEFAULT_MEMFUNCTIONS

void sq_vm_init_alloc_context(SQAllocContext *) {}
void sq_vm_destroy_alloc_context(SQAllocContext *) {}
void sq_vm_assign_to_alloc_context(SQAllocContext, HSQUIRRELVM) {}

void *sq_vm_malloc(SQAllocContext ctx, SQUnsignedInteger size) { 
    return ctx ? ctx->memAlloc(size) : malloc(size);
}

void *sq_vm_realloc(SQAllocContext ctx, void *p, SQUnsignedInteger oldsize, SQUnsignedInteger size) {
    return ctx ? ctx->memRealloc(p, oldsize, size) : realloc(p, size);
}

void sq_vm_free(SQAllocContext ctx, void *p, SQUnsignedInteger SQ_UNUSED_ARG(size)) {
    ctx ? ctx->memFree(p) : free(p);
}
#endif
