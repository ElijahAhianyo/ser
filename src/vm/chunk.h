#ifndef ser_chunk_h
#define ser_chunk_h

#include "common.h"
#include "value.h"

typedef enum
{
    OP_CONSTANT,
    OP_RETURN,
} Opcode;

typedef struct
{
    int count;
    int capacity;
    u_int8_t *code;
    int *lines;
    ValueArray constants;
} Chunk;

void initChunk(Chunk *chunk);
void writeChunk(Chunk *chunk, u_int8_t byte, int line);
void freeChunk(Chunk *chunk);
int addConstant(Chunk *chunk, Value value);

#endif