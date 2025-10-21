#ifndef ser_debug_h
#define ser_debug_h

#include <stdio.h>
#include "chunk.h"

void disassembleChunk(Chunk *chunk, const char *name);
int disassembleInstruction(Chunk *chunk, int offset);

#endif