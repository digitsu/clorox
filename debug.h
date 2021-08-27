//
// Created by digitsu on 2021/08/24.
//

#ifndef CLOROX_DEBUG_H
#define CLOROX_DEBUG_H

#include "chunk.h"

void disassembleChunk(Chunk* chunk, const char* name);
int disassembleInstruction(Chunk* chunk, int offset);

#endif //CLOROX_DEBUG_H
