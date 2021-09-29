//
// Created by digitsu on 2021/08/26.
//

#ifndef CLOROX_COMPILER_H
#define CLOROX_COMPILER_H

#include "object.h"
#include "vm.h"

ObjFunction* compile(const char* source);
void markCompilerRoots();

#endif //CLOROX_COMPILER_H
