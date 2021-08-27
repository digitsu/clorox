//
// Created by digitsu on 2021/08/24.
//

#ifndef CLOROX_VALUE_H
#define CLOROX_VALUE_H

#include "common.h"

typedef double Value;

typedef struct {
    int capacity;
    int count;
    Value* values;
} ValueArray;

void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif //CLOROX_VALUE_H
