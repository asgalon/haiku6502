//
// Created by Peter Köllner on 14/2/26.
//

#ifndef HAIKU6502_FILTER_H
#define HAIKU6502_FILTER_H

#include "image.h"

struct filter {
    char weights[9];
    char bias;
};

extern struct filter *filter_create();
extern char filter_apply(struct filter *f, ImageType type, void *img, unsigned char row, unsigned char col, unsigned char color = 0);

#endif //HAIKU6502_FILTER_H