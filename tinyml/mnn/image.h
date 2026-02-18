//
// Simple image data formats
//
// Not much memory, so thr best we can have is 64x64 8 bit greyscale.
// The memory limit is 4096 bytes.
//
// This is ANSI-C
//
// Created by Peter Köllner on 14/2/26.
//

#ifndef HAIKU6502_IMAGE_H
#define HAIKU6502_IMAGE_H

//
// 64x64 8-bit greyscale picture
// size: 4096 bytes
//
struct image64_grey {
    char pixels[64][64];
};

//
// 32x32 8-bit greyscale picture
// size: 1024 bytes
//
struct image32_grey {
    char pixels[32][32];
};

//
// 32x32 8-bit RGB picture
// size: 3072 bytes
//
struct image32_rgb {
    struct image32_grey color[3];
};

//
// 16x16 8-bit greyscale picture
// size: 256 bytes
//
struct image16_grey {
    char pixels[16][16];
};


//
// 16x16 8-bit RGB picture
// size: 768 bytes
//
struct image16_RGB {
    struct image16_grey color[3];
};

enum ImageType {
    IMG_64_G, IMG_32_G, IMG_32_RGB, IMG_16_G, IMG_16_RGB
};


#endif //HAIKU6502_IMAGE_H