#!/usr/bin/env python3

import sky_mono

class Font:
    def __init__(self):
        self.width = self.height = self.start = self.length = 0
        self.bitmap: list[int] = []

class Crop:
    def __init__(self, x: int, y: int, width: int, height: int):
        self.x      = x
        self.y      = y
        self.width  = width
        self.height = height

def conv_font(font: Font, start: int, length: int, crop: Crop):
    data = []
    for i in range(start, length + start):
        glyph = []
        for x in range(crop.width):
            tmp = 0
            for y in range(crop.height):
                raw_row = font.bitmap[font.height * i + y + crop.y]
                tmp    |= ((raw_row >> (crop.x + x)) & 1) << (crop.height - y - 1)
            glyph.append(tmp)
        for j in range(crop.width):
            for k in range((crop.height + 7) // 8):
                data.append((glyph[j] >> (8 * k)) & 255)
    return bytes(data)

def dump_bytes(data: bytes, cols = 16):
    for y in range(0, len(data), cols):
        print("    .byte " + ", ".join(f"0x{x:02x}" for x in data[y:y+cols]))

dump_bytes(conv_font(sky_mono, 33, 127-33, Crop(0, 1, 7, 8)), 7)
