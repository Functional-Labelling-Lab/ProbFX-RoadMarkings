#ifndef HOUGH_H
#define HOUGH_H

#include <vector>

struct hough_line {
    int startX, startY, endX, endY;
};

typedef struct hough_line hough_line_t;

struct detected_lines {
    hough_line_t* lines;
    int len;
};

typedef struct detected_lines detected_lines_t;

detected_lines_t *detect_lines(const char *image);

#endif
