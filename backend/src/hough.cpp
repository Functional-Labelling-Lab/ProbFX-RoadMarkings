#include <iostream>
#include <math.h>
#include <opencv2/core.hpp>
#include <opencv2/highgui.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/imgproc.hpp>

#include "hough.h"

using namespace cv;
using namespace std;

const double CONE_HALF_ANGLE = 86 * (CV_PI / 180);

bool sufficiently_vertical(Vec4i l) {
  double angle = l[3] == l[1] ? CV_PI / 2 : atan((l[2] - l[0]) / (l[3] - l[1]));
  double bearing = angle < 0 ? angle + CV_2PI : angle;

  return bearing < CONE_HALF_ANGLE || bearing > (CV_2PI - CONE_HALF_ANGLE) ||
         (bearing > (CV_PI - CONE_HALF_ANGLE) &&
          bearing < (CV_PI + CONE_HALF_ANGLE));
}

detected_lines_t *detect_lines(const char *image) {
  std::string filename(image);

  Mat dst, cdst;
  Mat src = imread(filename, IMREAD_GRAYSCALE);

  if (src.empty()) {
    std::cout << " Error opening image\n" << std::endl;
    std::cout << " Program Arguments:" << filename << std::endl;

    exit(EXIT_FAILURE);
  } else {
    // Edge detection
    Canny(src, dst, 50, 200, 3);

    // Copy edges to the images that will display the results in BGR
    // cvtColor(dst, cdst, COLOR_GRAY2BGR);

    std::vector<Vec4i> linesP;
    HoughLinesP(dst, linesP, 1, CV_PI / 180, 50, 50, 10);

    // Draw the lines on the screen and display the image
    // for (size_t i = 0; i < linesP.size(); i++) {
    //   Vec4i l = linesP[i];
    //   line(cdst, Point(l[0], l[1]), Point(l[2], l[3]),
    //        sufficiently_vertical(l) ? Scalar(0, 255, 0) : Scalar(0, 0, 255),
    //        1, LINE_AA);
    // }

    std::vector<Vec4i> suff_vert_lines;
    std::copy_if(linesP.begin(), linesP.end(),
                 std::back_inserter(suff_vert_lines),
                 [](Vec4i v) { return sufficiently_vertical(v); });

    // Create malloc'd detected_lines struct
    detected_lines_t *new_dls =
        (detected_lines_t *)malloc(sizeof(detected_lines_t));
    if (new_dls == NULL) {
      exit(EXIT_FAILURE);
    }

    new_dls->len = suff_vert_lines.size();
    new_dls->lines =
        (hough_line_t *)malloc(suff_vert_lines.size() * sizeof(hough_line_t));
    if (new_dls->lines == NULL) {
      exit(EXIT_FAILURE);
    }

    for (size_t i = 0; i < suff_vert_lines.size(); i++) {
      Vec4i l = suff_vert_lines[i];
      new_dls->lines[i] = {
          .startX = l[0], .startY = l[1], .endX = l[2], .endY = l[3]};
    }

    // imshow("Detected Lines (in red) - Standard Hough Line Transform", cdst);
    // waitKey(0);

    return new_dls;
  }
}
