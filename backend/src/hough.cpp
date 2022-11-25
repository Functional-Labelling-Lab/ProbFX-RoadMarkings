#include <iostream>
#include <math.h>
#include <opencv2/core.hpp>
#include <opencv2/highgui.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/imgproc.hpp>

#include "hough.h"

const double CONE_HALF_ANGLE = 86 * (CV_PI / 180);

bool sufficiently_vertical(cv::Vec4i l) {
  double angle = l[3] == l[1] ? CV_PI / 2 : atan((l[2] - l[0]) / (l[3] - l[1]));
  double bearing = angle < 0 ? angle + CV_2PI : angle;

  return bearing < CONE_HALF_ANGLE || bearing > (CV_2PI - CONE_HALF_ANGLE) ||
         (bearing > (CV_PI - CONE_HALF_ANGLE) &&
          bearing < (CV_PI + CONE_HALF_ANGLE));
}

detected_lines_t *detect_lines(const char *image) {
  std::string filename(image);

  cv::Mat dst, cdst;
  cv::Mat src = cv::imread(filename, cv::IMREAD_GRAYSCALE);

  if (src.empty()) {
    std::cout << " Error opening image\n" << std::endl;
    std::cout << " Program Arguments:" << filename << std::endl;
    exit(EXIT_FAILURE);
  }

  std::vector<cv::Vec4i> linesP;
  std::vector<cv::Vec4i> suff_vert_lines;

  cv::Canny(src, dst, 50, 200, 3);
  cv::HoughLinesP(dst, linesP, 1, CV_PI / 180, 50, 50, 10);
  std::copy_if(linesP.begin(), linesP.end(),
               std::back_inserter(suff_vert_lines),
               [](cv::Vec4i v) { return sufficiently_vertical(v); });

  detected_lines_t *new_dls =
      (detected_lines_t *)malloc(sizeof(detected_lines_t));

  if (!new_dls)
    exit(EXIT_FAILURE);

  new_dls->len = suff_vert_lines.size();
  new_dls->lines =
      (hough_line_t *)malloc(suff_vert_lines.size() * sizeof(hough_line_t));

  if (!new_dls->lines)
    exit(EXIT_FAILURE);

  for (size_t i = 0; i < suff_vert_lines.size(); i++) {
    cv::Vec4i l = suff_vert_lines[i];
    new_dls->lines[i] = {
        .startX = l[0], .startY = l[1], .endX = l[2], .endY = l[3]};
  }

  return new_dls;
}
