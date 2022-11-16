#include <iostream>
#include <math.h>
#include <opencv2/core.hpp>
#include <opencv2/highgui.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/imgproc.hpp>

using namespace cv;
using namespace std;

// *
double CONE_HALF_ANGLE = 86 * (CV_PI / 180);

bool sufficiently_vertical(Vec4i l) {
  double angle = l[3] == l[1] ? CV_PI / 2 : atan((l[2] - l[0]) / (l[3] - l[1]));
  double bearing = angle < 0 ? angle + CV_2PI : angle;

  // Upper cone
  if (bearing < CONE_HALF_ANGLE || bearing > (CV_2PI - CONE_HALF_ANGLE)) {
    return true;
  }

  // Lower cone
  if (bearing > (CV_PI - CONE_HALF_ANGLE) &&
      bearing < (CV_PI + CONE_HALF_ANGLE)) {
    return true;
  }

  return false;
}

int main(int argc, char **argv) {
  // Declare the output variables
  Mat dst, cdst;
  std::string filename(argv[1]);
  Mat src = imread(filename, IMREAD_GRAYSCALE);

  // Check if image is loaded fine
  if (src.empty()) {
    std::cout << " Error opening image\n" << std::endl;
    std::cout << " Program Arguments:" << filename << std::endl;
    return -1;
  }
  // Edge detection
  Canny(src, dst, 50, 200, 3);

  // Copy edges to the images that will display the results in BGR
  cvtColor(dst, cdst, COLOR_GRAY2BGR);

  // Probabilistic Line Transform
  std::vector<Vec4i> linesP;
  HoughLinesP(dst, linesP, 1, CV_PI / 180, 50, 50, 10);
  // Draw the lines
  for (size_t i = 0; i < linesP.size(); i++) {
    Vec4i l = linesP[i];
    if (sufficiently_vertical(l)) {
      line(cdst, Point(l[0], l[1]), Point(l[2], l[3]), Scalar(0, 255, 0), 1,
           LINE_AA);
    } else {
      line(cdst, Point(l[0], l[1]), Point(l[2], l[3]), Scalar(0, 0, 255), 1,
           LINE_AA);
    }
  }

  // imshow("Source", src);
  imshow("Detected Lines (in red) - Standard Hough Line Transform", cdst);

  // Wait and Exit
  waitKey(0);

  for (;;)
    ;

  return 0;
}
