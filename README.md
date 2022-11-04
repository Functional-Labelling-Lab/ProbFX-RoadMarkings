![probFX-RoadMarkings](https://user-images.githubusercontent.com/44177991/196529156-eb616529-baff-4b82-b078-9d083787d3cf.png)

## What is this?
This repository contains our attempt at developing a working model for auto labelling road marking geometry on images.
- Indending to use ProbFX (or a derivative) to allow for ergonomic & reusable models
- Currently using the [Haskell Image Processing Library (HIP)](https://github.com/lehins/hip) for image handling (currently unstable, so looking to move to an [openCV](https://opencv.org/) binding)

## How will this work?
The long-term goal is to build the pipeline described below...

![Pipeline drawio](https://user-images.githubusercontent.com/44177991/196551636-ffc268d2-25db-4bad-8f2f-d19663754dfd.png)


## File Structure
```bash
├── CHANGELOG.md
├── LICENSE
├── ProbFX-Examples.cabal # Cabal dependencies and setup
├── README.md             # This readme
├── app
│   ├── Image.hs          # Image functions (generating images from equations parameterised by samples from model)
│   └── Main.hs           # Main function to interact with images
└── cabal.project
```

## Repo Goals (Short Term)
- Implement basic image transformations (hough, edge detections, filtering etc)
- Implement a basic model for road geometry
- Improve basic equation to error function check routines
- Implement our own metropolis hastings to improve/refine model parameters on a single image


## Building The Project
### Dependencies
On Ubuntu ensure you have installed the following:
```bash
sudo apt-get install libx11-dev
sudo apt-get install mesa-common-dev libgl1-mesa-dev libglu1-mesa-dev
sudo apt-get install libxinerama1 libxinerama-dev
sudo apt-get install libxcursor-dev
sudo apt-get install libxrandr-dev
sudo apt-get install libxi-dev
sudo apt-get install libxmu-dev
sudo apt-get install libblas-dev
```

And then, you'll need to run the following script to install glad:
```bash
sudo bash setup.sh
```

Finally, run the example with Cabal:
```bash
cabal update
cabal run
```