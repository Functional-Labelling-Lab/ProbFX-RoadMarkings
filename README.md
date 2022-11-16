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
├── README.md     # This readme
|
├── ProbFX-RoadMarkings.cabal  # Cabal configuration
├── Setup.hs                   # Hooks for running C++ side build
├── cabal.project              # ProbFX dependency
|
├── backend ── ... # The C++ side of the project
├── app ────── ... # The main 
└── src ────── ... # The Haskell side of the project (contains bindings, logic)
```

## Repo Goals (Short Term)
- Implement basic image transformations (hough, edge detections, filtering etc)
- Implement a basic model for road geometry
- Improve basic equation to error function check routines
- Implement our own metropolis hastings to improve/refine model parameters on a single image


## Building The Project
### System Dependencies
On Ubuntu ensure you have installed the following:
```bash
sudo apt-get install libx11-dev mesa-common-dev libgl1-mesa-dev libglu1-mesa-dev libxinerama1 libxinerama-dev libxcursor-dev libxrandr-dev libxi-dev libxmu-dev libblas-dev
```

### Build & Clean
```bash
cabal build
cabal run
cabal clean
```
