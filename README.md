# raylib-Ada

Ada binding for the Raylib library.
Many functions are missing and some are not tested !

## Prerequisites

- GNAT toolsuite : [www.adacore.com](https://www.adacore.com/community)
- Raylib 4.5 : [raysan5/raylib](https://github.com/raysan5/raylib)

## Development

Porting examples (full list [here](https://www.raylib.com/examples.html)) :

- audio
  - [x] sound loading and playing
- core
  - [x] input gamepad
  - [ ] input keys
  - [ ] input mouse
  - [ ] input mouse_wheel
  - [x] 3d camera first person
  - [ ] 2d camera
- shapes
  - [ ] collision area
  - [ ] draw ring
  - [ ] draw rectangle_rounded
- textures
  - [x] rectangle
  - [ ] srcrec dstrec
  - [ ] sprite button
- text
  - [ ] format text
  - [x] input box
  - [ ] rectangle bounds
- models
  - [ ] box collisions
