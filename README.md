LISP routine for AutoCAD 3D (2020) that calculates and marks the center of gravity of one or more 3D solid elements that may or may not be of differing material densities (concrete, steel, etc.).

This was initially written to assist in the planning and calculation for erection / lifting procedures.

Dialog boxes are defined in the *.DCL file.

Routine does the following:
- prompt user to select one or more 3D solids
- note the centroid, volume, and layer of each element
- initiate a dialog box for the user to enter the densities (lb / cu. ft. or other) of each noted layer
- calculate the composite center of gravity
- export the information to a CSV file for documentation and verification

Video of sample use: https://youtu.be/bLOZLg_tG28
