Mathematical images: Peter de Jong attractors.
==============================================

Transforming mathematical content into images is rather simple to realize with computer programs and produces astonishing pictures; the beauty of maths,
I would call it. The Peter de Jong attractors are series of points P(x,y), where the coordinates of the points are calculated from those of the previous
point in the series. They can be described by the following system of two equations:
  X[n+1] = sin(aY[n]) - cos(bX[n]
  Y[n+1] = sin(cX[n]) - cos(dY[n]
and setting, for example X[0] = Y[0] = 0.

"dejong" is a very simple command line program, that may be used to draw the de Jong attractors pictures. All you have to do, is to enter the parameters
a, b, c and d (and the coordinates of the drawing center; cf. below). Just try it out - and enjoy!

Equations parameters.
---------------------
Here some examples of parameters (in fact those, used to produce the pictures on my website - www.streetinfo.lu/computing/lazarus/doc/dejong_pics.html).
    a      b      c      d     center
  ---------------------------------------
   1.5    2.0    0.5    1.5    (0,50)
  -2.5   -0.1   -1.0   -2.0    (240,-100)
  -0.8   -1.6    1.6   -1.0    (25,75)
   2.0   -2.0   -1.0   -2.0    (0,0)
   2.0    2.0    1.2    2.0    (0,0)
   1.5   -2.5    2.5   -2.0    (0,0)
  -2.0   -2.5    1.5   -0.4    (-50,150)
   1.0   -2.0    1.5   -1.5    (0,0)
   0.5   -2.0    2.5   -1.5    (25,25)
Of course, an infinity of other possible images...

Centring the drawing.
---------------------
The images may be centered manually, by entering the coordinates of the drawing surface center. The default (values used, if you just hit ENTER, when asked
for the center coordinates) is C(0,0). Entering a value for X, will displace the center, and thereby the image horizontally; a positive value moves the image
towards the right. Entering a value for Y, will displace the center vertically; a positive value moves the image towards the top (usual orientation of the
Y-axis in maths). Thus, the simplest way to proceed is to draw the image with center C(0,0) and based on the dissplay, redo the drawing, by entering appropriate
center coordinates. Note, that you will not have to re-enter the parameter values. In fact, entering nothing for a paramter (just hitting ENTER) will result in
that parameter keeping its value from before.

                                                                                                                     "dejong", version 1.0, © allu, February 2021.
