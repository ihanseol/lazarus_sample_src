mandelbrot.exe readme
---------------------

For explanations of the mathematical background of the Mandelbrot set, please, search the Internet
(e.g. the Wiki site: https://en.wikipedia.org/wiki/Mandelbrot_set).

General notes:
--------------

  1. The program runs in 2 windows, the first for data entry, the second to display the graphical
     representation of the Mandelbrot set. After the set has been displayed, you have to close the
     graphics window (using the 'X' at its top right corner) before proceeding.

  2. The format of the number values entered depends on your operating system's regional settings:
     whereas in USA you use a dot (.) as decimal separator, in Europe it's normally a comma (,) that
     separates the integer and fractional parts of a decimal number.

Data entry:
-----------

  1. The number of iterations.
     It affects the precision of the set with more correct colors used. In practice this seems not
     to change a lot with this (very simple) program. Just hitting ENTER, which corresponds to 256
     iterations (which is also the number of different colors used) is mostly fine.

  2. The zoom factor.
     It allows to magnify (= to zoom into) the displayed region. Together with the starting X-value,
     it determines what the picture displayed will look like.
	 
  3. The starting X-value within the Mandelbrot set scale.
     The Mandelbrot set scale X-values are from -2.5 to 1. Thus, entering -2.5 starts the graphical
     display at the beginning of the set and with zoom factor = 1 displays the entire set at the
     center of the screen. Using values > -2.5 is moving the starting X-value to the right, what
     has the effect to "shift" the graphics to the left. Modifying the zoom factor at the same time,
     you will display different regions of the set with different magnitudes, producing lots of
     completely different colorful pictures.

Examples:
---------

  Here some starting X-values / zoom factor pairs, producing nice (and quite different) pictures:
  
    X-value = -2.5     zoom =   1   the entire set
    X-value = -1,8     zoom =  60   the first of the 3 circular black regions
    X-value = -1,425   zoom = 150   the smaller black regions at the left of the set
    X-value = -1,42    zoom = 750   enlargement (zoom-in) of the previous example

  Just try it out. And enjoy!
