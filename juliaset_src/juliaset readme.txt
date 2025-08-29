juliaset.exe readme
---------------------

For explanations of the mathematical background of the Julia set, please, search the Internet
(e.g. the Wiki site: https://en.wikipedia.org/wiki/Julia_set). Quadratic Julia sets are defined
by z(n+1) = z(n)^2 + C, where C is a complex constant, which will determine the shape of the
corresponding Julia set graphical representation.

General notes:
--------------

  1. The program runs in 2 windows, the first for data entry, the second to display the graphical
     representation of the Julia set. After the set has been displayed, you have to close the
     graphics window (using the 'X' at its top right corner) before proceeding.

  2. The format of the number values entered depends on your operating system's regional settings:
     whereas in USA you use a dot (.) as decimal separator, in Europe it's normally a comma (,) that
     separates the integer and fractional parts of a decimal number.

Data entry:
-----------

  1. The complex constant C.
     It determines the shape of the Julia set. It has to be entered as 2 real numbers, corresponding
	 to the real respectively imaginary part of the complex.

  2. The zoom factor.
     It allows to magnify (= to zoom into) the displayed region. Together with the starting X- and
	 Y-value, it determines what the picture displayed will look like.
	 
  3. The starting X- and Y-value within the Julia set set scale.
     The Julia set scale X- and Y-values are from -2.0 to 2.0. Thus, entering -2.0 for both X and Y-value
	 starts the graphical display at the beginning of the set and with zoom factor = 1 displays the entire
	 set at the center of the screen. Using X-values > -2.0 is moving the starting X-value to the right,
	 what has the effect to "shift" the graphics leftwards; using Y-values > -2.0 is moving the starting
	 Y-value to the top, what has the effect to "shift" the graphics downwards. Modifying the zoom factor
	 at the same time, you will display different regions of the set with different magnitudes, producing
	 lots of different pictures for a given set (defined by the value of C).

Examples:
---------

  1. You may want to see how different values of C produce different shapes by trying integer numbers from
     C = -2 to C = 2 (the imaginary part of real numbers being 0)
	 
  2. Trying the zoom and changing starting X- and Y-values for C = -1,5 with:
       - zoom =  4
	   - zoom = 10; X0 = -1,2 and Y0 = -1,5
  
  3. Some special sets:
      C = i (real part = 0; imaginary part = 1): dendrite fractal
	  C = -0.75: San Marco fractal 
	  C = -0.123 + 0.745i: Douady's rabbit fractal	  
	  C = -0,391 - 0,587i: Siegel disk fractal (try it with zoom = 2 and zoom = 3...)
	  
  4. Two really nice pictures:
      C = -0.1  + 0.651i
	  C = -0.75 + 0.11i
   
  5. Just try it out... And enjoy!
