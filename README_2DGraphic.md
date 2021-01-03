# KeiLisp
（this document fix： 2020/01/03 create: 2020/01/01）

## Reference
+ [Atom](./README_Atom.md)
+ [Cons](./README_Cons.md)
+ [Function](./README_Function.md)
+ [2D Graphic Function](./README_2DGraphic.md)

## 2D Graphic Function
In this interpreter the following functions are defined.
To use these functions, you must first call [(gopen)](#gopen) to bring up the canvas.

+ [galpha](#galpha)
+ [garc](#garc)
+ [garc-to](#garc-to)
+ [gbezcurve-to](#gbezcurve-to)
+ [gclear](#gclear)
+ [gclose](#gclose)
+ [gcolor](#gcolor)
+ [gfill](#gfill)
+ [gfill-color](#gfill-color)
+ [gfill-rect](#gfill-rect)
+ [gfill-text](#gfill-text)
+ [gfill-tri](#gfill-tri)
+ [gfinish-path](#gfinish-path)
+ [gimage](#gimage)
+ [gmove-to](#gmove-to)
+ [gline-to](#gline-to)
+ [gline-cap](#gline-cap)
+ [gline-join](#gline-join)
+ [gline-width](#gline-width)
+ [gopen](#gopen)
<!-- + [gpattern](#gpattern) -->
+ [gquadcurve-to](#gquadcurve-to)
+ [gsave-jpeg](#gsave-png)
+ [gsave-png](#gsave-jpeg)
+ [gscale](#gscale)
+ [gshadow-blur](#gshadow-blur)
+ [gshadow-color](#gshadow-color)
+ [gshadow-offsetx](#gshadow-offsetx)
+ [gshadow-offsety](#gshadow-offsety)
+ [gsleep](#gsleep)
+ [gstart-path](#gstart-path)
+ [gstroke](#gstroke)
+ [gstroke-color](#gstroke-color)
+ [gstroke-rect](#gstroke-rect)
+ [gstroke-text](#gstroke-text)
+ [gstroke-tri](#gstroke-tri)
+ [gtext-align](#gtext-align)
+ [gtext-dire](#gtext-dire)
+ [gtext-font](#gtext-font)
+ [gtext-line](#gtext-line)
+ [gtranslate](#gtranslate)
+ [grect](#grect)
+ [grotate](#grotate)


### galpha
**(galpha X)**
Function to change the transparency.
The closer the value of X is to 0, the more transmission occurs; the closer it is to 1, the less transmission occurs.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gfill-color "red")
t
>> (galpha 1)(gfill-rect 50 100 90 90)
t
t
>> (galpha 0.5)(gfill-rect 200 100 90 90)
t
t
>> (galpha 0)(gfill-rect 350 100 90 90)
t
t
```

### garc
**(garc x y radius startAngle endAngle anticlockwise)**
Function to draw an arc.
anticlockwise can be specified as clockwise if it is greater than or equal to 0 and counterclockwise if it is less than 0.
This function is a path function. 
The path function allows you to specify the line to be drawn before deciding to draw it.
Path function is necessary to call [(gstart-point)](#gstart-path) in advance.
```
>> (gopen)
canvas size, width :600 height :300
t
>> (gstart-path)(garc 300 150 50 0 360 0)(gstroke)
t
t
t
```

### garc-to 
**(garc-to x1 y1 x2 y2 radius)**
Function to draw an arc.
Draws an arc connected to the previous point by a straight line using the two specified points and radius.
This function is a path function. 
The path function allows you to specify the line to be drawn before deciding to draw it.
Path function is necessary to call [(gstart-point)](#gstart-path) in advance.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gstart-path)(gmove-to 180 190)(garc-to 180 230 110 230 130)(gline-to 110 230)(gstroke)
t
t
t
t
t
```

### gbezcurve-to
**(gbezcurve-to x1 y1 x2 y2 x3 y3)**
Function to draw a　bezier curve.
Draws a Bezier curve connected to last point using the first two points you specify.
This function is a path function. 
The path function allows you to specify the line to be drawn before deciding to draw it.
Path function is necessary to call [(gstart-point)](#gstart-path) in advance.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gcolor "pink")
t
>> (gstart-path)(gmove-to 75 40)
(gbezcurve-to 75 37 70 25 50 25)
(gbezcurve-to 20 25 20 62.5 20 62.5)
(gbezcurve-to 20 80 40 102 75 120)
(gbezcurve-to 110 102 130 80 130 62.5)
(gbezcurve-to 130 62.5 130 25 100 25)
(gbezcurve-to 85 25 75 37 75 40)
(gfill)
t
t
t
t
t
t
t
t
t
```

## gclear
**(gclear)**
Function to clear the canvas.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gfill-rect 100 100 100 100)
t
>> (gclear)
t
```

### gclose
**(gclose)**
Function to close the canvas.

```
>> (gclose)
The canvas has already been closed.
nil
>> (gopen)
canvas size, width :600 height :300
t
>> (gclose)
t
```

### gcolor
**(gcolor X)**
Function to specify fill and stroke color when drawing.
X can be a color string, a color code, three numbers for RGB, or four numbers for RGBA.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gcolor "orage")
t
>> (gcolor "#FFA500")
t
>> (gcolor 255 165 0)
t
>> (gcolor 255 165 0 1)
t
```

### gfill
**(gfill)**
This function fills in a shape by connecting the lines specified by the path function.
When using this function, there is no need to declare the end of the path function in [(finish-point)](#finish-path).

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gstart-path)(gmove-to 75 40)
(gbezcurve-to 75 37 70 25 50 25)
(gbezcurve-to 20 25 20 62.5 20 62.5)
(gbezcurve-to 20 80 40 102 75 120)
(gbezcurve-to 110 102 130 80 130 62.5)
(gbezcurve-to 130 62.5 130 25 100 25)
(gbezcurve-to 85 25 75 37 75 40)
(gfill)
t
t
t
t
t
t
t
t
t
```

### gfill-color
**(gfill-color X)**
Function to specify fill color when drawing.
X can be a color string, a color code, three numbers for RGB, or four numbers for RGBA.


```
>> (gopen)
canvas size, width :600 height :300
t
>> (gfill-color "orage")
t
>> (gfill-color "#FFA500")
t
>> (gfill-color 255 165 0)
t
>> (gfill-color 255 165 0 1)
t
```

### gfill-rect
**(gfill-rect x y width height)**
Function to draw a fill rectangle.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gfill-rect 100 100 100 100)
t
```

### gfill-text
**(gfill-text text x y)**
Function to draw a fill text.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gtext-font "48px serif")(gfill-text "Hello World" 10 50)
t
t
```

### gfill-tri
**(gfill-tri x1 y1 x2 y2 x3 y3)**
Function to draw a fill triangle.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gfill-tri 100 100 50 150 150 150)
t
```

### gfinish-path
**(gfinish-path)**
Function to finish the path function.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gstart-path)(gmove-to 50 50)(gline-to 550 50)(gfinish-path)(gstroke)
t
t
t
t
t
```

### gImage
**(gimage url x y)**
**(gimage url x y width height)**
Function to draw an image.
Please specify the image by URL.
It is preferable to use "https" instead of "http".
Unfortunately, you cannot specify a local image.
Also, [(gsave-png)](#gsave-png) [(gsave-jpeg)](#gsave-jpeg) is not available with this function.
These are problems with the web browser.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gimage "http://www.cc.kyoto-su.ac.jp/~atsushi/thumbnails/AokiHanko.jpg" 200 100 200 200)
t
```

### gline-to
**(gline-to x1 y1 x2 y2)**
Function to draw line.
This function is a path function. 
The path function allows you to specify the line to be drawn before deciding to draw it.
Path function is necessary to call [(gstart-point)](#gstart-path) in advance.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gstart-path)(gmove-to 50 50)(gline-to 550 50)(gfinish-path)(gstroke)
t
t
t
t
t
```

### gline-cap
**(gline-cap X)**
Function to specify the shape of the tip of a line to be drawn.
When the value of X is 0, it is normal, when it is greater than 0, it is "round", and when it is less than 0, it is "square".

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gstart-path)(gline-width 15)(gmove-to 50 50)(gline-cap 0)(gline-to 50 250)(gstroke)
t
t
t
t
t
t
>> (gstart-path)(gline-width 15)(gmove-to 100 50)(gline-cap 1)(gline-to 100 250)(gstroke)
t
t
t
t
t
t
>> (gstart-path)(gline-width 15)(gmove-to 150 50)(gline-cap -1)(gline-to 150 250)(gstroke)
t
t
t
t
t
t
>>   
```

### gline-join
**(gline-join X)**
Function to specify the shape of the joining part of a line to be drawn.
It is normal when the value of X is 0, "round" when it is greater than 0, and "bevel" when it is less than 0.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gstart-path)(gline-width 20)(gline-join 0)(gmove-to 50 50)(gline-to 100 100)(gline-to 50 150)(gline-to 100 200)(gstroke)
t
t
t
t
t
t
t
t
>> (gstart-path)(gline-width 20)(gline-join 1)(gmove-to 150 50)(gline-to 200 100)(gline-to 150 150)(gline-to 200 200)(gstroke)
t
t
t
t
t
t
t
t
>> (gstart-path)(gline-width 20)(gline-join -1)(gmove-to 250 50)(gline-to 300 100)(gline-to 250 150)(gline-to 300 200)(gstroke)
t
t
t
t
t
t
t
t
```

### gline-width
**(gline-width X)**
Function to specify the thickness of a line to be drawn.
When the value of X is less than or equal to 1, the thickness of the line will be 1.

```
>> (gopen)
canvas size, width :600 height :
>> (gstart-path)(gline-width 5)(gmove-to 50 50)(gline-to 50 250)(gstroke)
t
t
t
t
t
>> (gstart-path)(gline-width 10)(gmove-to 100 50)(gline-to 100 250)(gstroke)
t
t
t
t
t
>> (gstart-path)(gline-width 15)(gmove-to 150 50)(gline-to 150 250)(gstroke)
t
t
t
t
t
```

### gmove-to
**(gline-to x1 y1 x2 y2)**
Function to move the coordinates currently indicated by the path function. 
It does not draw a line.
This function is a path function. 
The path function allows you to specify the line to be drawn before deciding to draw it.
Path function is necessary to call [(gstart-point)](#gstart-path) in advance.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gstart-path)(gmove-to 50 50)(gline-to 550 50)(gfinish-path)(gstroke)
t
t
t
t
t
```

### gopen
**(gopen)**
Function to open the canvas.

```
>> (gopen)
canvas size, width :600 height :300
t
>>  (gopen)
The canvas has already been opened.
nil
```

<!-- ### gpattern
**(gpattern X)**
Function to ...
Please specify the image by URL.
It is preferable to use "https" instead of "http".
Unfortunately, you cannot specify a local image.
Also, [(gsave-png)](#gsave-png) [(gsave-jpeg)](#gsave-jpeg) is not available with this function.
These are problems with the web browser.

```
>> (gopen)
canvas size, width :600 height :300
t
``` -->

### gquadcurve-to
**(gquadcurve-to x1 y1 x2 y2)**
Function to draw a　quad curve.
Draws a curve starting and ending at the current coordinates and the last specified coordinates, and moving toward the first specified coordinates.
This function is a path function. 
The path function allows you to specify the line to be drawn before deciding to draw it.
Path function is necessary to call [(gstart-point)](#gstart-path) in advance.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gstart-path)
(gmove-to 75 25)
(gquadcurve-to 25 25 25 62.5)
(gquadcurve-to 25 100 50 100)
(gquadcurve-to 50 120 30 125)
(gquadcurve-to 60 120 65 100)
(gquadcurve-to 125 100 125 62.5)
(gquadcurve-to 125 25 75 25)
(gstroke)
t
t
t
t
t
t
t
t
t
```

### gsave-png
**(gsave-png)**
Function to download the current canvas as an image file with ".png" extension.
If you are calling [(gimage)](#gimage), it may not work correctly. This is a problem with your web browser.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gfill-rect 100 100 100 100)
t
>> (gsave-png)
t
```

### gsave-jpeg
**(gsave-jpeg)**
Function to download the current canvas as an image file with ".jpeg" extension.
If you are calling [(gimage)](#gimage), it may not work correctly. This is a problem with your web browser.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gfill-rect 100 100 100 100)
t
>> (gsave-jpeg)
t
```

### gscale
**(gscale widht height)**
Function to scale the width and height of a shape to be drawn.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gfill-rect 1 1 10 10)
t
>> (gscale 5 10)
t
>> (gcolor "red")(gfill-rect 10 10 10 10)
t
t
```

### gshadow-blur
**(gshadow-blur X)**
Function to specify the degree of shadow blur when drawing.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gshadow-color "red")(gshadow-blur 15)(gshadow-offsetx 10)(gshadow-offsety 10)(gfill-rect 20 20 100 100)
```

### gshadow-color
**(gshadow-color X)**
Function to specify shadow color when drawing.
X can be a color string, a color code, three numbers for RGB, or four numbers for RGBA.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gshadow-color "orage")
t
>> (gshadow-color "#FFA500")
t
>> (gshadow-color 255 165 0)
t
>> (gshadow-color 255 165 0 1)
t
```

### gshadow-offsetx
**(gshadow-offsetx X)**
Function to specify the x coordinate offset of the shadow when drawing.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gshadow-color "red")(gshadow-offsetx 10)(gfill-rect 20 20 100 100) 
t
t
t
```

### gshadow-offsety
**(gshadow-offsety X)**
Function to specify the y coordinate offset of the shadow when drawing.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gshadow-color "red")(gshadow-offsety 10)(gfill-rect 20 20 100 100) 
t
t
t
```

### gsleep
**(gsleep X)**
Function to delay for the time (ms) specified by X.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gsllep 1000)
t
```

### gstart-path
**(gstart-path)**
Function to start the path function.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gstart-path)(gmove-to 50 50)(gline-to 550 50)(gfinish-path)(gstroke)
t
t
t
t
t
```

#### gstroke
**(gstroke)**
This function stroke in a shape by connecting the lines specified by the path function.
When using this function, if the coordinates of the declared path function are connected, there is no need to declare the end of the path function with [(finish-point)](#finish-path).

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gstart-path)
(gmove-to 75 25)
(gquadcurve-to 25 25 25 62.5)
(gquadcurve-to 25 100 50 100)
(gquadcurve-to 50 120 30 125)
(gquadcurve-to 60 120 65 100)
(gquadcurve-to 125 100 125 62.5)
(gquadcurve-to 125 25 75 25)
(gstroke)
t
t
t
t
t
t
t
t
t
```

### gstroke-color
**(gstroke-color X)**
Function to specify stroke color when drawing.
X can be a color string, a color code, three numbers for RGB, or four numbers for RGBA.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gstroke-color "orage")
t
>> (gstroke-color "#FFA500")
t
>> (gstroke-color 255 165 0)
t
>> (gstroke-color 255 165 0 1)
t
```

### gstroke-rect
**(gstroke-rect x y width height)**
Function to draw a stroke rectangle.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gstroke-rect 100 100 100 100)
t
```

### gstroke-text
**(gstroke-text text x y)**
Function to draw a stroke text.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gtext-font "48px serif")(gstroke-text "Hello World" 10 50)
t
t
```

### gstroke-tri
**(gstroke-tri x1 y1 x2 y2 x3 y3)**
Function to draw a stroke triangle.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gstroke-tri 100 100 50 150 150 150)
t
```

### gtext-align
**(gtext-align X)**
Function to specify the align of text.
X can be specified as "center", "right", or "left".

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gtext-font "48px serif")
t
>> (gtext-align "left")(gfill-text "left" 300 50)
t
t
>> (gtext-align "center")(gfill-text "center" 300 100)
t
t
>> (gtext-align "right")(gfill-text "right" 300 150)
t
t
```

### gtext-dire
**(gtext-dire X)**
Function to specify the direction of text.
When the value of X is 0, it is normal, when it is greater than 0, it is "rtl", and when it is less than 0, it is "ltr".

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gtext-font "48px serif")
t
>>(gtext-dire 0)(gfill-text "Hello World" 300 50)
t
t
>> (gtext-dire 1)(gfill-text "Hello World" 300 100)
t
t
>> (gtext-dire -1)(gfill-text "Hello World" 300 150)
t
t
```

### gtext-font
**(gtext-font X)**
Function to specify the font of text.
X can be a string of fonts and sizes that can be specified by a web browser.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gtext-font "48px serif")(gfill-text "京都 あい アイ 123 abc" 10 50)
t
t
>> (gtext-font "48px sans-serif")(gfill-text "京都 あい アイ 123 abc" 10 100)
t
t
>> (gtext-font "48px monospace")(gfill-text "京都 あい アイ 123 abc" 10 150)
t
t
>> (gtext-font "bold 48px serif")(gfill-text "京都 あい アイ 123 abc" 10 200)
t
t
>> (gtext-font "italic 48px serif")(gfill-text "京都 あい アイ 123 abc" 10 250)
t
t
```

### gtext-line
**(gtext-line X)**
Function to specify the line of text.
X can be specified as "top", "hanging", "middle", "alphabetic", "ideographic" or "bottom".

```
>> (gopen)
canvas size, width :600 height :300
t
>> (setq y 30)(gtext-font "30px serif")
t
t
>> (dolist (each '("top" "hanging" "middle" "alphabetic" "ideographic" "bottom") t) 
	(gstart-path)
	(gmove-to 0 y)
	(gline-to 550 y)
	(gstroke)
	(gtext-line each)
	(gfill-text each 0 y)
	(setq y (+ y 50))
)
t
```

### gtranslate
**(gtranslate x y)**
Function to move the origin of the canvas。

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gfill-rect 0 0 100 100)
t
>> (gtranslate 200 200)
t
>> (gfill-rect 0 0 100 100)
t
```

### grect
**(grect x y width height)**
Function to draw a rectangle.
This function is a path function. 
The path function allows you to specify the line to be drawn before deciding to draw it.
Path function is necessary to call [(gstart-point)](#gstart-path) in advance.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gstart-path)(grect 20 20 100 100)(gfill)(gfinish-path)
t
t
t
t
>> (gstart-path)(grect 20 170 150 100)(gstroke)(gfinish-path)
t
t
t
t
```

### grotate
**(grotate X)**
Function to rotate the coordinates of the canvas by X(°) when drawing around the origin of the canvas.

```
>> (gopen)
canvas size, width :600 height :300
t
>> (gcolor "blue")(gfill-rect 100 100 100 100)
t
t
>> (grotate 30)
t
>> (gcolor "red")(gfill-rect 100 100 100 100)
t
t
```
