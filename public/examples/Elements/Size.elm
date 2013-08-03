
{-------------------------------------------------------
  You can set the width and height of the element with
  the following two functions:

        width, height : Int -> Element -> Element

  You can set both width and height at the same time
  with this function:
  
          size : Int -> Int -> Element -> Element

   Try them out on the car.
-------------------------------------------------------}


main = width 300 (image 472 315 "/car.jpg")