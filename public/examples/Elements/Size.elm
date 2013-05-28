{-------------------------------------------------------
  You can set the width and height of the element with
  the following two functions:

        width, height :: Int -> Element -> Element

   Try them out on Yogi Bear.
-------------------------------------------------------}


main = width 300 (image 472 315 "/car.jpg")
