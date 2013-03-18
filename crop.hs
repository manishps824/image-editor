import Graphics.Filters.GD
import Graphics.GD

crop :: Image -> Point -> Point -> IO Image
crop img (a,b) (c,d) = do
  newimg <- newImage ((c-a),(d-b))
  copyRegion (a,b) ((c-a),(d-b)) img (0,0) newimg
  resizeImage (c-a) (d-b) newimg
  
main = do
  myimg <- loadJpegFile "download.jpg"
  new <- crop myimg (20,20) (100,100) 
  saveJpegFile (-1) "download5.jpeg" new
