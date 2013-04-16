import Graphics.Filters.GD
import Graphics.GD

laplaceEdges :: Image ->  IO()
laplaceEdges img = convolute img [[0.0,1.0,0.0],[1.0,-4.0,1.0],[0.0,1.0,0.0]] 1 0

sobelY :: Image ->  IO()
sobelY img = convolute img [[-1.0,0.0,1.0],[-2.0,0.0,2.0],[-1.0,0.0,1.0]] 1 0

sobelX :: Image ->  IO()
sobelX img = convolute img [[-1.0,-2.0,-1.0],[0.0,0.0,0.0],[1.0,2.0,1.0]] 1 0

main = do
  myimg<-loadJpegFile "download.jpg"
  --laplaceEdges myimg
  --sobelX myimg
  sobelY myimg
  saveJpegFile (-1) "Xedges.jpeg" myimg
