library(EBImage)
 
   # Segment a given image
   segment.image <- function(image, method=c("bw","watershed"), plot=FALSE){
         method <- match.arg(method)
         # Perform thresholding
           .thresh <- function(frame, th) frame < th
           thresholds <- otsu(image)
           image.thresholded <- combine(mapply(.thresh, getFrames(image), thresholds, SIMPLIFY=FALSE))
       
             # Use method to segment
             if(method=="bw"){
                   labels <- bwlabel(image.thresholded)
               } else {
                     labels <- watershed(distmap(image), 1,3)
                 }
       
             # Return and (optionally) return
             if(plot)
                  display(colorLabels(labels))
          return(table(labels))
   }
   Image <- readImage('../gis/nasmp-background-images/third_transect-google.png')
   display(Image)
segment.image(Image,"watershed",plot = TRUE)  
  
  