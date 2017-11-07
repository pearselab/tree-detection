#how to download the EBImage library
#source("https://bioconductor.org/biocLite.R")
#biocLite("EBImage")

#You will need to use the EBImage library
library(EBImage)
require('EBImage')

#loading image file
Image <- readImage('C:/Users/Michael/Documents/GitHub/tree-detection/gis/nasmp-background-images/third_transect-google.png')
display(Image)

# changing the viewing frame of the image
Image <- Image[0:800, 0:800,]
display(Image)
#The image must be in grayscale. Change to grayscale
colorMode(Image) <- Grayscale
display(Image)
#Thresholding the image using the otsu function.
threshold <- otsu(Image)

#Will i am not sure what is going on here.
#If i understand it correctly, it is combining all the frames and thresholds into one image using the mapply function we learned
#about the other day in class. Could you clarify this step for me? thanks.
Image_th <-combine( mapply(function(frame, th) frame > th, getFrames(Image), threshold, SIMPLIFY=FALSE) )
display(Image_th, all=TRUE)

##Image SEGMENTATION EFFORTS

#BWLAbel segmentation. Seems to do a decent job. I am confused at what the segmentation does and how to go further.
Image_label <- bwlabel(Image_th)
display(colorLabels(Image_label))
table(Image_label)

#The file also had a watershed type segmentation. I couldnt get the images to look how they where supposed to.
#Also i couldnt test easily because it would take a long time to run? can i use an mapply or similar function and use multi core
#Processing? The issue i had when running the watershed is that it had a color patchwork in the background and you couldnt see
#the trees location in it.
Imask <- watershed(distmap(Image_th), 1,3)
display(colorLabels(Imask), all=TRUE)


#I tried to see if using the example on the website would work in a similar way to our image.
#Im confused on how to set the seed properly. If i increase the makeBrush() number it seems to get more acurate to a point but it
#is still cutting our trees. much faster than the watershed.
Image_th=opening(Image_th, makeBrush(3,shape = 'disc'))
Image_th = fillHull(Image_th)
Image_th = bwlabel(Image_th)
#table(Image_th)
display(colorLabels(Image_th), all=TRUE)
imask = opening(Image>.1, makeBrush(3,shape = 'disc'))
cmask = propagate(Image, seeds=Image_th, mask = imask)
display(imask, all = true)
segmented = paintObjects(cmask,Image,col = 'red',thick=TRUE)
segmented= paintObjects(Image_th, segmented, col='blue',thick=TRUE)
display(segmented,all=TRUE)

#raster 
#mean distribution by pixels
third_transect <- 'C:/Users/Michael/Documents/GitHub/tree-detection/gis/nasmp-background-images/third_transect-google.png'
Sat_Image <- 'C:/Users/Michael/Documents/GitHub/tree-detection/Test Satalitte image/q0421_sw_naip2016_rgb.tiff'


detectTree <- function(images){
  #loading image file
  Image <- readImage(images,convert=TRUE)
  display(Image)

  
  # changing the viewing frame of the image
  
  ##Image <- Image[0:y, 0:x,]
  #display(Image)
  #The image must be in grayscale. Change to grayscale
  colorMode(Image) <- Grayscale
  #display(Image)
  #Thresholding the image using the otsu function.
  threshold <- otsu(Image)
  
  #Will i am not sure what is going on here.
  #If i understand it correctly, it is combining all the frames and thresholds into one image using the mapply function we learned
  #about the other day in class. Could you clarify this step for me? thanks.
  Image_th <-combine( mapply(function(frame, th) frame > th, getFrames(Image), threshold, SIMPLIFY=FALSE) )
  #display(Image_th, all=TRUE)
  Image_th=opening(Image_th, makeBrush(3,shape = 'disc'))
  Image_th = fillHull(Image_th)
  Image_th = bwlabel(Image_th)
  #table(Image_th)
  #display(colorLabels(Image_th), all=TRUE)
  imask = opening(Image>.1, makeBrush(3,shape = 'disc'))
  cmask = propagate(Image, seeds=Image_th, mask = imask)
  #display(imask, all = true)
  segmented = paintObjects(cmask,Image,col = "red",thick=TRUE)
  segmented= paintObjects(Image_th, segmented, col='blue',thick=TRUE)
  display(segmented,all=TRUE)
  return(segmented)
}


Image <- readImage(third_transect)
satImages <- readImage(Sat_Image,convert=TRUE)
detectTree(third_transect,100,100)
detectTree(Sat_Image)
##Attempt to split image dynamically
split_image <- function(image,x){
    coltest <- seq(1,ncol(image),by=x)
    rowtest <- seq(1,nrow(image),by=x)
    new_image <-matrix(ncol=length(coltest), nrow=1)
    coltestsize <- length(seq(1,ncol(image),by=x))
    rowtestsize <- length(seq(1,nrow(image),by=x))
    for(i in 1:(coltestsize-1)){
      for(j in 1:(rowtestsize-1) ){
        append(new_image,image[coltest[i]:coltest[i+1],rowtest[j]:rowtest[j+1]])
        
      }
    }
  return(new_image)
  }



##size <-dim(Image)
##print(size)
##print(satImages)

#hist(detectTree(Sat_Image,1000,1000))
#use functional programming to split image based on lists that the user provides.
detectTree(Sat_Image)

mcMap(detectTree,split_image(Sat_Image,14),cores = 4)
