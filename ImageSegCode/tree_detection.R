library(EBImage)
require('EBImage')
detectTree <- function(images){
  #loading image file
  Image <- readImage(images,convert=TRUE)
  display(Image)
  colorMode(Image) <- Grayscale
  threshold <- otsu(Image)
  Image_th <-combine( mapply(function(frame, th) frame < th, getFrames(Image), threshold, SIMPLIFY=FALSE) )
  Image_th=opening(Image_th, makeBrush(3,shape = 'disc'))
  Image_th = fillHull(Image_th)
  Image_th = bwlabel(Image_th)
  imask = opening(Image>.1, makeBrush(3,shape = 'disc'))
  cmask = propagate(Image, seeds=Image_th, mask = imask)
  segmented = paintObjects(cmask,Image,col = "red",thick=TRUE)
  segmented= paintObjects(Image_th, segmented, col='blue',thick=TRUE)
  display(segmented,all=TRUE)
  return(segmented)
}


split_image <- function(image,x){
  coltest <- seq(1,ncol(image),by=x)
  rowtest <- seq(1,nrow(image),by=x)
  new_image <-matrix(ncol=length(coltest), nrow=1)
  coltestsize <- length(seq(1,ncol(image),by=x))
  rowtestsize <- length(seq(1,nrow(image),by=x))
  output <- vector("list", length(coltestsize)*length(rowtestsize))
  tracker <- 1
  for(i in 1:(coltestsize-1)){
    for(j in 1:(rowtestsize-1) ){
      append(new_image,image[coltest[i]:coltest[i+1],rowtest[j]:rowtest[j+1]])
      
    }
    for(j in 1:(rowtestsize-1) ){
      output[[tracker]] <- image[coltest[i]:coltest[i+1],rowtest[j]:rowtest[j+1],]
      tracker <- tracker + 1
    }
  }
  return(new_image)
}
return(output)
}


mcMap(detectTree,split_image(Sat_Image,14),cores = 4)

