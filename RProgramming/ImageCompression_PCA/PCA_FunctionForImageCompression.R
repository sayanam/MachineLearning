#install.packages('jpeg')


########################## PCA - IMAGE COMPRESSION #############################


library(jpeg)


# Finds the PCA column upto which the cumulative sum of PCA variance reaches the required variance
find_pcaVal_for_variance <- function(pca, percentage){
  pca_var_per = cumsum(pca$sdev^2)/sum(pca$sdev^2) * 100
  len_pca_var_per = length(pca_var_per)
  for (i in c(1: len_pca_var_per)){
    if(pca_var_per[i] > percentage){
      return (i-1)
    }
  }
}

# Compressses the image object to upto which the variance can be retained. And performs image correction.
compress_image <- function(image, var_to_be_retained){
  r = image[,,1]
  g = image[,,2]
  b = image[,,3]
  
  pca_r = prcomp(r, center= FALSE)
  pca_g = prcomp(g, center= FALSE)
  pca_b = prcomp(b, center= FALSE)
  
  r_n_var = find_pcaVal_for_variance(pca_r, var_to_be_retained)
  g_n_var = find_pcaVal_for_variance(pca_g, var_to_be_retained)
  b_n_var = find_pcaVal_for_variance(pca_b, var_to_be_retained)
  
  r_comp = pca_r$x[,1:r_n_var]%*%t(pca_r$rotation[,1:r_n_var])
  g_comp = pca_g$x[,1:g_n_var]%*%t(pca_g$rotation[,1:g_n_var])
  b_comp = pca_b$x[,1:b_n_var]%*%t(pca_b$rotation[,1:b_n_var])
  img_comp = array(c(r_comp, g_comp, b_comp), dim=dim(image))
  
  # Image Correction
  for (i in 1: dim(img_comp)[3]){
    img_comp[,,i] = ifelse(img_comp[,,i] < 0,0,img_comp[,,i])
    img_comp[,,i] = ifelse(img_comp[,,i] > 1,1,img_comp[,,i])
  }
  return (img_comp)
}


# Compress all the jpeg images present in the directory
compress_JPEG_in_Folder <- function(path, var_to_be_retained, deleteOriginalFiles = FALSE){
  
  if (! dir.exists(path)) {
    stop("The given directory does not exist")
  }
  newFolderDir = paste(path,'CompressedFiles_',var_to_be_retained,'\\',sep='') 
  if(! dir.exists(newFolderDir)){
    dir.create(newFolderDir)
  }
  jpgFiles = list.files(path= path, pattern='.jpg')
  if(length(jpgFiles) == 0){
    stop('There are no JPEG files in the given directory')
  }
  
  library(jpeg)
  for (file in jpgFiles){
    image = readJPEG(paste(path,file, sep=''))
    img_comp =  compress_image(image, var_to_be_retained)
    newFileName = paste(newFolderDir,file,sep='')
    writeJPEG(img_comp, newFileName)
  }
  
  if (deleteOriginalFiles){
    library(yesno)
    if(yesno("Are you sure you want to delete the files ?")){
      for (file in jpgFiles){
        file.remove(paste(path,file, sep=''))
      }
    }
  }
}

# Testing
compress_JPEG_in_Folder('C:\\Users\\91809\\Desktop\\Praxis\\Sem2\\MachineLearning\\Assignments\\R\\PCA\\Images\\',99, TRUE)



