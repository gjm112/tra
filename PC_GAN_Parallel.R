# nohup R CMD BATCH /home/gmatthews1/greg/tra/PC_GAN_Parallel.R  /home/gmatthews1/greg/tra/PC_GAN_Parallel.Rout &

#let's start with a bunch of 10 by 10 image.  
expit <- function(x){
  out <- exp(x)/(1+exp(x))
  return(out)
}

#let's start with a bunch of 10 by 10 image.  
logit <- function(x){
  x[x > 0.999] <- 0.999
  x[x < 0.001] <- 0.001
  out <- log((x)/(1-x))
  return(out)
}


# n <- 7 #sample size
# xres <- 500
# yres <- 500
# library(EBImage)
# filelist <- red <- green <- blue <- list()
# #im<-readImage("/Users/gregorymatthews/Dropbox/Rart/queenOfHearts.jpeg")
# filnames <- c("denny","greg","sarah","michaela","john")
# path <- "/Users/gregorymatthews/Dropbox/Rart/DataArtProject/opioidFaces/"
# path <- "/Users/gregorymatthews/Dropbox/Rart/DataArtProject/eos/"
# #path <- "/Users/gregorymatthews/Dropbox/Rart/DataArtProject/mondrian/"
# path <- "/Users/gregorymatthews/Dropbox/Rart/DataArtProject/landscape/"
# #path <- "/home/gmatthews1/greg/tra/picasso/"
# #path <- "/Users/gregorymatthews/Dropbox/Rart/DataArtProject/red/"
# filnames <- list.files(path)
# imList <- list()
# for (i in 1:length(filnames) ){
#   im<-readImage(paste0(path,filnames[i]))
#   #im<-readImage("/Users/gregorymatthews/Desktop/desktopCrap/greg.png")
#   #im<-readImage("/Users/gregorymatthews/Desktop/desktopCrap/denny.png")
#   #im<-readImage("/Users/gregorymatthews/Desktop/obama.png")
#   #im <- readImage("/Users/gregorymatthews/Desktop/guyAlley.jpg")
#   #imMagr <- readImage("/Users/gregorymatthews/Desktop/desktopCrap/magritte.png")
#   #imMagr <- readImage("/Users/gregorymatthews/Desktop/magrTrain.png")
#   ##imDali <- readImage("/Users/gregorymatthews/Desktop/dali.png")
#   #imVangogh <- readImage("/Users/gregorymatthews/Desktop/vangogh.png")
# 
#   ddd<-dim(im)
#   gran<-xres
#   grany<-yres
#   xxx<-round(seq(1,ddd[1],length=gran))
#   yyy<-round(seq(1,ddd[2],length=grany))
#   small<-im@.Data[xxx,yyy,1:3]
#   imList[[i]] <- small
# 
#   #red[[i]] <- small[,,1]
#   #green[[i]] <- small[,,2]
#   #blue[[i]] <- small[,,3]
# }
# save(imList, file = "/Users/gregorymatthews/Dropbox/tra/imList_landscape.RData")

load("/home/gmatthews1/greg/tra/imList_landscape.RData")


# for (q in 1:length(filnames)){
# plot(0,0, xlim = c(0,xres), ylim = c(0,yres), col = "white")
# for (i in 1:xres){
#   for (j in 1:yres){
#     col <- rgb(imList[[q]][i,j,1], imList[[q]][i,j,2], imList[[q]][i,j,3])
#     polygon(c(0,1,1,0) + i - 1, c(0,0,1,1) + yres -j, col = col, border = col)
#   }
# }
# 
#  }
xres <- dim(imList[[1]])[1]
yres <- dim(imList[[1]])[2]


real <- c(imList[[1]])
for (q in 2:length(imList)){
  real <- rbind(real,c((imList[[q]])))
}

set.seed(1234)
n_fake <- 10*length(imList)
fake <- matrix(rnorm(xres*yres*3*n_fake),nrow = n_fake)

dat <- rbind(real, fake)
pc <- prcomp(logit(dat))

pc_dat  <- dat%*%pc$rotation

x <- pc_dat
y <- c(rep(1,length(imList)),rep(0,n_fake))

library(randomForest)
mod <- randomForest(x,factor(y))
#back to raw data: expit(pc_dat[3,] %*%t(pc$rotation))

good <- list()
for (g in 1:n_fake){
  score <- 0
  vec <- rnorm(ncol(x))
  #predict(mod,vec, type = "prob")[,2]
  
  for (i in 1:2){
    n <- sample(1:ncol(x),1)
    ind <- sample(1:ncol(x),n, replace = FALSE)
    vec_temp <- vec
    vec_temp[ind] <- vec[ind] + rnorm(n, 0, 0.25)
    score_temp <- predict(mod,vec_temp, type = "prob")[,2]
    
    if (score_temp > score){
      score <- score_temp
      vec <- vec_temp
      print(score)
      print(n)
      
      out <- expit(vec %*%t(pc$rotation))
      arr <- array(out,dim = c(xres,yres,3))
    }
    
  }
  
  # plot(0,0, xlim = c(0,xres), ylim = c(0,yres), col = "white", asp = 1)
  # for (i in 1:xres){
  #   for (j in 1:yres){
  #     col <- rgb(arr[i,j,1], arr[i,j,2], arr[i,j,3])
  #     polygon(c(0,1,1,0) + i - 1, c(0,0,1,1) + yres -j, col = col, border = col)
  #   }
  # }
  # 
  
  good[[g]] <- expit(vec %*%t(pc$rotation))
}


gen <- function(model, thresh = 0.95, maxit = 100000){
  score <- 0
  vec <- rnorm(ncol(x))
  count <- 0
  #for (i in 1:1000){
  
  while (score < thresh  & count < maxit){
    n <- sample(1:ncol(x),1)
    ind <- sample(1:ncol(x),n, replace = FALSE)
    vec_temp <- vec
    vec_temp[ind] <- vec[ind] + rnorm(n, 0, 1)
    score_temp <- predict(model, vec_temp, type = "prob")[,2]
    
    if (score_temp > score){
      score <- score_temp
      vec <- vec_temp
      print(score)
      print(n)
      
      out <- expit(t(pc$rotation %*% vec))
      arr <- array(out,dim = c(xres,yres,3))
      
      
    }
    count <- count + 1
  }
  
  out <- expit(vec %*%t(pc$rotation))
  return(out)
  
}


#Next step 
for (iter in 1:10){print(iter)
  
  real <- c(imList[[1]])
  for (q in 2:length(imList)){
    real <- rbind(real,c((imList[[q]])))
  }
  
  
  fake <- do.call(rbind,good)
  
  dat <- rbind(real, fake)
  pc <- prcomp(logit(dat))
  
  pc_dat  <- dat%*%pc$rotation
  
  x <- pc_dat
  y <- c(rep(1,length(imList)),rep(0,n_fake))
  
  mod <- randomForest(x,factor(y))
  
  library(parallel)
  good <- mclapply(as.list(c(1:n_fake)),gen, model = mod, mc.cores = 32)
  
}
  

save.image("/home/gmatthews1/greg/tra/picasso_20190707.RData")
  
#   
#   
#   for (q in 1:length(good)){
#     
#   arr <- array(good[[q]], dim = c(xres,yres,3))
#     plot(0,0, xlim = c(0,xres), ylim = c(0,yres), col = "white", asp = 1)
#     for (i in 1:xres){
#       for (j in 1:yres){
#         col <- rgb(arr[i,j,1], arr[i,j,2], arr[i,j,3])
#         polygon(c(0,1,1,0) + i - 1, c(0,0,1,1) + yres -j, col = col, border = col)
#       }
#     }
#     
#   }
# 
# 
# 
# 
# fret <- dat%*%pc$rotation
# vec <- apply(fret,2,mean) + 1000*apply(fret,2,sd)
# 
# set.seed(22)
# ind_vec <- c(1:8)
# #vec <- rnorm(length(ind_vec),0,10)
# #vec <- rnorm(length(ind_vec),5,10)
# #vec <- rep(1,length(ind_vec))
# #vec <- c(100,10,10,10,-10,10,10,10,10,-100)
# #vec <- c(-100,-100,-100,50,50,50,50,50,50,50, rep(10,22))
# out <- expit(vec[ind_vec] %*%t(pc$rotation)[ind_vec,])
# arr <- array(out,dim = c(xres,yres,3))
# plot(0,0, xlim = c(0,xres), ylim = c(0,yres), col = "white", asp = 1)
# for (i in 1:xres){
#   #for (j in 25:125){
#   for (j in 1:yres){
#     col <- rgb(arr[i,j,1], arr[i,j,2], arr[i,j,3])
#     polygon(c(0,1,1,0) + i - 1, c(0,0,1,1) + yres -j, col = col, border = col)
#   }
# }
# 
# 
# 
# 
