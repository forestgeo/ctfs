library(forestr)
library(bci)
library(dplyr)



# original fun ------------------------------------------------------------
pdf.allplot=function(splitdata=bci.split6,spplist=bci.spptable,elev=ctfs.elev$bci$mat,plotdim=c(1000,500),cutoff=c(10,100,300,3000),topoint=2,bw=FALSE,
                     topoclr='gray80',bty='o',plotside=9,h=8,w=10,size=c(.3,.45,.65),deadtree=FALSE,symbols=NULL,legsize=0.75,legpos=c(plotdim[1]/2,1.16*plotdim[2]),
                     export=TRUE,singlefile=TRUE,path='/home/condit/data/maps/bci/')
{
 on.exit(graphics.off())
 if(singlefile) pdf(file=pst(path,'allsppMap.pdf'),width=w,height=h)

 allspp=names(splitdata)
 for(i in 1:length(allspp))
  {
   if(!singlefile & export) pdf(file=pst(path,allspp[i],'Map.pdf'),width=10,height=8)
   map(splitdata,species=allspp[i],spplist=spplist,elevdata=elev,plotdim=plotdim,cutoff=cutoff,size=size,deadtree=deadtree,bw=bw,topoclr=topoclr,
       bty=bty,symbols=symbols,legsize=legsize,legpos=legpos,plotside=plotside,topoint=topoint,export="no")
   if(!singlefile & export) graphics.off()
  }
}



# work --------------------------------------------------------------------



# ?pdf.allplot

# Save time by filtering only 3 spp
sp_wanted <- unique(bci12full1$sp)[1:3]
plot_data <- filter(bci::bci12full1, sp %in% sp_wanted)
tibble::as_tibble(plot_data)

plot_split <- split_data(plot_data)

pdf.allplot(
  splitdata = plot_split, 
  spplist = NULL,
  elev = NULL,
  path = "./")
