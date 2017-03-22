# <supplemental>
# This is supplemental code necessary for the functinos to run.
MONTHNAMES=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

# # 
#
# <name>
# tojulian
#
#
# @description
# Accepts any character representation of a date and a description of the format. The submitted dates can
# be a vector or a matrix. See strptime for details about the format. 
# Returns a julian date, the number of days since 1 Jan 1960; a julian is an integer and can be graphed or operated as such, 
# though it displays as a date.
#
#
# 
# @examples
#
# tojulian(c('23Oct2010','29Mar1956'),'%d%b%Y')

#
# <source>
tojulian=function(x,dateform='%m/%d/%Y')
{
 x=strptime(x,format=dateform)
 
 year=x$year+1900

 month=x$mon+1
 day=x$mday
 return(mdy.date(month=month,day=day,year=year))
}



#
# 
#
# 
#
# <name>
# fromjulian
#
#
# @description
#  Accepts a julian date and returns a character representation of date. See tojulian(). The input
# can be vector or array. 

#
#
# 
# @examples
#
# fromjulian(1000,'%d%B%Y')

#
# <source>
fromjulian=function(j,dateform='%m/%d/%Y')
{
 oneDim=FALSE
 if(is.null(dim(j))) oneDim=TRUE
 else if(is.na(dim(j)[2])) oneDim=TRUE
 
 if(oneDim)
  {
   s=date.mmddyyyy(j)

   d=strptime(s,format='%m/%d/%Y')
   return(strftime(d,dateform))
  }
 
 result=array(dim=dim(j))  
 for(i in 1:(dim(j)[2])) 
  {
   s=date.mmddyyyy(j[,i])

   d=strptime(s,format='%m/%d/%Y') 
   result[,i]=strftime(d,dateform)
  }
   
 return(result)
}


#
# 
#
# 
#
# <name>
# create.fulldate
#
#
# @description
#  Converts a vector of date character strings in any format to a dataframe with year, month, day, yday (day of the year) and julian.
# Submitted datestr cannot be an array. 

#
#
# 
# @examples
#
# create.fulldate(c('23Oct2010','29Mar1956'),'%d%b%Y')

#
# <source>
create.fulldate=function(datestr,format='%Y-%m-%d') 
{
 julian=tojulian(datestr,dateform=format)
 fulldate=date.mdy(julian)
 dates=strptime(datestr,format=format)
 
 return(data.frame(year=fulldate$year,month=fulldate$month,day=fulldate$day,yday=dates$yday+1,julian=julian))
}


#
# 
#
# 
#
# <name>
# create.fulldate.split
#
#
# @description
# Converts the MySQL date format by splitting on the hyphen (or other characters by using sep). 
# Date must be year-month-day, and month must be numeric (create.fulldate takes any format).
# In most cases, create.fulldate
# is preferable, but Mysql allows dates with zeroes (1995-00-00 or 2002-2-00), and create.fulldate cannot handle those;
# this allows the 0 to be read.

#
#
# 
# @examples
#
# create.fulldate.split(c('23-10-2010','29-0-1956'),sep='-')

#
# <source>
create.fulldate.split=function(datestr,sep='-')
{
 year=month=day=numeric()
 
 datelst=strsplit(datestr,split=sep)
 for(i in 1:length(datelst))
   if(length(datelst[[i]])==3)
    {
     year[i]=as.numeric(datelst[[i]][1])
     month[i]=as.numeric(datelst[[i]][2])
     day[i]=as.numeric(datelst[[i]][3])
    }
   else year[i]=month[i]=day[i]=NA
 
 return(data.frame(year,month,day))
}


#
# 
#
# 
#
# <name>
# order.by.rowcol
#
#
# @description
#  Reorder rows and columns of a matrix so they are sorted as if column and row names are numeric. This allows labels from cut to
# be ordered numerically. Otherwise, sort order is 1, 10, 11, 12, ... 18, 19, 2, 20, 21, etc. 

#
#
# 
# @examples
#
# 
# <source>
order.by.rowcol=function(x)
{
 ord1=order.bynumber(dimnames(x)[[1]])
 ord2=order.bynumber(dimnames(x)[[2]])

 return(x[ord1,ord2])
}


#
# 
#
# 
#
# <name>
# order.bynumber
#
#
# @description
#  Returns ordering of a character vector with any numbers coming first, in numeric order.

#
#
# 
# @examples
#
# 
# <source>
order.bynumber=function(x)
{
 options(warn=(-1))
 
 num=(!is.na(as.numeric(x)))
 numpart=as.character(sort(as.numeric(x[num])))
 charpart=sort(x[!num])
 
 options(warn=0)
 return(match(c(numpart,charpart),x))
}
#
# 
#
#  
# <name>
# fill.dimension
#
#
# @description
#  This function fills out an array of 2 dimensions, adding zeroes (or other values) for extra columns
# and rows as named in class1 and class2. If a column (or row) is
# missing, it will be filled with the value given by fill. It is useful for results of table
# or tapply when some elements had no records. 

#
#
# 
# @examples
#
# 
# <source>
fill.dimension=function(dataarray,class1,class2,fill=0)
{
 result=data.frame(matrix(fill,nrow=length(class1),ncol=length(class2)))
 rownames(result)=class1
 colnames(result)=class2

 result[rownames(dataarray),colnames(dataarray)]=dataarray
 result[is.na(result)]=fill
 
 return(result)
}
  


#
# 
#
# 
#
# <name>
# fill.1dimension
#
#
# @description
#  Filling a 1D array resulting from tapply. Same as fill.dimension, but for a vector. 

#
#
# 
# @examples
#
# 
# <source>
fill.1dimension=function(dataarray,class1,fill=0)
{
 if(dim(dataarray)[1]<length(class1))
  {
   m=match(class1,dimnames(dataarray)[[1]])
   dataarray=dataarray[m]
   
   names(dataarray)=class1
  }

  dataarray[is.na(dataarray)]=fill
  return(dataarray)
 }
  


#
# 
#
# 
#
# 
# <name>
# convert.factor
#
#
# @description
# Converts any character fields in a dataframe from factors to character fields. 
#
#
# 
# @examples
#
# 
# <source>
convert.factor=function(r)
{
 result=r
 
 for(j in 1:dim(r)[2]) if(is.factor(r[,j])) result[,j]=as.character(r[,j])
 return(result)
}

#
# 
#
# 
#
# 
# <name>
# split.data
#
#
# @description
#  Converts a big dataframe into a list of smaller dataframes, grouped using any
# column in the database, or any external vector. The variable allsplit can be set to a vector
# of data sections to be included; if allsplit includes values not in the data, empty elements
# are included in the list, but if allsplit includes fewer values than found in the data, then
# missing elements are omitted. 
# The option keepsplitcol can be set to TRUE
# in order to retain in the new dataframes the column on which the data are grouped; otherwise, that
# column is removed. 

#
#
# 
# @examples
#
# 
# <source>
split.data=function(censdata,splitcol='sp',keepsplitcol=FALSE,allsplit=NULL,showOutput=NULL)
{
 output=list()

 split=censdata[,splitcol]
 if(!keepsplitcol) includecol=which(colnames(censdata)!=splitcol)
 else includecol=colnames(censdata)
 
 if(is.null(allsplit)) allsplit=sort(unique(split))

 for(i in 1:length(allsplit))
  {
   if(!is.null(showOutput)) if(i%%showOutput==0) cat('working on number ', i, ' -- ', allsplit[i], '\n')
   thisspecies=allsplit[i]
   output[[i]]=subset(censdata,split==thisspecies,select=includecol)
  }

 names(output)=allsplit

 return(output)
}


#
# 
#
# 
#
# <name>
# merge.data
#
#
# @description
#  Combine many dataframes as elements of a list into a single large dataframe. Each individual dataframe must
# have exactly the same columns. This is exactly the opposite operation as split.data. 

#
#
# 
# @examples
#
# 
# <source>
merge.data=function(listdata,showOutput=NULL)
{
 k=length(listdata)
 
 if(k>0) result=listdata[[1]]
 if(k==1) return(result)
 
 if(is.null(dim(result))) headers=names(result)
 else headers=colnames(result)
 
 for(i in 2:k) 
   {
    if(!is.null(listdata[[i]])) 
      {
       nextpart=listdata[[i]]
       
       if(is.null(dim(nextpart))) names(nextpart)=headers
       else colnames(nextpart)=headers
       
       result=rbind(result,nextpart)
      }
    if(!is.null(showOutput)) cat('concatenating element ', i, '\n')
   }
   
 return(result)
}



#
# 
#
# 
#
# <name>
# pst
#
#
# @description
# A version of paste with sep=''.
#
#
# 
# @examples
#
# 
# <source>
pst=function(...)
{
 s=list(...)
 len=length(s)
 
 if(len==0) return(NA)

 result=s[[1]]
 if(len==1) return(s[[1]])

 for(i in 2:len) result=paste(result,s[[i]],sep='')

 return(result)
}



#
# 
#
# 
#
# <name>
# detachfiles
#
#
# @description
#  detachs from the searchpath files matching a submitted vector of names.

#
#
# 
# @examples
#
# 
# <source>
detachfiles=function(filename)
{
 error=character()
 error=paste(filename,': nothing to detach')
 
 for(i in 1:length(filename)) 
  {
   searchlist=search()
   cut=grep(filename[i],searchlist)
   if(length(cut)==1) { detach(pos=cut); error[i]=paste('detached',filename[i]) }
   if(length(cut)>1) error[i]=paste(filename[i],': Too many matches')
  }
  
 for(i in 1:length(error)) cat(error[i],'\n')
 for(i in 1:10) gc()
}



#
# 
#
# 
#
# <name>
# save.searchpath
#
#
# @description
#  Saves all functions in position n to the file already 
# attached at that position; n can be a vector. Allows changes in attached data to be saved easily, but
# please use with care, as it will over-write the existing file. 

#
#
# 
# @examples
#
# 
# <source>
save.searchpath=function(n)
{
 filename=character()
 counter=0

 for(i in n)
  {
   counter=counter+1
   filename[counter]=strsplit(search()[i],'file:')[[1]][2]
   save(list=ls(i),file=filename[counter])
  }

 return(paste('saved file(s)', filename))
}



#
# 
#
# 
#
# <name>
# gsp
#
#
# @description
#  Returns one of the objects at a given search position. This provides a way to write programs to
# check multiple sets of data attached at different positions. 

#
#
# 
# @examples
#
# 
# <source>
gsp=function(searchpos,whichobj=1) 
  return(get(ls(searchpos)[whichobj]))



#
# 
#
# 
#
# <name>
# match.dataframe
#
#
# @description
#  Matches two dataframes using two or more columns. R's function match() works only on vectors 
# (and thus single columns only). The return is a vector of indices, exactly as match() does. 

#
#
# 
# @examples
#
# 
# <source>
match.dataframe=function(x,y)
{
 for(i in 1:dim(x)[2]) x[,i]=as.character(x[,i])
 for(i in 1:dim(y)[2]) y[,i]=as.character(y[,i])
 
 xrow=t(apply(x,1,paste,collapse='-'))
 yrow=t(apply(y,1,paste,collapse='-'))
 
 return(match(xrow,yrow))
}


#
# 
#
# 
#
# <name>
# trim
#
#
# @description
#  Trims leading and trailing blanks from a vector of character variables. Multibyte character strings are returned intact.
# (extended ascii in R appears as hex values).

#
#
# 
# @examples
#
# 
# <source>
trim=function(s)
{
 if(length(s)==0) return(s)
 
 news=s
 hexflag=rep(FALSE,length(s))
 for(i in 1:length(s)) if(is.na(nchar(s[i],'width'))) hexflag[i]=TRUE
 s=s[!hexflag]
 
 len=nchar(s) 
 startblank=which(substr(s[!hexflag],1,1)==' ')
 if(length(startblank)>0) s[startblank]=substr(s[startblank],2,len[startblank])
 
 len=nchar(s) 
 endblank=which(substr(s,len,len)==' ')
 if(length(endblank)>0) s[endblank]=substr(s[endblank],1,len[endblank]-1)

 len=nchar(s) 
 startblank=which(substr(s,1,1)==' ') 
 if(length(startblank)>0) s=trim(s)
 
 len=nchar(s) 
 endblank=which(substr(s,len,len)==' ') 
 if(length(endblank)>0) s=trim(s) 
 
 news[!hexflag]=s
 return(news)
}


#
# 
#
# 
# <name>
# StringToVect
#
#
# @description
# Converts a character string into a vector of individual characters.
#
#
# 
# @examples
# StringToVect('anystring')
#
# <source>
StringToVect=function(s)
{
 result=character()
 len=nchar(s)
 for(i in 1:len) result[i]=substr(s,i,i)
 return(result)
}
#
# 
#
# 
# <name>
# charlocate
#
#
# @description
# Finds position of a substring needle inside a longer string haystack. Can return more than one position if the needle appears more
# than once.  Returns 0 if no matches. The input are atomic: this is not vectorized.
#
#
# 
# @examples
# charlocate("19","x190019xxx")
#
# <source>
charlocate=function(needle,haystack)
{
 if(needle==haystack) return(1)
 
 len1=nchar(needle)
 len2=nchar(haystack)
 if(len2<len1) return(0)
  
 result=numeric()
 for(i in 1:len2)
  {
   subs2=substr(haystack,i,i+len1-1)
   if(needle==subs2) result=c(result,i)
  }
 
 if(length(result)==0) return(0)
 return(result)
}
#
# 
#
# 
# <name>
# left
#
#
# @description
# A standard left function. Returns the leftmost n characters of a string. If n<0, returns all except the rightmost n. If n==0, returns an
# empty string. Arguments can be vectors, but both must be the same length, or n can be a scalar.
#
#
# 
# @examples
#
# 
# <source>
left=function(s,n) 
 {
  if(length(s)==0) return(s)
  
  result=s
  if(length(n)==1) n=rep(n,length(s))
  
  for(i in 1:length(n))
   {
    if(n[i]>0) result[i]=substr(s[i],1,n[i])
    else if(n[i]<0)
      {
		len=nchar(s[i])
	    result[i]=substr(s[i],1,len+n[i])
	  }
	else result[i]=''
   }
	  
  return(result)
}
#
# 
#
# 
#
# <name>
# leftbut
#
#
# @description
#  Returns the leftmost characters of a string, excluding the last n.

#
#
# 
# @examples
#
# 
# <source>
leftbut=function(s,n,bytetype='bytes')
{
 strlen=nchar(s,type=bytetype)
 return(substr(s,1,strlen-n))
}


#
# 
#
# 
#
# <name>
# right
#
#
# @description
#  Returns the rightmost n characters of a string

#
#
# 
# @examples
#
# 
# <source>
right=function(s,n,bytetype='bytes')
{
 strlen=nchar(s,type=bytetype)
 return(substr(s,strlen-n+1,strlen))
}
 

#
# 
#
# 
#
# <name>
# rightbut
#
#
# @description
#  Returns the rightmost characters of a string, excluding the initial n.

#
#
# 
# @examples
#
# 
# <source>
rightbut=function(s,n,bytetype='bytes')
{
 strlen=nchar(s,type=bytetype)
 return(substr(s,n+1,strlen))
}
#
# 
#
# <name>
# explode
#
#
# @description
# Split a single (atomic) character variable into sections, separated on sep. With the default, sep='', it divides the string into it's individual letters.  
#
#
# 
# @examples
#
# 
# <source>
explode=function(str,sep='')
{
 answer=character()
 strlist=strsplit(str,split=sep)
 return(strlist[[1]])
}
#
# 
#
# <name>
# ditch
#
#
# @description
#  Detaches all files at one or more search positions; v can be a vector. 

#
#
# 
# @examples
#
# 
# <source>
ditch=function(v)
{
 invert=(-sort(-v))

 for(i in invert) detach(pos=i)
}




#
# 
#
# 
#
# <name>
# is.leap
#
#
# @description
#  Return a logical indicating which elements of a vector are leap years.

#
#
# 
# @examples
#
# 
# <source>
is.leap=function(yr,start=1904,end=2096)
{
 leapyears=seq(start,end,by=4)
 matchleap=match(yr,leapyears)
 leap=(!is.na(matchleap))

 nonleap=seq(100,3000,by=100)
 matchnonleap=match(yr,nonleap)
 leap[!is.na(matchnonleap)]=FALSE

 leapyears=seq(400,3000,by=400)
 matchleap=match(yr,leapyears)
 leap[!is.na(matchleap)]=TRUE

 return(leap)
}


#
# 
#
# 
#
# <name>
# mergeParam
#
#
# @description
#  Merges a list of parameter matrices into one
# large matrix. Used for the parameter output from MCMCmetrop1R, stored
# as a list. No longer used; superseded by merge.data. 

#
#
# 
# @examples
#
# 
# <source>
mergeParam=function(p)
{
 k=length(p)

 result=p[[1]]
 if(k>1) 
   for(i in 2:k) result=rbind(result,p[[i]])

 return(result)
}



#
# 
#
# 
#
# <name>
# countzero
#
#
# @description
#  Counts vector elements exactly zero.

#
#
# 
# @examples
#
# 
# <source>
countzero=function(x) 
 return(length(x[x==0 & !is.na(x)]))


#
# 
#
# 
#
# <name>
# countone
#
#
# @description
#  Counts vector elements exactly one.

#
#
# 
# @examples
#
# 
# <source>
countone=function(x) 
 return(length(x[x==1 & !is.na(x)]))


#
# 
#
# 
#
# <name>
# countNA
#
#
# @description
#  Counts vector elements that are NA.

#
#
# 
# @examples
#
# 
# <source>
countNA=function(x) 
 return(length(x[is.na(x)]))


#
# 
#
# 
#
# <name>
# countEmpty
#
#
# @description
#  Counts vector elements that are NA or a string of no length.

#
#
# 
# @examples
#
# 
# <source>
countEmpty=function(x) 
 return(length(x[is.na(x) | x=='']))


#
# 
#
# 
#
# <name>
# countpresent
#
#
# @description
#  Counts vector elements > 0. See countspp as well.

#
#
# 
# @examples
#
# 
# <source>
countpresent=function(x) 
 return(length(x[x>0 & !is.na(x)]))


#
# 
#
# 
#
# <name>
# find.nonNA
#
#
# @description
#  Returns the first value of a vector x which is not NA

#
#
# 
# @examples
#
# 
# <source>
find.nonNA=function(x)
{
 good=which(!is.na(x))[1]
 return(x[good])
}



#
# 
#
# 
#
# <name>
# which.nonNA
#
#
# @description
#  Returns the first index at which a vector x is not NA

#
#
# 
# @examples
#
# 
# <source>
which.nonNA=function(x)
{
 good=which(!is.na(x))[1]
 return(good)
}



#
# 
#
# 
#
# <name>
# which.allnonNA
#
#
# @description
#  Finds all values of a vector which are not NA. Fills out a vector to
# length 6 with NAs if there are fewer than 6.

#
#
# 
# @examples
#
# 
# <source>
which.allnonNA=function(x)
{
 good=which(!is.na(x))

 result=rep(NA,6)
 if(length(good)==0) return(result)
 result[1:length(good)]=x[good]

 return(result)
}



#
# 
#
# 
#
# <name>
# which.vmatch
#
#
# @description
#  Finds which subsequent element of a vector matches the first element.

#
#
# 
# @examples
#
# 
# <source>
which.vmatch=function(v)
{
 v2=v[-1]
 return(match(v[1],v2))
}




#
# 
#
# 
#
# <name>
# logical.grep
#
#
# @description
#  A form of grep returning logical instead of indices (numbers).

#
#
# 
# @examples
#
# 
# <source>
logical.grep=function(needle,haystack)
{
 result=rep(FALSE,length(haystack))
 result[grep(needle,haystack,fixed=TRUE)]=TRUE
 return(result)
}
#
# 
#


#
# <name>
# nhd
#
#
# @description
#  A version of head with only 6 columns shown.

#
#
# 
# @examples
#
# 
# <source>
nhd=function(d,w=1:6,h=8) 
  print(head(d,h)[,w])


#
# 
#
# 
#
# <name>
# TextToRdata
#
#
# @description
#  Reads a tab-delimited text file and save as rdata.

#
#
# 
# @examples
#
# 
# <source>
TextToRdata=function(infile,outfile,object,NAs='NA')
{
 assign(object,read.delim(infile,as.is=TRUE,na.strings=NAs))
 if(dim(get(object))[1]>0) save(list=object,file=outfile)
 else cat('No lines in ', infile, '\n')
}



#
# 
#
# 
#
# <name>
# CountByGroup
#
#
# @description
#  Groups a dataframe by one or more columns (named by groupcol). This does exactly what
# COUNT(*) GROUP BY does in SQL.

#
#
# 
# @examples
#
# 
# <source>
CountByGroup=function(data,groupcol,count_to_data=FALSE,countname='count',groupname='groupID')
{
 uniq=unique(data[,groupcol])
 noneed=FALSE

 if(length(groupcol)==1) 
  {
   uniq=data.frame(uniq,rep(1,length(uniq)))
   colnames(uniq)[1]=groupcol
   colnames(uniq)[2]=countname
  }
 else uniq[,countname]=1
 uniq[,groupname]=1:dim(uniq)[1]

 if(dim(uniq)[1]==dim(data)[1]) return(uniq)
  
 if(length(groupcol)==1) m=match(data[,groupcol],uniq[,groupcol])
 else m=match.dataframe(data[,groupcol],uniq[,groupcol])
 freq=table(m)

 uniq[as.numeric(names(freq)),countname]=freq
 return(uniq)
}
 


#
# 
#
# 
#
# <name>
# which.maxNAs
#
#
# @description
# An unfortunate bug in which.max: if all elements are NA, it doesn't return anything. This means that for any vector, which.max returns
# a vector of length 1 unless all are NAs. This is a silly error, since a program expecting an element of length 1 falls apart otherwise.
#
#
# 
# @examples
#
# 
# <source>
which.maxNAs=function(v)
{
  if(length(which(!is.na(v)))==0) return(NA)
  return(which.max(v))
}
#
# 
#
# 
#
# 
#
# <name>
# attach_if_needed
#
#
# @description
# Attach one or more datafiles,checking first whether the file is already attached. If it is attached,
# it is not reattached, and the search position where attached is returned. 

#
#
# 
# @examples
#
# 
# <source>
attach_if_needed=function(datafiles)
{
 already=numeric()
 j=1
 
   for(file in datafiles)
    {
     found=grep(file,search())
     
     if(length(found)==0) 
      {
       attach(file)
       already[j]=2
       if(j>1) for(k in 1:(j-1)) already[k]=already[k]+1
      }
     else already[j]=found
     
     j=j+1
    }
 
 return(already)
}


#
# 
#
# 
#
# <name>
# IfElse
#
#
# @description
# A more convenient version of the R function ifelse in cases where test, a, and b are atomic.  
#
#
# 
# @examples
#
# 
# <source>
IfElse=function(test,a,b)
{
 if(test) return(a)
 else return(b)
}

#
# <name>
# AssignDiag
#
#
# @description
# A way to assign the diagonals of a matrix that can handle input having no dimensions. Ordinarily, x is square matrix and newdiag is a vector equal in length to x's diagonal.
# A new x is returned having the newdiag on its diagonal. In that usage, it matches the assign option for R's function diag. 
# This improves diag by handling x with no dimensions, ie a scalar, or just one dimension. Then newdiag is simply returned. 
#
#
# 
# @examples
#
# 
# <source>
AssignDiag=function(x,newdiag)
{
 if(is.null(dim(x))) return(newdiag)
 if(length(dim(x))==0) return(newdiag)
 diag(x)=newdiag
 return(x)
}
#
# 
#
# 
# <name>
# vectToCommas
#
#
# @description
# Given a vector of character variables, collapse into a single string with quotes, separated by commas
#
#
# 
# @examples
#
# 
# <source>
vectToCommas=function(v)
{
	withquotes = paste("'",v,"'",sep="")
    return(paste(withquotes,collapse=","))
}

#
# 
#
# 


# <name>
# drp
#
#
# @description
# A version of drop which includes as.matrix. Without it, drop does not serve its purpose. This is necessary in many many situations
# where a single row is taken out of a dataframe, but must be passed as a vector.
#
#
# 
# @examples
#
# 
# <source>
drp=function(x)
{
	return(drop(as.matrix(x)))
}

#
# 
#
# 

# <name>
# randomRow
#
#
# @description
# Return a random row from a dataframe
#
#
# 
# @examples
#
# 
# <source>
randomRow=function(data)
{
 size=dim(data)[1]
 if(size==0) return(data)

 r=sample.int(size,1)
 return(data[r,])
}
#
# 

# <name>
# randomRow
#
#
# @description
# Return a random element from a vector
#
#
# 
# @examples
#
# 
# <source>
randomElement=function(data)
{
 size=length(data)
 if(size==0) return(data)

 r=sample.int(size,1)
 return(data[r])
}
#
# 


# <name>
# countUnique
#
#
# @description
# Count the number of unique elements in a vector
#
#
# 
# @examples
#
# 
# <source>
countUnique=function(x) 
  return(length(unique(x)))
#
# 


# <name>
# graphFilledBand
#
#
# @description
# Fill the area between two curves on a graph. Useful for confidence limits, for example. 
# Typical use is to draw a graph first with some central y values, then add a confidence band by filling the area between upper and lower confidence limits
# (designated by variables lower.y and upper.y in the example below). The central line should then be redrawn over the filled area.
#
#  @param x = The x axis values, as in any R graph
#  @param y1, y2 = Two sets of y axis values, each of exactly the same length as x
#  @param fillcol = The color filling the area between the two curves
#  @param add = TRUE or FALSE, as in other R graphs; if TRUE, there must be an appropriate existing graph, otherwise, a new graph is drawn
#  @param linecol = If add==FALSE, lines drawn at y1 and y2 are this color; ignored if add==TRUE
#  @param ltype = If add==FALSE, lines drawn at y1 and y2 are this type ('solid','dashed',etc);  ignored if add==TRUE
#  @param lwidth = If add==FALSE, lines drawn at y1 and y2 are this thickness (a number);  ignored if add==TRUE
#
# @examples
# plot(x,y,type='l')
# graphFilledBand(x,lower.y,upper.y)
# lines(x,y)
#
# <source>
graphFilledBand=function(x,y1,y2,fillcol='gray',add=TRUE,linecol='black',ltype='solid',lwidth=1)
{
 nopt=length(x)
 if(length(y1)!=nopt) return('To fill confidence band, y1 and y2 must be same length as x\n')
 if(length(y2)!=nopt) return('To fill confidence band, y1 and y2 must be same length as x\n')

 ord=order(x)
 x=x[ord]
 y1=y1[ord]
 y2=y2[ord]
 
 if(!add) plot(x,y1,type='l',lty=ltype,lwd=lwidth,col=linecol)
 if(!add) lines(x,y2,lty=ltype,lwd=lwidth,col=linecol)
 
 poly.x=c(x,x[nopt1])
 poly.y=c(y1,y2[nopt:1])
 polygon(poly.x,poly.y,col=fillcol,border=NA)
}
#
# 


# <name>
# make.CredIntervalVect
#
#
# @description
# Take a vector y having mean then lower and upper credible limits and convert to character string with parentheses. Result is a single (atomic) character vector with mean followed by parentheses enclosing lower and upper limits. This is not vectorized: it will work only with a single vector of length 3. Use apply to repeat for rows of a matrix. 
#
#  @param y must be a vector of length 3, not a matrix. 
#  @param digits number of decimal places in result, first for the mean, then the two credible limits
#
# @examples
# make.CredIntervalVect(c(3.124,2.76,5.01),digits=c(2,1))
#
# <source>
make.CredIntervalVect=function(y,digits=c(3,3),CIonly=FALSE)
{
 if(length(digits)==1) digits=rep(digits,2)
 CI=pst('  (',decimal.form(y[2],digits[2]),',',decimal.form(y[3],digits[2]),')')
 result=pst(decimal.form(y[1],digits[1]),CI)
 
 if(CIonly) return(CI)
 return(result)
}
#
# 
