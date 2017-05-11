
# Roxygen documentation generated programatically -------------------

#'
#'

#' Calculates alpha from a stem and species count submitted using Newt...
#'
#' @description
#'
#' Calculates alpha from a stem and species count submitted using Newton's method.
#'
#'
'calcalpha'

#' The derivative for use in Newtons methodd.calcS.alpha calcS.alpha  ...
#'
#' @description
#'
#' The derivative for use in Newton's method
#'
#'
'd.calcS.alpha'

#' Estimated number of species in a sample of N individuals, given Fis...
#'
#' @description
#'
#' Estimated number of species in a sample of N individuals, given Fisher's alpha
#'
#'
'calcS.alpha'

#' takes a full plot dataframe and calculates species number in a rand...
#'
#' @description
#' takes a full plot dataframe and calculates species number in a
#' random subsample of s individuals. All it requires is a column
#' called sp which has species designation for each individual. 
#'
#'

'rarefy.diversity'

# Source code and original documentation ----------------------------
# 
# <function>
# <name>
# calcalpha
# </name>
# <description>
#  Calculates alpha from a stem and species count submitted using Newton's method.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
#' @export

calcalpha=function(n, s)
{
 bads=(s==0 | n<=s) 
 ind=n[!bads]
 spp=s[!bads]

 alpha=rep(NA,length(n))
 a=rep(20,length(ind))
 poorest=rep(TRUE,length(ind))

 while(length(a[poorest])>0)
  {
   f=calcS.alpha(ind,a)-spp
   a=a-f/d.calcS.alpha(a,ind,spp)
   poorest=abs(f)>1e-2
   a[a<=0]=1
  }
 
 alpha[!bads]=a
 return(alpha)
}


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# d.calcS.alpha
# </name>
# <description>
#  The derivative for use in Newton's method

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
#' @export

d.calcS.alpha=function(a, n, s)
  return(log(1+n/a)-n/(a+n))


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# calcS.alpha
# </name>
# <description>
#  Estimated number of species in a sample of N individuals, given Fisher's alpha

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
#' @export

calcS.alpha=function(n, a) 
  return( a*log(1+n/a) )
 


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# rarefy.diversity
# </name>
# <description>
#  takes a full plot dataframe and calculates species number in a
# random subsample of s individuals. All it requires is a column
# called sp which has species designation for each individual. 


# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
#' @export

rarefy.diversity=function(fulldata,s)
{ 
 n=dim(fulldata)[1]
 
 randsample=fulldata[sample(1:n,s,replace=T),]
 
 return(length(unique(randsample$sp)))
}
 
 

# </source>
# </function>
# 
# 
