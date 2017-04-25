
# Roxygen documentation generated programatically -------------------

#'
#'

#' individual_grow.table
#'
#' @description
#'
#' Create a table of individual trees and their growth over two censuses, with many species included.
#'
#' The flag ctr is used to center the time variable, which is the number of
#' years since 1960 of the interval midpoint. There is a logarithmic
#' transformation, for which all growth <= 0 is converted to mingrow. There is
#' also a power transformation, where each growth rate is raised to the power
#' given by powertransformation.
#'
#' In the latter, negative growths are transformed to negative, so do not need
#' to be corrected.
#' 
#' @param cnsdata A list of census data sets, for example: 
#'   `list(bci::bci12full1, bci::bci12full2, bci::bci12full3)`.
#' @param rnd used to rounddown dbhs for certain intervals. This argument
#'   indicates that dbhs<50 mm are rounded down to 5-mm, necessary because
#'   saplings at BCI in 1982 and 1985 were measuring in 5-mm increments. The
#'   growth functions in the CTFS R Package handle this.
#'   
#' @return 
#' A data frame where each row gives data from one individual, including:
#' - `tag`: the tree’s tag number;
#' - `gx`, `gy`: coordinates;
#' - `species`: species;
#' - `dbh1`, `dbh2`: tree diameter at census 1 and 2;
#' - `census`, `censusfact`: census interval; 1 means growth between census 1 
#' and 2, and 2 means growth from census 2 to 3, etc. `census` and `censusfact` 
#' store the same information but they differ in that their values are integers 
#' and factors;
#' - `time`: mid-point of the interval over which growth was measured, centered
#' on 1992. This is the format needed by lmer. The reason for centering time is
#' that it is much better in linear regressions to have the x-axis near zero;
#' - `LnSize`: log(dbh1);
#' - `incgr`: untransformed growth increment; growth increment (difference in
#' dbh) per year;
#' - `LnGrowth`: log of the growth rate, with negatives and zeroes converted to
#' 0.1 (from the argument mingrowth in the function);
#' - `CRGrowth`: cube root of growth rate; power transformation of the growth
#' rate, with negatives maintained negative (exponent = 0.45). Althouth you may
#' use `incgr` or `LnGrowth`, `CRGrowth` allows cleaner analyses, that need no
#' assumption about negative growths.
#' 
'individual_grow.table'

#' individual_mort.table
#'
#' @description
#'
#' Create a table of individual trees and their survival status over two censuses, with many species included. 
#'
#' @seealso individual_grow.table
#'
'individual_mort.table'

#' calcMortIndivTable
#'
#' @description
#'
#' Calculate mortality rate per species per census interval using the output of individual_mort.table. 
#'
#' Formerly named calcMortlmerTable.
#'
#'
'calcMortIndivTable'

#' lmerMortLinear
#'
#' @description
#'
#' A linear model of an annual mortality parameter [which is -log(annual survival)] as a function of N predictors x, which must be the first N columns of x. 
#'
#' The parameters are standard slope and intercept for a linear model. There must be one additional column in x for the time interval t. The linear model predicts annual log(survival).
#'
#' Return value is predicted survival rate (probability) over an interval of t years. Nothing prevents the output from being outside (0,1); that must be handled in the likelihood function.
#'
#'
'lmerMortLinear'

#' lmerMortFixedTime
#'
#' @description
#'
#' A model for mortality as a function of a single predictor variable, with the time interval for each individual incorporated (as a secondpredictor).
#'
#' The predictor must be an integer. The log(mortality parameter) is modeled as a different value for each distinct predictor. The number of parameters must exceed the maximum value of the predictor. 
#'
#' The return value is a survival probability. Nothing prevents the output from being outside (0,1); that must be handled in the likelihood function.
#'
#'
'lmerMortFixedTime'

# Source code and original documentation ----------------------------
# <function>
# <name>
# individual_grow.table
# </name>
# <description>
# Create a table of individual trees and their growth over two censuses, with many species included. 
# The option rnd is used to rounddown dbhs for certain intervals. The flag ctr is used to center the time variable,
# which is the number of years since 1960 of the interval midpoint. There is a logarithmic transformation, for which all growth<=0
# is converted to mingrow. There is also a power transformation, where each growth rate is raised to the power given by powertransformation.
# In the latter, negative growths are transformed to negative, so do not need to be corrected. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
#' @export
individual_grow.table <- function(cnsdata,
                                  powertransformation = 0.45,
                                  rnd = c(FALSE, TRUE, rep(FALSE, 4)),
                                  mingrow = 0.1,
                                  mindbh = 10,
                                  maxdbh = 10000,
                                  maxerrorSD = 4,
                                  maxerrorGrow = 75,
                                  center = 1992,
                                  debug = FALSE) {
  for(i in 1:(length(cnsdata)-1))
    {
        cns=i:(i+1)
        gtbl=growth.indiv(cnsdata[[i]],cnsdata[[i+1]],rounddown=rnd[i],err.limit=maxerrorSD,maxgrow=maxerrorGrow)
        gtbl$time=(cnsdata[[i+1]]$date+cnsdata[[i]]$date)/(2*365.25)
        gtbl$census=i
        gtbl$censusfact=as.factor(i)
        # browser()
        
        cond <- !is.na(gtbl$incgr) & gtbl$dbh1 < maxdbh & gtbl$dbh1 >= mindbh
        section <- gtbl[cond, , drop = FALSE]
        
        if(i==1) final=section
        else final=rbind(final,section)
    }
  
  final$growth=final$incgr
  final$growth[final$incgr<=0]=mingrow
  final$LnGrowth=log(final$growth)
  final$LnSize=log(final$dbh1)-mean(log(final$dbh1))
  final$CRGrowth=pospower(final$incgr,powertransformation)
  if(debug) browser()
  
  colnames(final)[which(colnames(final)=='sp')]='species'
  
  vars <-c(
     'treeID',
     'tag',
     'gx',
     'gy',
     'species',
     'dbh1',
     'dbh2',
     'LnSize',
     'incgr',
     'LnGrowth',
     'CRGrowth',
     'time',
     'census',
     'censusfact'
  )
  final <- final[final$status2 != 'M', vars, drop = FALSE]
  
  if(!is.null(center)) final$time=final$time+1960-center
     
  return(final)
}

# </source>
# </function>
# 
# 

# <function>
# <name>
# individual_mort.table
# </name>
# <description>
# Create a table of individual trees and their survival status over two censuses, with many species included. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
#' @export

individual_mort.table <- function(cnsdata,
                                  mindbh = 10,
                                  maxdbh = 10000,
                                  alivecode = c("A", "AB", "AS"),
                                  center = 1992) { 
  for(i in 1:(length(cnsdata)-1))
    {
        cns=i:(i+1)
        
        vars <- c('date', 'treeID', 'tag', 'sp', 'gx', 'gy', 'dbh', 'status')
        section <- cnsdata[[i]][vars]
        
        section$status2=cnsdata[[i+1]]$status
        section$date2=cnsdata[[i+1]]$date
        section$census=i
        
        cond <- section$status == 'A' & 
          section$dbh >= mindbh & 
          section$dbh < maxdbh
        section <- section[cond, , drop = FALSE]
        
        if(i==1) final=section
        else final=rbind(final,section)
    }
    
  final$interval=(final$date2-final$date)/365.25
  final$time=(final$date2+final$date)/(2*365.25)+1960-center
  
  final=subset(final,status2!='M',select=c('treeID','tag','sp','gx','gy','dbh','interval','status2','time','census'))
  final$censusfact=as.factor(final$census)
  
  A=rep(NA,dim(final)[1])
  for(i in 1:length(alivecode)) A[final$status2==alivecode[i]]=TRUE
  A[final$status2=='D']=FALSE
  final$fate=A
  final$status2=NULL
  
  return(final)
  }
# </source>
# </function>
# 
# 

# 
# <function>
# <name>
# calcMortIndivTable
# </name>
# <description>
# Calculate mortality rate per species per census interval using the output of individual_mort.table. 
# Formerly named calcMortlmerTable.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
#' @export

calcMortIndivTable=function(mtable,by='species')
{
 if(by=='species') splitby=list(mtable$sp,mtable$census)
 else splitby=mtable$census
 
 S=tapply(mtable$fate,splitby,sum)
 S[is.na(S)]=0
 N=tapply(mtable$fate,splitby,length)
 N[is.na(N)]=0
 time=tapply(mtable$interval,splitby,mean,na.rm=TRUE)
 
 mortality=mortality.calculation(N,S,time)$rate
 return(mortality)
}
# </source>
# </function>
# 
# <function>
# <name>
# lmerMortLinear
# </name>
# <description>
# A linear model of an annual mortality parameter [which is -log(annual survival)] as a function of N predictors x, which must be the first N columns of x. 
# The parameters are standard slope and intercept for a linear model. There must be one additional column in x for the time interval t. The linear model predicts annual log(survival).
# Return value is predicted survival rate (probability) over an interval of t years. Nothing prevents the output from being outside (0,1); that must be handled in the likelihood function.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
#' @export

lmerMortLinear=function(x,param,...)
{
 if(!is.array(x)) x=array(x,dim=c(1,length(x)))
 ncol=dim(x)[2]
 xpred=x[,-ncol]
 time=x[,ncol]
 
 lograte=linear.model(x=xpred,param=param)
 annualrate=exp(lograte)
 surv=exp(-annualrate*time)
 
 return(surv)
}
# </source>
# </function>

# <function>
# <name>
# lmerMortFixedTime
# </name>
# <description>
# A model for mortality as a function of a single predictor variable, with the time interval for each individual incorporated (as a secondpredictor).
# The predictor must be an integer. The log(mortality parameter) is modeled as a different value for each distinct predictor. The number of parameters must exceed the maximum value of the predictor. 
# The return value is a survival probability. Nothing prevents the output from being outside (0,1); that must be handled in the likelihood function.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
#' @export

lmerMortFixedTime=function(x,param)
{
 if(!is.array(x)) x=array(x,dim=c(1,length(x)))
 interval=x[,2]
 predictor=x[,1]
 # browser()
 
 logpred=numeric()
 noparam=length(param)
 for(i in 1:noparam) 
  {
   found=predictor==i
   logpred[found]=param[i]
  }
 # browser()
  
 pred=(-1)*exp(logpred)
 survprob=exp(pred*interval)
 return(survprob)
}
# </source>
# </function>
# 
