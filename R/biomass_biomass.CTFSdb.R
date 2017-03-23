
#'
#'


#' biomass.CTFSdb
#'#'
#' @description
#' Calculate biomass from existing R-formatted tables for trees and stems using dbh allometry. By default, it uses the Chave (2005) equations.
#' Note that the standard downloads of R Analytical Tables, already has the agb column filled, calculated with this routine using the default parameters for moist forest. 
#' This program can be used to repeat or redo the calculation. It can also be used for other tables, always requiring two tables:  one with trees and one with all stems. 
#' The function returns the same table as submitted (either tree or stem), with a column agb added; if the agb column was already present, it is replaced. 
#' Calculations are done by AGB.ind and the subroutines it call.
#' An alternative option is to have AGB calculations already stored in the server's AGB database, setting useChave=FALSE. This function then looks up the AGB for 
#' each stem from the table named for the plot. 
#'#' RStemTable: Name of table with one row per stem; must have dbh, species (column sp), treeID
#' RTreeTable: Name of table with one row per tree; must have dbh, species (column sp), treeID
#' whichtable: Set to "tree" to return the entire tree table with agb, or to "stem" to return stem table
#' dbhunit: Set to "mm", "cm", or "inch"
#' plot: Name of plot, matching the plot name used in the wood-density table
#' wsgdata: Name of R object having wood-density for species in all CTFS plots
#' forest: Set to "moist", "dry", or "wet" to use the equations publshed in Chave (2005) for the 3 forest types
#' ht.param: A vector of parameters for a formula relating tree height to dbh; if NULL, the biomass formula does not use height
#' htmodel: Name of an R function that returns tree height giving a dbh and any number of parameters; if ht.param is NULL, htmodel is ignored 
#'#'@examples
#' \dontrun{
#' CTFSplot("bci","full",census=1) 
#' CTFSplot("bci","stem",census=1) 
#' attach("biomass/wsg.ctfs.Rdata") 
#' newtable=biomass.CTFSdb(RStemTable=bci.stem1,RTreeTable=bci.full1)}
#'
#'
'biomass.CTFSdb'


#' density.ind
#'#'
#' @description
#' Create a vector of wood density for each individual tree based on the species name and plot. The table of individuals, called df,
#' must include a dbh and a species name, the latter named sp. There must be a table of wood density submitted (wsgdata), and this
#' table must have a column sp with species names, a column plot, plus the wood density in a column called wsg (though the
#' name of that column can be changed using the argument denscol). The CTFS wood-density table has this structure, but any table with those
#' columns will work. If a species in the df table has a matching species name in the correct plot, its wood density is taken.
#' If a species is not found in the correct plot, then the mean wood density of all species in the same plot is taken. The function
#' fails (returns only NAs) if there are no entries for the selected plot in the wood-density table.  
#' Returns a vector of wood density of the same size as the df table submitted. 
#'#'
#'
#' @examples
#' \dontrun{
#' wooddens=density.ind(df=bci.full1,plot="bci",wsg=wsg.ctfs2) #'
#' mean(wooddens,na.rm=TRUE) #'
#' length(which(is.na(wooddens)))}
#'
#'
'density.ind'


#' AGB.ind
#'#'
#' @description
#' Compute biomass (agb) based on one of the Chave (Oecologia, 2005) models for tropical forest types. 
#' Requires a table (df) with dbh and species names, a wood-density table (described under density.ind), a plot name, 
#' dbh units, and a forest type (for most lowland tropical plots, the default moist is recommended.)
#' The height parameters default to NULL, and the Chave equations without height are then used. Alternatively, height parameters
#' and a height function can be supplied, the latter to calculate height from diameter for every tree, in which case the
#' Chave model with height is used. Returns a vector of biomass in kg for every individual in the submitted table df. 
#' This is called by AGB.tree in the standard calculation of biomass for CTFS R tables. 
#'#'
#'
#' @examples
#' \dontrun{
#' biomass=AGB.ind(df=bci.full1) #'
#' hist(log(biomass),breaks=100) #'
#' sum(biomass,na.rm=TRUE)/50}
#'
#'
'AGB.ind'


#' AGB.tree
#'#'
#' @description
#' Computes AGB of each tree in a table, grouping all stems of one tree and adding there agbs. 
#' The submitted table, df, must have dbh, species name (sp),
#' and a treeID to identify which tree every stem belong to. There must be just one dbh for each stem.  Returns
#' a dataframe with one row per tree, including the treeID and total agb per tree. Note that it will have fewer rows
#' than the table submitted. This is called by biomass.CTFSdb in the standard calculation of biomass for CTFS R tables. 
#'#' biomasstbl=AGB.tree(df=bci.stem1)
#' dim(bci.stem1)
#' dim(biomasstbl)
#' head(biomasstbl)
#'#'
#'
#'
'AGB.tree'


#' Chave.AGB
#'#'
#' @description
#' The Chave 2005 Oecologia model for calculating biomass from dbh in cm. All dbhs are submitted as a vector, and a vector of wood density
#' of the same length must also be submitted (or a single wood density can be passed, to be used for every tree). 
#' Parameter values for the 3 forest types according to Chave 2005 are hard-coded in the function.
#' The recommended CTFS use is with htparam=NULL, so height is not used. If height parameters and a height model are passed,
#' then the height of every tree is calculated, and the Chave AGB formula that includes height is used. The default height parameters are 
#' from Chave et al 2003 on BCI biomass, and the default height function is predht.asym, provided in this file. But any height model can be
#' substituted, providing the function name is passed and the necessary number of parameters included as htparam. 
#' Returns a vector of biomass of same length as vector of dbh submitted. 
#' This is called by AGB.tree in the standard calculation of biomass for CTFS R tables.
#'#'
#'
#' @examples
#' \dontrun{
#' testdbh=c(1,2,5,10,20,30,50,100,200) #'
#' AGBmoist=Chave.AGB(dbh=testdbh,forest="moist") #'
#' AGBwet=Chave.AGB(dbh=testdbh,forest="wet") #'
#' plot(testdbh,AGBmoist,col="green",type="l") #'
#' lines(testdbh,AGBwet,col="blue")}
#'
#'
'Chave.AGB'


#' agb.model
#'#'
#' @description
#' Calculates biomass from density, height, and dbh. Requires just two parameters, following Chave (2005). The parameters can be
#' changed, but the formula cannot be. Returns a vector of biomass as long as vector of dbh submitted. 
#' This is called by Chave.AGB in the standard calculation of biomass for CTFS R tables.
#'#'
#'
#' @examples
#' \dontrun{
#' agb.model(dbh=c(1,1,2),density=c(.6,.6,.5),height=c(2,3,4),param=c(.0501,1))}
#'
#'
'agb.model'


#' agb.dbhmodel
#'#'
#' @description
#' Calculates biomass from density and diameter, without height. Requires four parameters, following Chave (2005). 
#' The parameters can be changed, but the formula cannot be. Returns a vector of biomass as long as vector of dbh submitted. 
#' This is called by Chave.AGB in the standard calculation of biomass for CTFS R tables.
#'#'
#'
#' @examples
#' \dontrun{
#' agb.dbhmodel(dbh=c(1,1,2),density=c(.6,.6,.5),param=c(-1.499,2.148,0.207,-0.0281)) }
#'
#'
'agb.dbhmodel'


#' predht.asym
#'#'
#' @description
#' An allometric model predicting an asymptote at large size, used in estimating tree height as a function of dbh. 
#' The model uses 3 parameters, submitted as argument param. The matrix form of param allows a different set of parameters to
#' be submitted for every species. The default parameters given in the function Chave.AGB assume dbh is in cm, as do all the biomass
#' allometry functions. 
#'#' dbh: Vector of dbh
#' param: Either a vector of length 3, or a matrix of 3 columns; if the latter, there must be one row for each dbh
#'#'@examples
#' \dontrun{
#' htparam=c(41.7,.057,.748) #'
#' d=c(1,2,5,10,20,50) #'
#' ht=predht.asym(dbh=d,param=htparam)}
#'
#'
'predht.asym'


#' biomass.change
#'#'
#' @description
#' Finds biomass in two censuses and change between them. The submitted dataframes are exactly the standard CTFS R Analytical tables,
#' with a column for biomass (agb) already calculated. Each dataframe has a record for every tree in a single census (or a stem
#' table can be passed, with one record for each stem). Biomass for all living trees is summed over whatever grouping variables are 
#' submitted (split1 and
#' split2) in both censuses, along with the annual rate of change in each category. Returns a list with 6 components:
#' N.1: Total biomass in first census submitted
#' N.2: Total biomass in second census submitted
#' date1: Mean date in first census
#' date2: Mean date in second census
#' interval: The mean census interval in years
#' little.r: The rate of biomass change, or (log(N.2)-log(N.1))/interval
#' If no split variables are submitted (split1=split2=NULL), each component of the list is a single number, for the entire plot. 
#' If split1 is submitted but not split2, each component is a vector, one value for each category of split. If both splits are 
#' submitted, each component of the list is a matrix. 
#' Based closely on the function pop.change in abundance.r, and differs only in taking sum of agb instead of counting individuals.
#'#' census1: The R Analytical Table for a single census, either tree or stem
#' census2: The matching R Analytical Table for a later census
#' alivecode: A vector of status codes that indicate a tree is alive (usually just "A" in most CTFS R tables)
#' mindbh: The minimum dbh to include (if NULL, all dbhs included); biomass is summed in trees larger that this
#' split1: A vector of exactly the same length as the number of rows in census1 and census2, with a grouping variable for each tree;
#' a common use is the species name
#' split2: Another split vector of the same length, for example a dbh category
#'#'@examples
#' \dontrun{
#' CTFSplot("bci","full",census=c(3,7)) #'
#' deltaAGB=biomass.change(bci.full3,bci.full7) #'
#' deltaAGB.spp=biomass.change(bci.full3,bci.full7,split1=bci.full3$sp) #'
#' deltaAGB.table=assemble.demography(deltaAGB.spp,type="a")  #'
#' rate=deltaAGB.table$little.r #'
#' hist(rate,breaks=50) #'
#' summary(rate[is.finite(rate)]) #'
#' subset(deltaAGB.table,is.infinite(rate)) }
#'
#'#'
#'#'
'biomass.change'


#' AGB.dbtable
#'#'
#' @description
#' This function looks up the database named AGB in the MySQL server to get a table of biomass per stem. The MySQL table must have treeID, stemID, censusID,
#' and a column agb with biomass in tons. This is used in cases where biomass for a plot has been calculated separately, using a method other than one of
#' the Chave allometric equations. The alternative AGB calculation is stored for each plot in this database. The name of the table matches the plot name. 
#'#' df: A table of individual stems.
#' plot: The plot name
#'#'
#'

'AGB.dbtable'
