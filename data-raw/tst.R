# It requires fill.dimension and climits in utilities.r.<br><br>
# Output of the mortality function is a list with components:
# <ul>
# <li>N, the number of individuals alive in the census 1 per category selected
# <li>D, the number of individuals no longer alive in census 2
# <li>rate, the mean annualized mortality rate constant per category selected, calculated as (log(N)-log(S))/time 
# <li>upper, upper confidence limit of mean rate
# <li>lower, lower confidence limit of mean rate
# <li>time, mean time interval in years
# <li>date1, mean date included individuals were measured in census 1, as julian object (R displays as date, but treats as integer)
# <li>date2, mean date in census 2 
# <li>dbhmean, mean dbh in census 1 of individuals included
# </ul>

# Pass the list to assemble.demography (in utilities.r) with type="m" to convert the list a data.frame.
# </description>
# <arguments>
# <ul>
# <li> Generally, alivecode="A" suffices, as this is the standard in CTFS data for a living tree; "AS" and "AB" are seldom used now
# <li> split1 and split2 must both be vectors of character variables with exactly as many elements as there are rows in the tables census1 and census2
# (or both can be NULL), for instance, species names, dbh categories, or quadrat numbers<br>
# </ul>
# </arguments>
# much more stuffffffffffffff
