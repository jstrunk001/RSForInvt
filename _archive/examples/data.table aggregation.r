if(F){
# Not run:
example(data.table)  # to run these examples at the prompt
## End(Not run)

DF = data.frame(x=rep(c("b","a","c"),each=3), y=c(1,3,6), v=1:9)
DT = data.table(x=rep(c("b","a","c"),each=3), y=c(1,3,6), v=1:9)
DF
DT

DT <- data.table(V1=c(1L,2L),
V2=LETTERS[1:3],
V3=round(rnorm(4),4),
V4=1:12)
DT[, print(.SD), by=V2]

f1=function(){



  DT <- data.table(V1=c(1L,2L),
  V2=LETTERS[1:3],
  V3=round(rnorm(4),4),
  V4=1:12)
  DT[, print(.SD), by=V2]

}
f2=function()f1()
f1()
f2()

#jacob test
f1=function(z)list(a=mean(z$v+z$y),b=mean(z$v-7))
DT[,{f1(.SD)},by=x]

library(lidR)
las1=readLAS("G:\\data\\2015_naip_phodar\\319_169.las")
dtlas=las1@data
extlas=extent(t(apply(dtlas[,c("X","Y")],2,function(x)c(min(x),max(x)))))
r0=raster(extlas,resolution=66)
dtlas[,"cells"] <- cellFromXY(object=r0, xy=data.frame(dtlas[,c("X","Y")]))
ag1=dtlas[,compute_metrics2(.SD,as_list=F,outliers=NA),by=cells]


f1=function(x){list(sum(x$X),sum(x$Z))}
test=dtlas[,{f1(.SD)},by=Y,as_list]

las2=las1$pts
coordinates(las2)=~X+Y
las2@data[,c("X","Y")]=coordinates(las2)
dtlas2=data.table(las2@data)
dtlas2[,c("X","Y")]=coordinates(las2)

f1=function(x){list(sum(x$X),sum(x$Z))}
test=dtlas[,{f1(.SD)},by=Y]
test2=dtlas2[,{f1(.SD)},by=Y]

r1=raster()

las3=readLAS("G:\\data\\2015_naip_phodar\\319_169.las")


identical(dim(DT), dim(DF))    # TRUE
identical(DF$a, DT$a)          # TRUE
is.list(DF)                    # TRUE
is.list(DT)                    # TRUE

is.data.frame(DT)              # TRUE

tables()

# basic row subset operations
DT[2]                          # 2nd row
DT[3:2]                        # 3rd and 2nd row
DT[order(x)]                   # no need for order(DT$x)
DT[order(x), ]                 # same as above. The ',' is optional
DT[y>2]                        # all rows where DT$y > 2
DT[y>2 & v>5]                  # compound logical expressions
DT[!2:4]                       # all rows other than 2:4
DT[-(2:4)]                     # same

# select|compute columns data.table way
DT[, v]                        # v column (as vector)
DT[, list(v)]                  # v column (as data.table)
DT[, .(v)]                     # same as above, .() is a shorthand alias to list()
DT[, sum(v)]                   # sum of column v, returned as vector
DT[, .(sum(v))]                # same, but return data.table (column autonamed V1)
DT[, .(sv=sum(v))]             # same, but column named "sv"
DT[, .(v, v*2)]                # return two column data.table, v and v*2

# subset rows and select|compute data.table way
DT[2:3, sum(v)]                # sum(v) over rows 2 and 3, return vector
DT[2:3, .(sum(v))]             # same, but return data.table with column V1
DT[2:3, .(sv=sum(v))]          # same, but return data.table with column sv
DT[2:5, cat(v, "\n")]          # just for j's side effect

# select columns the data.frame way
DT[, 2, with=FALSE]            # 2nd column, returns a data.table always
colNum = 2
DT[, colNum, with=FALSE]       # same, equivalent to DT[, .SD, .SDcols=colNum]
DT[["v"]]                      # same as DT[, v] but much faster

# grouping operations - j and by
DT[, sum(v), by=x]             # ad hoc by, order of groups preserved in result
DT[, sum(v), keyby=x]          # same, but order the result on by cols
DT[, sum(v), by=x][order(x)]   # same but by chaining expressions together

# fast ad hoc row subsets (subsets as joins)
DT["a", on="x"]                # same as x == "a" but uses binary search (fast)
DT["a", on=.(x)]               # same, for convenience, no need to quote every column
DT[.("a"), on="x"]             # same
DT[x=="a"]                     # same, single "==" internally optimised to use binary search (fast)
DT[x!="b" | y!=3]              # not yet optimized, currently vector scan subset
DT[.("b", 3), on=c("x", "y")]  # join on columns x,y of DT; uses binary search (fast)
DT[.("b", 3), on=.(x, y)]      # same, but using on=.()
DT[.("b", 1:2), on=c("x", "y")]             # no match returns NA
DT[.("b", 1:2), on=.(x, y), nomatch=0]      # no match row is not returned
DT[.("b", 1:2), on=c("x", "y"), roll=Inf]   # locf, nomatch row gets rolled by previous row
DT[.("b", 1:2), on=.(x, y), roll=-Inf]      # nocb, nomatch row gets rolled by next row
DT["b", sum(v*y), on="x"]                   # on rows where DT$x=="b", calculate sum(v*y)

# all together now
DT[x!="a", sum(v), by=x]                    # get sum(v) by "x" for each i != "a"
DT[!"a", sum(v), by=.EACHI, on="x"]         # same, but using subsets-as-joins
DT[c("b","c"), sum(v), by=.EACHI, on="x"]   # same
DT[c("b","c"), sum(v), by=.EACHI, on=.(x)]  # same, using on=.()

# joins as subsets
X = data.table(x=c("c","b"), v=8:7, foo=c(4,2))
X

DT[X, on="x"]                         # right join
X[DT, on="x"]                         # left join
DT[X, on="x", nomatch=0]              # inner join
DT[!X, on="x"]                        # not join
DT[X, on=.(y<=foo)]                   # NEW non-equi join (v1.9.8+)
DT[X, on="y<=foo"]                    # same as above
DT[X, on=c("y<=foo")]                 # same as above
DT[X, on=.(y>=foo)]                   # NEW non-equi join (v1.9.8+)
DT[X, on=.(x, y<=foo)]                # NEW non-equi join (v1.9.8+)
DT[X, .(x,y,x.y,v), on=.(x, y>=foo)]  # Select x's join columns as well

DT[X, on="x", mult="first"]           # first row of each group
DT[X, on="x", mult="last"]            # last row of each group
DT[X, sum(v), by=.EACHI, on="x"]      # join and eval j for each row in i
DT[X, sum(v)*foo, by=.EACHI, on="x"]  # join inherited scope
DT[X, sum(v)*i.v, by=.EACHI, on="x"]  # 'i,v' refers to X's v column
DT[X, on=.(x, v>=v), sum(y)*foo, by=.EACHI] # NEW non-equi join with by=.EACHI (v1.9.8+)

# setting keys
kDT = copy(DT)                        # (deep) copy DT to kDT to work with it.
setkey(kDT,x)                         # set a 1-column key. No quotes, for convenience.
setkeyv(kDT,"x")                      # same (v in setkeyv stands for vector)
v="x"
setkeyv(kDT,v)                        # same
# key(kDT)<-"x"                       # copies whole table, please use set* functions instead
haskey(kDT)                           # TRUE
key(kDT)                              # "x"

# fast *keyed* subsets
kDT["a"]                              # subset-as-join on *key* column 'x'
kDT["a", on="x"]                      # same, being explicit using 'on=' (preferred)

# all together
kDT[!"a", sum(v), by=.EACHI]          # get sum(v) for each i != "a"

# multi-column key
setkey(kDT,x,y)                       # 2-column key
setkeyv(kDT,c("x","y"))               # same

# fast *keyed* subsets on multi-column key
kDT["a"]                              # join to 1st column of key
kDT["a", on="x"]                      # on= is optional, but is preferred
kDT[.("a")]                           # same, .() is an alias for list()
kDT[list("a")]                        # same
kDT[.("a", 3)]                        # join to 2 columns
kDT[.("a", 3:6)]                      # join 4 rows (2 missing)
kDT[.("a", 3:6), nomatch=0]           # remove missing
kDT[.("a", 3:6), roll=TRUE]           # locf rolling join
kDT[.("a", 3:6), roll=Inf]            # same as above
kDT[.("a", 3:6), roll=-Inf]           # nocb rolling join
kDT[!.("a")]                          # not join
kDT[!"a"]                             # same

# more on special symbols, see also ?"special-symbols"
DT[.N]                                # last row
DT[, .N]                              # total number of rows in DT
DT[, .N, by=x]                        # number of rows in each group
DT[, .SD, .SDcols=x:y]                # select columns 'x' and 'y'
DT[, .SD[1]]                          # first row of all columns
DT[, .SD[1], by=x]                    # first row of 'y' and 'v' for each group in 'x'
DT[, c(.N, lapply(.SD, sum)), by=x]   # get rows *and* sum columns 'v' and 'y' by group
DT[, .I[1], by=x]                     # row number in DT corresponding to each group
DT[, grp := .GRP, by=x]               # add a group counter column
X[, DT[.BY, y, on="x"], by=x]         # join within each group

# add/update/delete by reference (see ?assign)
print(DT[, z:=42L])                   # add new column by reference
print(DT[, z:=NULL])                  # remove column by reference
print(DT["a", v:=42L, on="x"])        # subassign to existing v column by reference
print(DT["b", v2:=84L, on="x"])       # subassign to new column by reference (NA padded)

DT[, m:=mean(v), by=x][]              # add new column by reference by group
                                      # NB: postfix [] is shortcut to print()
# advanced usage
DT = data.table(x=rep(c("b","a","c"),each=3), v=c(1,1,1,2,2,1,1,2,2), y=c(1,3,6), a=1:9, b=9:1)

DT[, sum(v), by=.(y%%2)]              # expressions in by
DT[, sum(v), by=.(bool = y%%2)]       # same, using a named list to change by column name
DT[, .SD[2], by=x]                    # get 2nd row of each group
DT[, tail(.SD,2), by=x]               # last 2 rows of each group
DT[, lapply(.SD, sum), by=x]          # sum of all (other) columns for each group
DT[, .SD[which.min(v)], by=x]         # nested query by group

DT[, list(MySum=sum(v),
          MyMin=min(v),
          MyMax=max(v)),
    by=.(x, y%%2)]                    # by 2 expressions

DT[, .(a = .(a), b = .(b)), by=x]     # list columns
DT[, .(seq = min(a):max(b)), by=x]    # j is not limited to just aggregations
DT[, sum(v), by=x][V1<20]             # compound query
DT[, sum(v), by=x][order(-V1)]        # ordering results
DT[, c(.N, lapply(.SD,sum)), by=x]    # get number of observations and sum per group
DT[, {tmp <- mean(y);
      .(a = a-tmp, b = b-tmp)
      }, by=x]                        # anonymous lambdain 'j', j accepts any valid
                                      # expression. TO REMEMBER: every element of
                                      # the list becomes a column in result.
pdf("new.pdf")
DT[, plot(a,b), by=x]                 # can also plot in 'j'
dev.off()

# using rleid, get max(y) and min of all cols in .SDcols for each consecutive run of 'v'
DT[, c(.(y=max(y)), lapply(.SD, min)), by=rleid(v), .SDcols=v:b]

# Follow r-help posting guide, SUPPORT is here (*not* r-help) :
# http://stackoverflow.com/questions/tagged/data.table
# or
# datatable-help@lists.r-forge.r-project.org

## Not run:
vignette("datatable-intro")
vignette("datatable-reference-semantics")
vignette("datatable-keys-fast-subset")
vignette("datatable-secondary-indices-and-auto-indexing")
vignette("datatable-reshape")
vignette("datatable-faq")


test.data.table()          # over 5700 low level tests

# keep up to date with latest stable version on CRAN
update.packages()
# get the latest devel (needs Rtools for windows, xcode for mac)
install.packages("data.table", repos = "https://Rdatatable.github.io/data.table", type = "source")


## End(Not run)
}
