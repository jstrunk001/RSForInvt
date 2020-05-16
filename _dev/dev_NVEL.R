
#volume computation
calcTreeVolume <- function(e,r,f,d,s,dbh,h){
  q <- .Fortran(
    "vollib_r",
    as.character(e), #volume equation number
    as.integer(r),   #USFS Region -- 8: Southern Region
    as.character(f), #USFS Forest -- 12: Francis Marion-Sumnter National Forest
    as.character(d), #USFS Forest District -- 03:Long Cane District; 00:Default; 01:Enoree District; 02:Andrew Pickens District; 05:Francis Marion District
    as.integer(s),   #Species Code ie 122
    as.double(dbh),  #dbh outside bark
    as.double(h),  #height top
    as.double(0),  #merch top diameter
    as.double(0),  #
    as.double(0),  #height for saw timber
    as.double(0),  #pulp height (height to 4" top)
    as.double(0),  #
    as.double(0),  #
    as.double(0),  #
    as.integer(0), #
    as.double(0),  #
    as.double(0),  #
    as.double(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)),  #vector of volumes; filled in with values that can be calculated
    as.integer(0)  #error flag; >0 an error returned
  )
  return( q[[18]] ) #volume vector of 15 tree volumes; want [1], total cubic volume
  # 1)	Total Cubic Volume from ground to tip
  # 2)	Gross Scribner board foot volume. Note: The VOL(2) is International ¼ board foot volume for Region 8 Forest 8, 9, 10, and 12 (except Andrew Pickens district); and Region 9 Forest 4,5,8,11,12,14,19,20,21,22,24, and 30 when using Clark profile equation.
  # 3)	Net Scribner board foot volume.
  # 4)	Gross merchantable cubic foot volume
  # 5)	Net merchantable cubic foot volume
  # 6)	Merchantable cordwood volume
  # 7)	Gross secondary product volume in cubic feet
  # 8)	Net secondary product volume in cubic feet
  # 9)	Secondary product in cordwood
  # 10)	Gross International ¼ board foot volume
  # 11)	Net International ¼ board foot volume
  # 12)	Gross secondary product in Scribner board feet
  # 13)	Net secondary product in Scribner board feet
  # 14)	Stump volume
  # 15)	Tip volume
}
