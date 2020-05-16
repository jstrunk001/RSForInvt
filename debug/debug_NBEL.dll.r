

test1 = dyn.load("D:\\Box\\sync\\R\\r_packages_dev\\my_packages\\RSForInvt\\code\\NBEL_CPP_DAL\\Release\\NBEL_CPP_DAL.dll")

test = dyn.load("D:\\Box\\sync\\R\\r_packages_dev\\my_packages\\RSForInvt\\code\\NBEL_CPP_DAL\\nbel_cpp_tester\\Release\\NBEL.dll")


GetSppCode("balsam fir")

.C("GetSppCode", "balsam fir" , PACKAGE = "NBEL")
.C(".GetSppCode", "balsam fir" , PACKAGE = "NBEL")
.C(".GetSppCode", "balsam fir" )
.C("GetSppCode", "balsam fir" )
.Call("GetSppCode", "balsam fir" , PACKAGE = "NBEL")
.Call(".GetSppCode", "balsam fir" , PACKAGE = "NBEL")
.Call(".GetSppCode", "balsam fir" )
.Call("GetSppCode", "balsam fir" )
.External("GetSppCode", "balsam fir" , PACKAGE = "NBEL")
.External(".GetSppCode", "balsam fir" , PACKAGE = "NBEL")
.External("GetSppCode", "balsam fir")
.External(".GetSppCode", "balsam fir")


.Call("GetLocation", 1 , PACKAGE = "NBEL")
.Call("GetLocation", as.double(1) , PACKAGE = "NBEL")
.Call(".GetLocation", 1 , PACKAGE = "NBEL")
.Call(".GetLocation", as.double(1) )
.Call("GetLocation", as.double(1) )
