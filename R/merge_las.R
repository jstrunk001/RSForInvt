merge_las=function(list_las, keep_1st_header=T){

      require(lidR)
      points_merge = do.call(rbind,lapply(list_las,function(x)x@data))
      if(keep_1st_header) return(LAS(points_merge,list_las[[1]]@header))
      if(!keep_1st_header) return(LAS(points_merge))

  }
