run_analisys <- function () {
        
        mergedir <- function(dir1, dir2) {
                L1      <- lapply(dir1, read.table)
                L2      <- lapply(dir2, read.table)
                return(mapply(rbind, L1, L2, SIMPLIFY = FALSE))
        }
        ##
        ## Merginng Process
        ##
        
        ##   LIST "L_merged"
        ## ELEMENT      VALUE
        ## [[1]]        X
        ## [[2]]        y
        ## [[3]]        subject
        ## [[4]]        body acc x
        ## [[5]]        body acc y
        ## [[6]]        body acc z
        ## [[7]]        body gyro x
        ## [[8]]        body gyro y
        ## [[9]]        body gyro z
        ## [[10]]       total acc x
        ## [[11]]       total acc y
        ## [[12]]       total acc z
        
        V_train         <- c("train/X_train.txt",
                             "train/y_train.txt",
                             "train/subject_train.txt",         
                             dir("train/Inertial Signals", full.names=TRUE))
        V_test          <- c("test/X_test.txt",
                             "test/y_test.txt",
                             "test/subject_test.txt",
                             dir("test/Inertial Signals", full.names=TRUE))

        L_merged                <- mergedir(V_train, V_test)

        ##
        ## Columns Naming Process
        ## 
        
        DF_X                    <- L_merged[[1]]
        names(DF_X)             <- read.table("features.txt")[,2]  
        
        DF_y                    <- L_merged[[2]]        
        names(DF_y)             <- "y"
        DF_activity             <- read.table("activity_labels.txt")
        names(DF_activity)      <- c("y", "Activity")
        DF_y                    <- merge(DF_y, DF_activity)
        
        DF_subject              <- L_merged[[3]]     
        names(DF_subject)       <- "Subject"

        ##
        ## Crafting Integrated Data Frame (X + y + Subject) 
        ## 
        
        L_out                   <- list()
        
        L_out[[1]]              <- cbind(DF_X, DF_y)
        L_out[[1]]              <- cbind(L_out[[1]], DF_subject)
        
        rm(DF_X, DF_y, DF_subject, DF_activity)
        
        ##
        ## Selection Columns for Resume Average Table Process
        ## 
        
        v_col                   <- c(1:6,   41:46, 81:86, 121:126, 161:   166, 
                                     201,   202,   214,   215,     227, 
                                     228,   240,   241,   253,     254, 
                                     266:   271,   345:   350,     424:   429, 
                                     503,   504,   516,   517,     529,   530,
                                     542,   543)
        
        L_out[[2]]              <- data.frame(lapply(L_out[[1]][ , v_col], mean))
        
        ##
        ## Crafting Integrated Data Frame (X + y + Subject) 
        ## 
        
        L_out[[3]]              <- L_merged[[4]]
        L_out[[4]]              <- L_merged[[5]]
        L_out[[5]]              <- L_merged[[6]]
        L_out[[6]]              <- L_merged[[7]]
        L_out[[7]]              <- L_merged[[8]]
        L_out[[8]]              <- L_merged[[9]]
        L_out[[9]]              <- L_merged[[10]]
        L_out[[10]]             <- L_merged[[11]]
        L_out[[11]]             <- L_merged[[12]]
        
        rm(L_merged)
        
        ##
        ## Writing merged files (directory = "merge") 
        ## 
        
        dir.create("merge")
        dir.create("merge/Inertial Signals")
        
        V_onames <- c("merge/xys.txt",   "merge/average.txt",  "merge/Inertial Signals/body_acc_x.txt",
                     "merge/Inertial Signals/body_acc_y.txt",  "merge/Inertial Signals/body_acc_z.txt",    
                     "merge/Inertial Signals/body_gyro_x.txt", "merge/Inertial Signals/body_gyro_y.txt", 
                     "merge/Inertial Signals/body_gyro_z.txt", "merge/Inertial Signals/total_acc_x.txt",
                     "merge/Inertial Signals/total_acc_y.txt", "merge/Inertial Signals/total_acc_z.txt")
        
        mapply(write.csv, L_out, V_onames, row.names=FALSE)
        
        return(L_out[[2]])    
}