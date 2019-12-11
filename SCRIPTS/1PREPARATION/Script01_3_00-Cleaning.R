#############################################################################
# PLAN OF ATTACK:
# ----------------------------------------------------------------------
# Check Column names
# Trim whitespaces
# Change 100 values to -110
# Check for Duplicate Vars
# Check Zero and Near Zero Variance
# Check Data Types
# Check for Missing Values
#############################################################################

#### HEADINGS -----------------------------------------------------------
# --- Check Empty Colnames
which(colnames(df)=="")
# --- Check Col index of specific column by name
# which(colnames(df)==".. MyName ..")

# --- Assign colname
# colnames(df)[3] <- ".. MyName .."

#############################################################################
# REASSIGNING FAILED ACCESSES WAP 100
# --- Change value of WAPS= 100 to -110
WAPS<-grep("WAP", names(df), value=T)
df[,WAPS] <- sapply(df[,WAPS],function(x) ifelse(x==100,-110,x))

rm(WAPS)

#############################################################################
# DUPLICATION
# --- Count Duplicate records
duplex_row<-sum(duplicated(df))

# --- Remove Duplicated Records (637)
df <-if (duplex_row > 0) {
  df<-distinct(df)
} else {
  df<-df
}

rm(duplex_row)

# --- Remove Duplicated Columns
library(digest)
# Identifies which columns are duplicates
duplex_col <- duplicated(sapply(df, digest))
# Sets the sum
sum_duplex_col<-sum(duplex_col) # 54

# --- Eliminate duplicated columns if sum is greater than 0
df <-if (sum_duplex_col > 0) {
  df<-df[,which(!duplex_col)]
} else {
  df<-df
}

rm(duplex_col, sum_duplex_col)

#############################################################################
#### ZERO VARIANCE -------------------------------------
# --- Set near-zero variance predictors and register Variable "feature_variance"
feature_variance<-caret::nearZeroVar(df, saveMetrics = TRUE)
dim(feature_variance)
# head(feature_variance)

# --- View Zero Variance
# which(feature_variance$zeroVar == 'TRUE')

# --- View NearZeroVariance
# which(feature_variance$nzv == 'TRUE')

df_lwc<-which(colnames(df)=="WAP519")
feature_variance[feature_variance$zeroVar,][1:df_lwc,]
feature_variance[feature_variance$nzv,][1:df_lwc,]

# Filter out the rows that have zero variance
which(feature_variance$zeroVar==TRUE) # Row 3 has zeroVar
# Get the names of the Columns that have the row identified above with zero variance
zv_names<-row.names((feature_variance[17,])) # WAP018
# Make a list with the names I am keeping
var.out<-setdiff(names(df),zv_names)

# Filter it out from the main df
df<-df[var.out]

# Remove unnecessary vars
rm(df_lwc,feature_variance,zv_names,var.out)
#############################################################################
# MISSING DATA
na_test<-anyNA(df)

if (na_test == TRUE) {
  md.pattern(df, plot = TRUE, rotate.names = FALSE)
} else {
  df<-df
}

rm(na_test)
#############################################################################
# DROP UNUSED COLUMNS
# Instead of deleting transfer it to a new df
ts_col<-which(colnames(df)=="TIMESTAMP")
ts_lst<-df$TIMESTAMP

# Set column to NULL
df$TIMESTAMP<-NULL

# Drop from the original df
# df<-df %>% select(-ts_col)
rm(ts_col)

#############################################################################
# RENAME COLUMNS
df <- df %>% rename(POSITIONID=RELATIVEPOSITION,
                    FLOORID=FLOOR)

#############################################################################
# Create MAX value column
df$MAX<-apply(df[1:465],1,function(x) {max(x)})

# Drop any unnecessary field (i.e WAPS containing only -110 values)
df<-df %>% 
  filter(MAX>-95)

# Change any 0 value to -1 for division calculus
df$MAX<-sapply(df[,MAX],function(x) ifelse(x==0,-1,x))
