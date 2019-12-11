#### Run Statistcs

# --- View Raw Data
head(ori)
nrow(ori)
ncol(ori)
# names(ori)

# --- Inspect the data. 
# summary(ori)
str(ori)
dim(ori)
# glimpse(ori)
# class(ori)
sapply(ori, class)

# --- FIND COLUMNS CONTAINING STRING
# grep("WAP", names(df), value=T)

# --- See Column range with specific String value
range(grep("WAP", names(ori), value=T))

# --- Assign original file to new dataframe
df<-ori