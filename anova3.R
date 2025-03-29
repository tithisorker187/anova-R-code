# Create the data frame for the Latin square design
install.packages("agricolae")
library(agricolae)

install.packages("dplyr")
library(dplyr)



data <- data.frame(
  Row =factor( rep(1:6, each = 6)), # Rows
  Column =factor( rep(1:6, times = 6)), # Columns
  Treatment = c(
    "F", "E", "D", "C", "B", "A",
    "E", "C", "A", "D", "F", "B",
    "B", "A", "F", "E", "D", "C",
    "A", "B", "E", "F", "C", "D",
    "D", "F", "C", "B", "A", "E",
    "C", "D", "B", "A", "E", "F"
  ),
  Yield = c(
    219, 250, 227, 162, 182, 89,
    224, 141, 91, 191, 213, 195,
    204, 94, 225, 229, 250, 207,
    77, 204, 240, 199, 182, 250,
    250, 231, 209, 204, 92, 227,
    152, 186, 191, 77, 230, 198
  )
)
data


# Perform ANOVA
anova_results=aov(Yield ~ Row + Column + Treatment, data = data)
anova_results
summary(anova_results)

##(ii)

#dedine parameters.
n=6  #number of rows,columns,and treatments(since it is 6*6 latin square model
n
#calculate error df for CRD    #total observation=n^2
df_crd=n^2-n   #(total obsevartion-treatment)

df_crd
#calculate error df for RBD 
df_rbd=n^2-(n+n-1)    #total observation-(treatment+row-1)
df_rbd
#calculate error df for LSD

df_lsd=n^2-(n+n+n-2) #total obs-(treatment+row+column-2)
df_lsd

efficiency_lsd_crd=df_crd/df_lsd
efficiency_lsd_crd

efficiency_lsd_rbd=df_rbd/df_lsd
efficiency_lsd_rbd

###(III)


lsd_test=LSD.test(anova_results,"Treatment",p.adj="none")

lsd_test


###(IV)

t_test_AC=t.test(Yield~Treatment,data=subset(data,Treatment %in% c("A","C")))
t_test_AC
t_test_BF
t_test_BF=t.test(Yield~Treatment,data=subset(data,Treatment %in% c("B","F")))
t_test_BF


###(V)
latin_square=matrix(c(219,250,227,162,182,89,224,141,91,191,213,195,204,141,91,191,213,195,204,94,225,229,250,207,77,204,NA,199,182,250,250,231,209,205,92,227,152,186,191,77,230,198),nrow=6,byrow=TRUE)
latin_square
row_means=rowMeans(latin_square,na.rm=TRUE)
row_means

col_means=colMeans(latin_square,na.rm=TRUE)
col_means

grand_mean=mean(latin_square,na.rm=TRUE)
grand_mean

missing_row=4
missing_row
missing_col=4
missing_col

#calculated the missing value 

estimated_value=grand_mean+(row_means[missing_row]-grand_mean)+(col_means[missing_row]-grand_mean)
estimated_value


















































