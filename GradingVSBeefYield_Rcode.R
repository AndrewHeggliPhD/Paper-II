#############################################################################
##                                                                         ##
## R-script reproducing results presented in manuscript:                   ##
##      "Carcase grading reflects the variation in beef yield              ##
##      - A multivariate method for exploring the relationship             ##
##      between beef yield and carcase trait".                             ##
##                                                                         ##
## Authors: NN                                                             ##
##                                                                         ##
## Adresses: NN                                                            ##
##                                                                         ##
## Manuscript submitted to Animal February 2023                            ##
##                                                                         ##        
#############################################################################

##1) Clean workspace, load necessary packages --------------------------- ##

#  Clean workspace              
rm(list = ls())

# Load packages
packages <- c('plyr','dplyr','xtable','plot3D','corrplot')

for(pp in packages){require(pp,character.only = TRUE,quietly = TRUE)}

## 2) Define which components that constitutes the different tissue ------ ##
##    categories in the two cutting patterns. -----------------------------##
TissueCategories <- list(
          Detailed = list(
            Fronthalf = 
              list(HVC = c('X208.201YT1','X208.201YT2','X208.201YT3',
                           'X214.201BPL'),
                   meat5 = c('X206.202K5','X208.202K5','X212.202K5',
                             'X214.202K5','X215.202K5','X216.202K5',
                             'X217.202K5','X218.202K5','X206.202RUT',
                             'X212.202RUT','X214.202RUT','X215.202RUT',
                             'X216.202RUT','X212.201FIF','X213.201FIF',
                             'X216.201HFI','X212.201BOB'),
                   meat14 = c('X206.202K14','X208.202K14','X211.202K14',
                              'X212.202K14','X213.202K14','X214.202K14',
                              'X215.202K14','X216.202K14','X217.202K14',
                              'X218.202K14'),
                   meat21 = c('X206.202K21','X208.202K21','X211.202K21',
                              'X212.202K21','X213.202K21','X214.202K21',
                              'X215.202K21','X216.202K21','X217.202K21',
                              'X218.202K21'),
                   fat = c('X206.202KJF','X208.202KJF','X211.202KJF',
                           'X212.202KJF','X213.202KJF','X214.202KJF',
                           'X215.202KJF','X216.202KJF','X217.202KJF',
                           'X218.202KJF'),
                   bonewaste = c('X206.203AVF','X208.203AVF','X211.203AVF',
                                 'X212.203AVF','X213.203AVF','X214.203AVF',
                                 'X215.203AVF','X216.203AVF','X217.203AVF',
                                 'X218.203AVF','X206.203SEN','X208.203SEN',
                                 'X211.203SEN','X212.203SEN','X213.203SEN',
                                 'X214.203SEN','X215.203SEN','X216.203SEN',
                                 'X217.203SEN','X206.203AVB','X211.203AVB',
                                 'X212.203AVB','X215.203AVB','X216.203AVB',
                                 'X217.203AVB')),
              Backhalf = 
                list(HVC = c('X220.201FLB','X220.201MBR','X221.201LTU',
                             'X222.201RUB','X222.201RUD','X223.201YT1',
                              'X223.201YT2','X223.201YT3','X225.201IN1',
                              'X225.201IN2','X225.201IN3','X221.203BSK'),
                     meat5 = c('X220.202K5','X221.202K5','X222.202K5',
                               'X223.202K5','X224.202K5','X220.202RUT',
                               'X221.202RUT','X224.201RUS','X223.201DRA'),
                     meat14 = c('X220.202K14','X221.202K14','X222.202K14',
                                'X223.202K14','X224.202K14','X225.202K14'),
                     meat21 = c('X220.202K21','X221.202K21','X222.202K21',
                                'X223.202K21','X224.202K21','X225.202K21'),
                     fat = c('X220.202KJF','X221.202KJF','X223.202KJF',
                             'X224.202KJF','X225.202KJF'),
                     bonewaste = c('X220.203AVF','X221.203AVF','X222.203AVF',
                                   'X223.203AVF','X224.203AVF','X225.203AVF',
                                   'X220.203SEN','X221.203SEN','X222.203SEN',
                                   'X223.203SEN','X224.203SEN','X225.203SEN',
                                   'X220.203AVB','X223.203AVB','X224.203AVB'))),
          Undetailed = list(
                Fronthalf = 
                  list(HVC = c('X252.201BPL','X253.201YT1','X253.201YT2',
                               'X253.201YT3'),
                       meat5 = c('X251.202K5','X252.202K5','X253.202K5',
                                 'X251.202RUT','X252.202RUT'),
                       meat14 = c('X251.202K14','X252.202K14','X253.202K14'),
                       meat21 = c('X251.202K21','X252.202K21','X253.202K21'),
                       fat = c('X251.202KJF','X252.202KJF','X253.202KJF'),
                       bonewaste = c('X251.203AVF','X252.203AVF','X253.203AVF',
                                     'X251.203AVB','X251.203SEN','X252.203SEN',
                                     'X253.203SEN','X251.203SRM')),
                Backhalf = 
                  list(HVC = c('X262.201FLB','X262.201IN1','X262.201IN2',
                               'X262.201IN3','X262.201MBR','X262.201RUN',
                               'X262.201YT1','X262.201YT2','X262.201YT3',
                               'X263.201LTU','X263.203BSK'),
                    meat5 = c('X262.202K5','X263.202K5','X262.201RUS',
                              'X263.202RUT'),
                    meat14 = c('X262.202K14','X263.202K14'),
                    meat21 = c('X262.202K21','X263.202K21'),
                    fat = c('X262.202KJF','X263.202KJF'),
                    bonewaste = c('X262.203AVF','X263.203AVF','X262.203AVB',
                                  'X262.203SEN','X263.203SEN','X262.203SRM'))))


## 3) Data cleaning ------------------------------------------------------ ##
# plim is the percentage limit for excluding individuals during data 
# cleaning. If the difference between the total weight of the carcase, 
# and the sum of all the smallest component weights is more than the 
# absolute value of plim, the individual will be excluded
plim <- 0.02 

# Tissue weights that are registered with 0 weight (fat for some 
# individuals) are replaced with min_weight to ensure no production of NaNs
# n log-odds
min_weight <- 0.1


# Load raw data
setwd(choose.dir('','Choose folder containing file 
                 "Beef_Yield_RawData.csv"'))
Yield_data <- read.csv2('Beef_Yield_RawData.csv')

# Number of original half carcasses, number of whole carcases is 
# reported in manuscript
nn_org <- dim(Yield_data)[1]
print(nn_org/2)

# Get breed data from animal registry into a matrix within the dataframe
Yield_data$Breed16 <- I(as.matrix(Yield_data[,
                                    grep('Rase16F',names(Yield_data))]))
Yield_data <- Yield_data[,-grep('Rase16F',names(Yield_data))]


# Set negative weights to zero
Yield_data[,is.element(names(Yield_data),unlist(TissueCategories))] <- (
  pmax(Yield_data[,is.element(names(Yield_data),unlist(TissueCategories))],0))

# Data cleaning - removing individuals with more than plim absolute difference 
# between sum of tissue component weights.

percdiff <- cbind((Yield_data$X205.205VEF - 
                      (rowSums(Yield_data[,is.element(names(Yield_data),
                        unlist(TissueCategories$Detailed$Fronthalf))])))/
                        Yield_data$X205.205VEF, 
                  (Yield_data$X201.205VEB - 
                      (rowSums(Yield_data[,is.element(names(Yield_data),
                        unlist(TissueCategories$Detailed$Backhalf))])))/
                   Yield_data$X201.205VEB, 
                  (Yield_data$X250.205VEF - 
                      (rowSums(Yield_data[,is.element(names(Yield_data),
                      unlist(TissueCategories$Undetailed$Fronthalf))])))/
                   Yield_data$X250.205VEF,
                  (Yield_data$X260.205VEB - 
                      (rowSums(Yield_data[,is.element(names(Yield_data),
                        unlist(TissueCategories$Undetailed$Backhalf))])))/
                Yield_data$X260.205VEB)

# ID for individuals with at least one quarter with more than plim weightdiff
IDremoveWeight <- unique(Yield_data$ID[apply(abs(percdiff), 1,max,na.rm = TRUE) > plim])

# ID for individuals with incomplete breed information
IDremoveBreed <- unique(Yield_data$ID[which(is.na(Yield_data$Rase)&
                                  rowSums(Yield_data$Breed16,na.rm=TRUE)!=1)])

# ID for individuals in the Calf, Steer and Bull categories

IDremoveCategory <- unique(Yield_data$ID[which(is.element(Yield_data$Kategori,
                                                          c(160,163,164)))])

# Remowe rows for individuals with incomplete weight or breed information, and categories with low sample numbers
Yield_data <- Yield_data[!is.element(Yield_data$ID,c(IDremoveWeight,IDremoveBreed,IDremoveCategory)),]

# Number of half carcasses for analysis, reported number of individuals
nn <- dim(Yield_data)[1] 
print(nn/2)


## 4) Merge half carcasses to whole carcasses ---------------------------- ##
# Get weights for different tissuecategories
Yield_data$WW_front <- (sapply(TissueCategories$Detailed$Fronthalf,
       function(x,y){rowSums(y[,x],na.rm=TRUE)},y=Yield_data)+
         sapply(TissueCategories$Undetailed$Fronthalf,
                function(x,y){rowSums(y[,x],na.rm=TRUE)},y=Yield_data))

Yield_data$WW_back <- (sapply(TissueCategories$Detailed$Backhalf,
              function(x,y){rowSums(y[,x],na.rm=TRUE)},y=Yield_data)+
                      sapply(TissueCategories$Undetailed$Backhalf,
              function(x,y){rowSums(y[,x],na.rm=TRUE)},y=Yield_data))

# Standarize with respect to fat and meat 5
StandardList <- list(Front = 
                       list(
                         Meat14 = 
                           list(QR_names = c('X209.204QM1','X209.204S14'),
                                Standard = 0.14,
                                FatPartFat = 0.72,
                                FatPartMeat = 0,
                                Trimname = 'meat14',
                                Meatname = 'meat5',
                                Fatname = 'fat',
                                WW_name = 'WW_front'),
                         Meat21 = 
                           list(QR_names = c('X209.204QM2','X209.204S21'),
                                Standard = 0.21,
                                FatPartFat = 0.72,
                                FatPartMeat = 0,
                                Trimname = 'meat21',
                                Meatname = 'meat5',
                                Fatname = 'fat',
                                WW_name = 'WW_front')),
                      Back = list(
                        Meat14 = 
                         list(QR_names = c('X204.204QM1','X204.204S14'),
                              Standard = 0.14,
                              FatPartFat = 0.72,
                              FatPartMeat = 0,
                              Trimname = 'meat14',
                              Meatname = 'meat5',
                              Fatname = 'fat',
                              WW_name = 'WW_back'),
                       Meat21 = 
                         list(QR_names = c('X204.204QM2','X204.204S21'),
                              Standard = 0.21,
                              FatPartFat = 0.72,
                              FatPartMeat = 0,
                              Trimname = 'meat21',
                              Meatname = 'meat5',
                              Fatname = 'fat',
                              WW_name = 'WW_back')))

Standard_func <- function(X,Y)
{
  Fat_per <- Y[,X$QR_names[1]]
  Fat_per[is.na(Fat_per)] <- 0
  Fat_per[Fat_per==0] <- Y[Fat_per==0,X$QR_names[2]]
  Fat_per[is.na(Fat_per)] <- 0
  Fat_per <- Fat_per/100
  Fat_per[Fat_per==0] <- X$Standard
  WW <- Y[,X$WW_name]
  idx_Meat_Add <- which(Fat_per>X$Standard)
  idx_Fat_Add <- which(Fat_per<X$Standard)
  res <- data.frame(Meat_add = rep(0,dim(Y)[1]),
                    Fat_add = rep(0,dim(Y)[1]))
  res$Meat_add[idx_Meat_Add] <- (WW[,X$Trimname]*(Fat_per-X$Standard)/
                                   (X$Standard-X$FatPartMeat))[idx_Meat_Add]
  res$Fat_add[idx_Fat_Add] <- (WW[,X$Trimname]*(Fat_per-X$Standard)/
                                   (X$Standard-X$FatPartFat))[idx_Fat_Add]
  
  return(res)
}

StandardWeights <- list(Front=lapply(StandardList$Front,Standard_func,Y=Yield_data),
                        Back = lapply(StandardList$Back,Standard_func,Y=Yield_data))

StandardAdjust <- list(Front = Yield_data$WW_front[,c('meat5','fat')]/
                         as.matrix(StandardWeights$Front$Meat14+StandardWeights$Front$Meat21),
                       Back = Yield_data$WW_back[,c('meat5','fat')]/
                         as.matrix(StandardWeights$Back$Meat14+StandardWeights$Back$Meat21))
StandardAdjust$Front[StandardAdjust$Front>1] <- 1
StandardAdjust$Back[StandardAdjust$Back>1] <- 1
StandardAdjust$Front[is.nan(StandardAdjust$Front)] <- 1
StandardAdjust$Back[is.nan(StandardAdjust$Back)] <- 1  

StandardWeights$Front$Meat14 <- StandardWeights$Front$Meat14*StandardAdjust$Front
StandardWeights$Front$Meat21 <- StandardWeights$Front$Meat21*StandardAdjust$Front
StandardWeights$Back$Meat14 <- StandardWeights$Back$Meat14*StandardAdjust$Back
StandardWeights$Back$Meat21 <- StandardWeights$Back$Meat21*StandardAdjust$Back


Yield_data$WW_front[,'meat14'] <- Yield_data$WW_front[,'meat14'] + rowSums(StandardWeights$Front$Meat14)
Yield_data$WW_front[,'meat21'] <- Yield_data$WW_front[,'meat21'] + rowSums(StandardWeights$Front$Meat21)
Yield_data$WW_front[,'meat5'] <- (Yield_data$WW_front[,'meat5'] - StandardWeights$Front$Meat14$Meat_add
                                 -StandardWeights$Front$Meat21$Meat_add)
Yield_data$WW_front[,'fat'] <- (Yield_data$WW_front[,'fat'] - StandardWeights$Front$Meat14$Fat_add
                               -StandardWeights$Front$Meat21$Fat_add)

Yield_data$WW_back[,'meat14'] <- Yield_data$WW_back[,'meat14'] + rowSums(StandardWeights$Back$Meat14)
Yield_data$WW_back[,'meat21'] <- Yield_data$WW_back[,'meat21'] + rowSums(StandardWeights$Back$Meat21)
Yield_data$WW_back[,'meat5'] <- (Yield_data$WW_back[,'meat5'] - StandardWeights$Back$Meat14$Meat_add
                                 -StandardWeights$Back$Meat21$Meat_add)
Yield_data$WW_back[,'fat'] <- (Yield_data$WW_back[,'fat'] - StandardWeights$Back$Meat14$Fat_add
                                 -StandardWeights$Back$Meat21$Fat_add)

Yield_data$Kategori <- as.factor(Yield_data$Kategori)

# Make new data frame for model input

Yield_input <- Yield_data%>%dplyr::select(ID,Klasse,FettGruppe,Slaktevekt,
                        Kategori,Rase)%>%
                        dplyr::rename(conformation=Klasse, fat_group = FettGruppe,
                                      weight = Slaktevekt,category=Kategori,
                                      breed_vec = Rase)%>%
                  dplyr::group_by(ID)%>%
                  dplyr::summarize(ID = mean(ID),
                                   conformation = mean(conformation),
                                   fat_group = mean(fat_group),
                                   weight = mean(weight),
                                   category = category[1],
                                   breed_vec = mean(breed_vec))%>%
                  as.data.frame()

Yield_input$WW_back <- I(t(sapply(lapply(split(Yield_data$WW_back,f=Yield_data$ID),
                                     matrix,nrow=2,ncol=6),colSums)))
Yield_input$WW_front <- I(t(sapply(lapply(split(Yield_data$WW_front,f=Yield_data$ID),
                                       matrix,nrow=2,ncol=6),colSums)))
Yield_input$breed <- I(t(sapply(lapply(split(Yield_data$Breed16,f=Yield_data$ID),
                                       matrix,nrow=2,ncol=dim(Yield_data$Breed16)[2]),colMeans)))
colnames(Yield_input$breed) <- colnames(Yield_data$Breed16)
  
# Get the raw weights for frontpart
Yield_input$WW <- Yield_input$WW_front + Yield_input$WW_back
Yield_input$WW <- cbind(Yield_input$WW,rowSums(Yield_input$WW_front, 
                        na.rm = TRUE))

# Adjust tissue categories with zero weights 
Yield_input$WW[Yield_input$WW<=min_weight] <- min_weight
Yield_input$WW[is.na(Yield_input$WW)] <- min_weight


# Calculate the percentage of different categories
Yield_input$PP <- Yield_input$WW/matrix(rowSums(Yield_input$WW[,1:6]),
                                        nn/2,7,byrow=FALSE)


colnames(Yield_input$WW) <- c(names(TissueCategories$Detailed$Fronthalf),
                              'frontpart')

colnames(Yield_input$PP) <- colnames(Yield_input$WW)

# Construct the response, i.e. the log-odds
Yield_input$YY <- log(Yield_input$PP/(1- Yield_input$PP))
colnames(Yield_input$YY) <- colnames(Yield_input$WW)


## 6) Create input for breed groups -------------------------------------- ##

# Define breed groups
BreedGroups <- list(NorwegianRed = '1',
                    Other = as.character(c(0,2:6,9,10,13,27,28,29,98,99)),
                    Hereford = '21',
                    Charolais = '22',
                    AberdeenAngus = '23',
                    LimousinAndBA = c('24','26'),
                    Simmentaler = '25')


# Sort breeds in matrix
Yield_input$BreedHR <- I(matrix(NA,dim(Yield_input)[1],
                                length(BreedGroups)))
colnames(Yield_input$BreedHR) <- names(BreedGroups)

# Put in results from NN
for(ii in 1:length(BreedGroups))
{
  ci <- which(is.element(colnames(Yield_input$breed),
                         paste('Rase16F.',BreedGroups[[ii]],sep=''))) 
  Yield_input$BreedHR[,ii] <- rowSums(as.matrix(Yield_input$breed[,ci]),
                                        na.rm=TRUE) 
}

Yield_input$BreedHR[is.na(Yield_input$BreedHR)] <- 0

# Individuals without registration in NN. 
# Based on breed from slaughterhouses
Idx_Unknown <- which(rowSums(Yield_input$BreedHR)!=1)
Yield_input$BreedHR[Idx_Unknown,] <- 0

for(ii in Idx_Unknown)
{
  ci <- which(sapply(lapply(BreedGroups,is.element,
            set = as.character(Yield_input$breed_vec[ii])),sum)==1)
  if(length(ci)==1)
  {
    Yield_input$BreedHR[ii,ci] <- 1
    }else{print(Yield_data$Rase[ii])}
}


# Set in the Breed group
Yield_input$Breed_group <- rep('NA',dim(Yield_input)[1])
for(ii in 1:length(BreedGroups))
{
  Yield_input$Breed_group[is.element(Yield_input$breed_vec,
                    BreedGroups[[ii]])] <- names(BreedGroups)[ii]
}

IdxNa <- which(Yield_input$Breed_group=='NA')
for(ii in IdxNa)
{
  if(max(Yield_input$BreedHR[ii,]) > 0)
  {
    cidx <- which(Yield_input$BreedHR[ii,]==max(Yield_input$BreedHR[ii,]))
    Yield_input$Breed_group[ii] <- names(BreedGroups)[cidx[1]] 
  }
}



## 7) Summary data by category ------------------------------------------- ##
Summary_data <- Yield_input %>% group_by(category)

Tab_catsum <- Summary_data %>% dplyr::summarise(n(),
             paste(round(mean(weight),2),'\\pm',round(sd(weight),2)),
            paste(round(mean(conformation),2),'\\pm',round(sd(conformation),2)))

Tab_catsum2 <- Summary_data %>% dplyr::summarise(paste(round(
           mean(fat_group),2),'\\pm',round(sd(fat_group),2)))

Tab_catsum <- bind_cols(Tab_catsum,Tab_catsum2[,-1])

colnames(Tab_catsum) <- c('Category','n','Carcase weight (kg)',
                          'Conformation', 'Fat group')
Tab_catsum[1] <- c("Young bull","Heifer","Young cow","Cow")


bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
Tab_catsum <- print.xtable(xtable(Tab_catsum,caption = 
                    'Summary statistics by cattle category for carcass weight 
                    and EUROP classification. n is the number of individuals 
                    per (cattle) category. Results for carcase weight, 
                    EUROP conformation and fat group are given as mean values 
                    $\\pm$ standard deviations.', 
                    align = c('l','l','r','r','r','r'),
                    label='Tab:SummaryCat'), 
                    caption.placement = 'top',include.rownames = FALSE,
                    comment=FALSE,sanitize.colnames.function=bold,
                    print.results = FALSE)

# Latex format of table 1
cat(Tab_catsum)


## 8) Summary data by breed group ---------------------------------------- ##
Summary_brdata <- Yield_input %>% group_by(Breed_group)

Tab_brsum <- Summary_brdata %>% dplyr::summarise(n(),
              paste(round(mean(weight),2),'\\pm',round(sd(weight),2)),
              paste(round(mean(conformation),2),'\\pm',round(sd(conformation),2)))

Tab_brsum2 <- Summary_brdata %>% dplyr::summarise(paste(round(mean(
               fat_group),2),'\\pm',round(sd(fat_group),2)))

Tab_brsum <- bind_cols(Tab_brsum,Tab_brsum2[,-1])

colnames(Tab_brsum) <- c('Breed group','n','Carcase weight (kg))',
                         'Conformation', 'Fat group')


Tab_brsum <- print.xtable(xtable(Tab_brsum,caption = 'Summary statistics by 
                        breed groups for carcass weight and EUROP 
                        classification. n is the number of individuals per 
                        (cattle) category. Results for carcase weight, 
                        EUROP conformation and fat group are given as 
                        mean values $\\pm$ standard deviations.', 
                        align = c('l','l','r','r','r','r'),
                        label='Tab:SummaryBreed'), caption.placement = 'top',
                        include.rownames = FALSE,comment=FALSE,
                        sanitize.colnames.function=bold,print.results = FALSE)

# Latex format of table 2
cat(Tab_brsum)


## 9) Apply Wilks test --------------------------------------------------- ##
# Model formula
Mod_form <- formula(YY~conformation*fat_group + I(conformation^2)+
                      I(fat_group^2)+weight+relevel(category,ref = "162"))

# Confirm Wilks lambda test for interaction and second order polinominals
maov_test <- summary(manova(Mod_form, data=Yield_input),test = "Wilks") 

# Approximate F-statistics and p-values are reported
print(maov_test)

# Confirm Wilks lambda test for ibreed group
maov_test_breed <- summary(manova(update(Mod_form,.~.+BreedHR-1), 
                                   data=Yield_input),test = "Wilks")

# Approximate F-statistics and p-values are reported
print(maov_test_breed$stats)


## 10) Fit the yield multivariate regression model ----------------------- ##
# Fit the yield multivariate regression model
Yield_Model <- lm(Mod_form,data=Yield_input)

# Get the parameterestimates in one list
ParameterEstimates <- list(Beta_hat = Yield_Model$coefficients,
                           Sigma_hat = (t(Yield_Model$residuals)%*%
                                          Yield_Model$residuals/
                                          Yield_Model$df.residual),
                           Corr_mat_hat = NULL)

ParameterEstimates$Corr_mat_hat <- (ParameterEstimates$Sigma_hat/
                      sqrt(as.matrix(diag(ParameterEstimates$Sigma_hat))%*%
                        t(as.matrix(diag(ParameterEstimates$Sigma_hat)))))

colnames(ParameterEstimates$Corr_mat_hat) <- gsub('bonewaste',
                'bone \\& waste',colnames(ParameterEstimates$Corr_mat_hat))
rownames(ParameterEstimates$Corr_mat_hat) <- colnames(
  ParameterEstimates$Corr_mat_hat)

# Make table with regression parameter estimates
Format_func <- function(CC)
{
  #TDf <- cbind()
  res <- paste(format(round(CC[,"Estimate"],4),nsmall = 4),' (',
               format(round(CC[,"t value"],2),nsmall = 2),
               ')',ifelse(CC[,"Pr(>|t|)"]<0.05,'*','space'),sep ='')
}


FullBetaEstimates <- sapply(coef(summary(Yield_Model)),Format_func)

Sigma_hat <- format(round(diag(ParameterEstimates$Sigma_hat),4))
RSquares <- format(round(sapply(summary(Yield_Model),
                                function(x) x$r.squared),3))
FullBetaEstimates <- rbind(FullBetaEstimates,Sigma_hat,RSquares)


colnames(FullBetaEstimates) <- c("HVC","Meat5","Meat14","Meat21",
                                 "Fat","Bone \\& waste","Forepart")
rownames(FullBetaEstimates) <- c("Intercept","Conformation",
                                 "Fat group","Conformation\\textsuperscript{2}",
                                 "Fat group\\textsuperscript{2}","Weight",
                                 "Heifer","Young cow","Cow",
                                 "C:F","$\\sigma\\textsuperscript{2}$",
                                 "R\\textsuperscript{2}")

# Hardcoding of row order
FullBetaEstimates <- FullBetaEstimates[c(1:3,dim(FullBetaEstimates)[1]-2,
                                         4:(dim(FullBetaEstimates)[1]-3),
                                         dim(FullBetaEstimates)[1]-c(1,0)),]


FullBetaEstimates <- print.xtable(xtable(FullBetaEstimates,
                    caption = 'Regression parameter estimates from a linear 
                    regression with a multivariate yield with t-values in brackets. 
                    Estimates with p-values <0.05 (t-test) are marked 
                    with an asterix (*). Regression parameter estimates are 
                    estimated based on log-odds of product categories.  
                    Columns are product categories. Rows are terms in the model. 
                    C:F is   the interaction between conformation and fat. 
                    $\\sigma^2$ is the estimated residual variance per product 
                    category. $R^2$ is the coefficient of determination when 
                    breed group is omitted from the model.', 
                    align = c('l','r','r','r','r','r','r','r'), 
                    digits=c(0,3,3,3,3,3,3,3),label = 'Tab:Parameters'), 
                    caption.placement = 'top', comment=FALSE,print.results = FALSE,
                    scalebox = '0.7')

FullBetaEstimates <- gsub('\\( ','\\(',FullBetaEstimates)
FullBetaEstimates <- gsub('\\( ','\\(',FullBetaEstimates)
FullBetaEstimates <- gsub('space','\\~',FullBetaEstimates)

# Latex format of table 3
cat(FullBetaEstimates)


## 11) Make table showing differences when changing input variables ------ ##
# Using young bull as standard individual
Idx_standard_Carcass <- which(Yield_input$category==162)

Standard_Animal <- data.frame(conformation = mean(Yield_input$conformation[
                                                Idx_standard_Carcass]),
                              fat_group = mean(Yield_input$fat_group[
                                                Idx_standard_Carcass]),
                              weight = mean(Yield_input$weight[
                                              Idx_standard_Carcass]),
                              category = as.factor(162))

# The different predictors to evaluate
predictors <- c("Prediction", "conformation","fat_group","weight",
                levels(Yield_input$category)[-which(
                  levels(Yield_input$category)==162)])

# Construct table with differences
PercTab <- matrix(NA,length(predictors),
                  dim(ParameterEstimates$Beta_hat)[2])

rownames(PercTab) <- predictors
colnames(PercTab) <- colnames(ParameterEstimates$Beta_hat)

# Prediction (in %) for standard animal
tmp_pred <- predict(Yield_Model,newdata = Standard_Animal)
PercTab[1,] <- exp(tmp_pred)/(1+exp(tmp_pred))

# Update for conformation, fat_group and weight
cc <- 2
for(ii in 1:3)
{
  Tmp_df <- Standard_Animal
  Tmp_df[,ii] <- Tmp_df[,ii]+1
  tmp_pred <- predict(Yield_Model,newdata = Tmp_df)
  PercTab[cc,] <- exp(tmp_pred)/(1+exp(tmp_pred)) - PercTab[1,]
  cc <- cc+1
}

# Update for category
jj_vec <- 1:length(levels(Yield_input$category))
jj_vec <- jj_vec[-which(levels(Yield_input$category)==162)]
for(jj in jj_vec)
{
  Tmp_df <- Standard_Animal
  Tmp_df$category <- factor(levels(Yield_input$category)[jj],
                          levels = levels(Yield_input$category))
  tmp_pred <- predict(Yield_Model,newdata = Tmp_df)
  PercTab[cc,] <- exp(tmp_pred)/(1+exp(tmp_pred)) - PercTab[1,]
  cc <- cc+1
}


PercTab <- round(PercTab*100,2)
rownames(PercTab) <- c("Prediction","Conformation","Fat group",
                       "Weight","Heifer",
                       "Young cow","Cow")

PercTab <- print.xtable(xtable(PercTab,caption = 'Predicted effects of 
                               conformation, fat group, weight, category 
                               on expected yield. Results are based on 
                               evaluation of the linear regression model. 
                               Columns represent the yield categories. 
                               The first row shows predictions for a 
                               standard individual, i.e. a young bull 
                               with corresponding mean weight, 
                               conformity and fat group. The values in 
                               the rows for conformation, fat group and 
                               weight all represent the expected change 
                               in percentage for the standard individual 
                               with an increase of one for the predictor 
                               in question. The values in the rows 
                               representing category, show the expected 
                               change if category is changed from young bull.', 
                               align = c('l','r','r','r','r','r','r','r'),
                               label = 'Tab:Percent'), caption.placement = 'top', 
                        comment=FALSE, print.results = FALSE, 
                        scalebox='1')

# Latex format of table 4
cat(PercTab)


## 12) Make table showing residuals for breed groups --------------------- ##
# Update for breed comb
Residual_df <- data.frame(Residuals = I(Yield_input$PP-
                    exp(predict(Yield_Model))/(1+exp(predict(Yield_Model)))),
                          Breed = I(Yield_input$BreedHR))

Format_func_res <- function(CC)
{
  #TDf <- cbind()
  res <- paste(format(round(100*CC[,"Estimate"],2),nsmall = 2),
               ifelse(CC[,"Pr(>|t|)"]<0.05,'*','space'),sep ='')
}


# Average residuals per breed group
BreedTab <- sapply(coef(summary(lm(Residuals~Breed-1,data=Residual_df))),
                                Format_func_res)

rownames(BreedTab) <- names(BreedGroups)

BreedTab <- gsub('\\( ','\\(',BreedTab)
BreedTab <- gsub('\\( ','\\(',BreedTab)
BreedTab <- gsub('space','\\~',BreedTab)

BreedTab <- print.xtable(xtable(BreedTab,
                caption = 'Predicted effects of breed group on 
                expected yield. Results are based on the residual 
                analysis. Columns represent the yield categories. 
                The values show the average percentage residual 
                for the breed group in question. Significant effects 
                (p-values $<0.05$, t-test) are marked with 
                an asterix (*)',
                align = c('l','r','r','r','r','r','r','r'),
                label = 'Tab:Percent'), caption.placement = 'top', 
                comment=FALSE, print.results = FALSE, 
                scalebox='1')

# Latex format of table 5
cat(BreedTab)


## 13) Make figure showing error correlations ---------------------------- ##

# Function to test significant correlations
cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

# Find p-values for correlations
res1 <- cor.mtest(residuals(Yield_Model),0.95)
colnames(res1[[1]]) <- colnames(ParameterEstimates$Corr_mat_hat)
rownames(res1[[1]]) <- colnames(ParameterEstimates$Corr_mat_hat)

# Saving tiff image to working directory
tiff(filename = 'Fig_Correlation.tiff',
     width = 12, height = 12, units = "cm", pointsize = 12, res = 1000,
     compression = "lzw")

par(mar = c(5, 4, 4, 2) + 0.1)
corrplot(ParameterEstimates$Corr_mat_hat, type="upper",method="color",
         p.mat = res1[[1]], sig.level=0.05)


dev.off()

## 14) Make figure showing surfaces as function of fat group and conf. --- ##

# Create data frame with all combinations of fat group and confirmation
PredDF <- data.frame(YY = rep(1,15*15),
                     conformation = rep(1:15,15),
                     fat_group = rep(1:15, each = 15),
                     weight = rep(mean(Yield_input$weight[
                       Idx_standard_Carcass]),15*15),
                     category = factor(rep(162,15*15),levels = 
                                         levels(Yield_input$category)))

PredDF$category[1:14] <- rep(c(162,166,168,169),2)


#Create predicted values
XXpred <- model.matrix(lm(Mod_form,data=PredDF))
XXpred[,grep("category",colnames(XXpred))] <- 0


YY_pred <- XXpred%*%ParameterEstimates$Beta_hat

YY_pred_pro <- 100*exp(YY_pred)/(1+exp(YY_pred))

colnames(YY_pred_pro)<- gsub('bonewaste','bone \\& waste',
                             colnames(YY_pred_pro))


# Saving tiff image to working directory
tiff(filename = 'Predictions3D.tiff',
     width = 20, height = 30, units = "cm", pointsize = 20,res = 1000, compression = "lzw")
#windows(width=8, height=12)
layout(matrix(1:6,3,2,byrow=TRUE))

for(ii in 1:6)
{
  par(mar=c(1.5,0.5,1,0.5))
  surf3D(matrix(PredDF$conformation,15,15,byrow=TRUE),matrix(PredDF$fat_group,15,15,byrow=TRUE),
         matrix(YY_pred_pro[,ii],15,15,byrow=TRUE),col='blue',border='black',
         main = colnames(YY_pred_pro)[ii],xlim = c(1,15),ylim=c(1,15),
         zlim = range(c(100*Yield_input$PP[,ii],YY_pred_pro[,ii])),xlab = 'Conformation', 
         ylab='Fat group',bty='b2',zlab = '%',theta=40,phi=15,ticktype='detailed',nticks=5)
  scatter3D(Yield_input$conformation,Yield_input$fat_group,100*Yield_input$PP[,ii],add=TRUE,
            col = '#9F2B68',pch=19)
  
}
dev.off()


## 15) Compare breeds registered in NN and at the slaughterhouses.   ##
Idx_HR <- which(rowSums(Yield_data$Breed16,na.rm=TRUE)==1)

Delta_breed <- rep(NA,length(BreedGroups))
names(Delta_breed) <- names(BreedGroups)

for(bb in 1:length(BreedGroups))
{
  Idx_tmp <- Idx_HR[which(is.element(Yield_input$breed_vec[Idx_HR],
                                     as.numeric(BreedGroups[[bb]])))]
  Delta_breed[bb] <- mean(Yield_input$BreedHR[Idx_tmp,bb])
}

# Results from Delta_breed reported in discussion.
print(Delta_breed)
