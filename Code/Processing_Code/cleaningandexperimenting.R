require(ggplot2)
require(dplyr)

## ---- loaddata --------
data_location <- "../../Data/Raw_data/Oreophryne_Character_Measurements.csv"
data_path <- "../../../Data/Raw_data/"

rawdata <- read.csv(data_location, check.names=FALSE)

## ---- cleanup --------
#Making a new rectangular dataframe for only converted measurements (mm not pixels) for each specimen

# exclude rows with NAs
nas <- which( is.na(rawdata$BPBM) ) # find which rows have NA 
dat <- rawdata[-nas,] # exclude these rows
head(dat)

#removing unnecessary columns, artifacts from importing pixel measurements from ImageJ
dat2 <- dat[-c(5:13)]

#cleaning up column names
colnames(dat2) <- c("genus","species", "BPBM", "SVL", "finger", "toe", "ft", "anterior", "posterior", "ap")

head(dat2)

## ---- exploratoryplots --------
# create scatter plots of SVL vs finger:toe ratio and SVL vs anterior width:posterior width of palatal groove, colored by genus
svl.vs.ft <- ggplot(data = dat2) + geom_point(aes(x = SVL, y = ft, col=dat2$genus))
svl.vs.ft

svl.vs.ap <- ggplot(data = dat2) + geom_point(aes(x = SVL, y = ap, col=dat2$genus))
svl.vs.ap

# just for fun... ft vs ap by genus
ft.vs.ap <- ggplot(data = dat2) + geom_point(aes(x = ft, y = ap, col=dat2$genus))
ft.vs.ap

#density plots
ft.dens <- dat2 %>%    # CC by species
        ggplot( aes(x=`ft`)) + 
		geom_density( aes(fill=genus), alpha=.5)
ft.dens

ap.dens <- dat2 %>%    # CC by species
        ggplot( aes(x=`ap`)) + 
		geom_density( aes(fill=genus), alpha=.5)
ap.dens
# we can see some trends but no clear distinctions yet

## ---- summarystats ------
#lets see means for each species
dat2$gensp <- paste0(dat2$genus, dat2$species)

dat2 %>%                                      
  group_by(gensp) %>%                         
  summarise_at(vars(finger, toe, ft, anterior, posterior, ap),          
               list(mean)) 

# and for each genus
dat2 %>%                                      
  group_by(genus) %>%                         
  summarise_at(vars(finger, toe, ft, anterior, posterior, ap),          
               list(mean)) 

## ---- ttests ------
# trying package rempsyc to make pretty ttest tables
require(rempsyc)
require(effectsize)
require(flextable)
t.test.results <- nice_t_test(
  data = dat2,
  response = names(dat2)[4:9],
  group = "genus",
  warning = FALSE)
t.test.results

my_table <- nice_table(t.test.results)
my_table

# t-test results are really only valid for the ratios ft and ap because finger/toe size and anterior/posterior palatal groove widths likely scale to body size, should be standardized

## ---- PCA --------
#let's try a PCA, this comes from a datacamp tutorial <https://www.datacamp.com/tutorial/pca-analysis-r>
require(corrr)
require(ggcorrplot)
require(FactoMineR)
require(factoextra)

str(dat2) # show summary of data
colSums(is.na(dat2)) # check for nulls
dat3 <- dat2[,4:10] # take only numerical columns

head(dat3)

dat4 <- scale(dat3) # normalize data
head(dat4)

corr_matrix <- cor(dat4) 
ggcorrplot(corr_matrix) # view a correlation matrix for the variables

data.pca <- princomp(corr_matrix) # calculate PCA analysis
summary(data.pca) # print summary of results

data.pca$loadings[, 1:2] 

fviz_eig(data.pca, addlabels = TRUE)

## ---- phylogenyexperiment ----
#I want to use one of the ways to plot either a heat map or dotTree as in this tutorial <http://www.phytools.org/Cordoba2017/ex/15/Plotting-methods.html> but I am having trouble getting the matrix (dat3) to match the tree
require(phytools)

tree_location <- "../../Data/Raw_data/asterophryinae_partitions.nex.timetree.nwk"
tree <- read.newick(tree_location)

species<-c("Oreophryneinornata",   "Oreophryneloriae" ,    "Oreophrynenotata"   , 
 "Oreophryneparkeri" ,   "Oreophrynebiroi" ,     "Oreophryneanamiatoi" 
,"Auparoparopenelopeia", "Auparoparoinsulana" ,  "Auparoparophoebe"    
, "Auparoparopicticrus" , "Auparoparomatawan"  ,  "Auparoparoezra"  ) #select included taxa

pruned.tree<-drop.tip(tree,tree$tip.label[-match(species, tree$tip.label)])
write.tree(pruned.tree) #remove other taxa
plot(pruned.tree)


