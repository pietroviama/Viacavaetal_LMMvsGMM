####################
#####################
#####################CHECK NEXT LINES BELOW AND COMPARE TO NEW LINES ABOVE. DO NOT FORGET TO CHECK IF LD AND PC %s ARE GOOD.

sizefree_dfd98.x <- subset(sizefree_dfd98, select = - ID)
sizefree_dfd98.x.pop <- sizefree_dfd98.x[-c(which(sizefree_dfd98.x$Population=="?")),]
modelsizefree_dfd98 <- lda(Population~., data = sizefree_dfd98.x.pop, prior = c(1,1,1)/3, CV = TRUE)
modelsizefree_dfd98
PCAd98 <- prcomp(sizefree_dfd98.x.pop[,1:21])
summary(PCAd98)


modelsizefree_dfd98.plot <- lda(Population~., data = sizefree_dfd98.x.pop, prior = c(1,1,1)/3)
ldsd98 <- predict(modelsizefree_dfd98.plot)$x
dataEllipse(ldsd98[,1], ldsd98[,2], groups=as.factor(sizefree_dfd98.x.pop$Population), levels=0.95, ylim=c(-4,6), xlim=c(-7,7))

dataldad98 <- data.frame(ldsd98[,1], ldsd98[,2], sizefree_dfd98.x.pop$Population)

plotldad98 <- ggplot(dataldad98, aes(x=ldsd98...1., y=ldsd98...2., colour = sizefree_dfd98.x.pop.Population)) +
  labs(x = "LD 1 (73.34%)", y = "LD 2 (26.66%)") +
  scale_color_manual(values = c("#332288", "#CC6677", "#999933")) +
  geom_point(size = 4, alpha = 0.8) +
  theme_bw() +
  stat_ellipse(size = 2, level = 0.95) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_blank(), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), axis.text = element_blank(), legend.position = "none")
plot(plotldad98)



dfintlmlda.popx.sizefree_dfd98 <- sizefree_dfd98.x[-c(which(sizefree_dfd98.x$Population=="subtropicus"), which(sizefree_dfd98.x$Population=="south"), which(sizefree_dfd98.x$Population=="north")),]
predsizefree_dfd98.lda <- predict(modelsizefree_dfd98.plot, dfintlmlda.popx.sizefree_dfd98)

rda.sizefree_dfd98 <- rda(Population ~ ., data=sizefree_dfd98.x.pop, crossval=TRUE, prior = 1)
predsizefree_dfd98.rda <- predict(rda.sizefree_dfd98, sizefree_dfd98.x.pop)
predsizefree_dfd98.rda.x <- predict(rda.sizefree_dfd98, dfintlmlda.popx.sizefree_dfd98)

attr.sizefree_dfd98 <- list(dim = c(136,3), dimnames = list(as.character(1:136), c("north", "south", "subtropicus") ))
attributes(predsizefree_dfd98.rda$posterior) <- attr.sizefree_dfd98

attr2.sizefree_dfd98 <- list(levels = c("north", "south", "subtropicus"), class ="factor")
attributes(sizefree_dfd98.x.pop$Population) <- attr2.sizefree_dfd98
factor.sizefree_dfd98 <- as.factor(sizefree_dfd98.x.pop$Population)

ucpm.sizefree_dfd98.rda <- ucpm(predsizefree_dfd98.rda$posterior, factor.sizefree_dfd98)
ucpm.sizefree_dfd98.rda

ucpm.sizefree_dfd98.lda <- ucpm(modelsizefree_dfd98$posterior, factor.sizefree_dfd98)
ucpm.sizefree_dfd98.lda

#For Baker, 2013



sizefree_dfb13.x <- subset(sizefree_dfb13, select = - ID)
sizefree_dfb13.x.pop <- sizefree_dfb13.x[-c(which(sizefree_dfb13.x$Population=="?")),]
modelsizefree_dfb13 <- lda(Population~., data = sizefree_dfb13.x.pop, prior = c(1,1,1)/3, CV = TRUE)
modelsizefree_dfb13
PCAb13 <- prcomp(sizefree_dfb13.x.pop[,1:18])
summary(PCAb13)
modelsizefree_dfb13.plot <- lda(Population~., data = sizefree_dfb13.x.pop, prior = c(1,1,1)/3)
ldsb13 <- predict(modelsizefree_dfb13.plot)$x
dataEllipse(ldsb13[,1], ldsb13[,2], groups=as.factor(antechinusdatapop$Population), levels=0.95, ylim=c(-4,6), xlim=c(-7,7))

dataldab13 <- data.frame(ldsb13[,1], ldsb13[,2], antechinusdatapop$Population)

plotldab13 <- ggplot(dataldab13, aes(x=ldsb13...1., y=ldsb13...2., colour = antechinusdatapop.Population)) +
  labs(x = "LD 1 (78.65%)", y = "LD 2 (21.35%)") +
  scale_color_manual(values = c("#332288", "#CC6677", "#999933")) +
  geom_point(size = 4, alpha = 0.8) +
  theme_bw() +
  stat_ellipse(size = 2, level = 0.95) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_blank(), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), axis.text = element_blank(), legend.position = "none")
plot(plotldab13)



dfintlmlda.popx.sizefree_dfb13 <- sizefree_dfb13.x[-c(which(sizefree_dfb13.x$Population=="subtropicus"), which(sizefree_dfb13.x$Population=="south"), which(sizefree_dfb13.x$Population=="north")),]
predsizefree_dfb13.lda <- predict(modelsizefree_dfb13, dfintlmlda.popx.sizefree_dfb13)$class

rda.sizefree_dfb13 <- rda(Population ~ ., data=sizefree_dfb13.x.pop, crossval=TRUE, prior = 1)
predsizefree_dfb13.rda <- predict(rda.sizefree_dfb13, sizefree_dfb13.x.pop)
predsizefree_dfb13.rda.x <- predict(rda.sizefree_dfb13, dfintlmlda.popx.sizefree_dfb13)

attr.sizefree_dfb13 <- list(dim = c(136,3), dimnames = list(as.character(1:136), c("north", "south", "subtropicus") ))
attributes(predsizefree_dfb13.rda$posterior) <- attr.sizefree_dfb13

attr2.sizefree_dfb13 <- list(levels = c("north", "south", "subtropicus"), class ="factor")
attributes(sizefree_dfb13.x.pop$Population) <- attr2.sizefree_dfb13
factor.sizefree_dfb13 <- as.factor(sizefree_dfb13.x.pop$Population)

ucpm.sizefree_dfb13.rda <- ucpm(predsizefree_dfb13.rda$posterior, factor.sizefree_dfb13)
ucpm.sizefree_dfb13.rda

ucpm.sizefree_dfb13.lda <- ucpm(modelsizefree_dfb13$posterior, factor.sizefree_dfb13)
ucpm.sizefree_dfb13.lda

#For Travouillon, 2016


sizefree_dft16.x <- subset(sizefree_dft16, select = - ID)
sizefree_dft16.x.pop <- sizefree_dft16.x[-c(which(sizefree_dft16.x$Population=="?")),]
modelsizefree_dft16 <- lda(Population~., data = sizefree_dft16.x.pop, prior = c(1,1,1)/3, CV = TRUE)
modelsizefree_dft16
PCAt16 <- prcomp(sizefree_dft16.x.pop[,1:22])
summary(PCAt16)

modelsizefree_dft16.plot <- lda(Population~., data = sizefree_dft16.x.pop, prior = c(1,1,1)/3)
ldst16 <- predict(modelsizefree_dft16.plot)$x
dataEllipse(ldst16[,1], ldst16[,2], groups=as.factor(sizefree_dft16.x.pop$Population), levels=0.95, ylim=c(-4,6), xlim=c(-7,7))

dataldat16 <- data.frame(ldst16[,1], ldst16[,2], antechinusdatapop$Population)

plotldat16 <- ggplot(dataldat16, aes(x=ldst16...1., y=ldst16...2., colour = antechinusdatapop.Population)) +
  labs(x = "LD 1 (71.48%)", y = "LD 2 (28.52%)") +
  scale_color_manual(values = c("#332288", "#CC6677", "#999933")) +
  geom_point(size = 4, alpha = 0.8) +
  theme_bw() +
  stat_ellipse(size = 2, level = 0.95) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_blank(), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), axis.text = element_blank(), legend.position = "none")
plot(plotldat16)


dfintlmlda.popx.sizefree_dft16 <- sizefree_dft16.x[-c(which(sizefree_dft16.x$Population=="subtropicus"), which(sizefree_dft16.x$Population=="south"), which(sizefree_dft16.x$Population=="north")),]
predsizefree_dft16.lda <- predict(modelsizefree_dft16, dfintlmlda.popx.sizefree_dft16)$class

rda.sizefree_dft16 <- rda(Population ~ ., data=sizefree_dft16.x.pop, crossval=TRUE, prior = 1)
predsizefree_dft16.rda <- predict(rda.sizefree_dft16, sizefree_dft16.x.pop)
predsizefree_dft16.rda.x <- predict(rda.sizefree_dft16, dfintlmlda.popx.sizefree_dft16)

attr.sizefree_dft16 <- list(dim = c(136,3), dimnames = list(as.character(1:136), c("north", "south", "subtropicus") ))
attributes(predsizefree_dft16.rda$posterior) <- attr.sizefree_dft16

attr2.sizefree_dft16 <- list(levels = c("north", "south", "subtropicus"), class ="factor")
attributes(sizefree_dft16.x.pop$Population) <- attr2.sizefree_dft16
factor.sizefree_dft16 <- as.factor(sizefree_dft16.x.pop$Population)

ucpm.sizefree_dft16.rda <- ucpm(predsizefree_dft16.rda$posterior, factor.sizefree_dft16)
ucpm.sizefree_dft16.rda

ucpm.sizefree_dft16.lda <- ucpm(modelsizefree_dft16$posterior, factor.sizefree_dft16)
ucpm.sizefree_dft16.lda

#for consensus

sizefree_dfcon.x <- subset(sizefree_dfcon, select = - ID)
sizefree_dfcon.x.pop <- sizefree_dfcon.x[-c(which(sizefree_dfcon.x$Population=="?")),]
modelsizefree_dfcon <- lda(Population~., data = sizefree_dfcon.x.pop, prior = c(1,1,1)/3, CV = TRUE)
modelsizefree_dfcon

dfintlmlda.popx.sizefree_dfcon <- sizefree_dfcon.x[-c(which(sizefree_dfcon.x$Population=="subtropicus"), which(sizefree_dfcon.x$Population=="south"), which(sizefree_dfcon.x$Population=="north")),]
predsizefree_dfcon.lda <- predict(modelsizefree_dfcon, dfintlmlda.popx.sizefree_dfcon)$class

rda.sizefree_dfcon <- rda(Population ~ ., data=sizefree_dfcon.x.pop, crossval=TRUE, prior = 1)
predsizefree_dfcon.rda <- predict(rda.sizefree_dfcon, sizefree_dfcon.x.pop)
predsizefree_dfcon.rda.x <- predict(rda.sizefree_dfcon, dfintlmlda.popx.sizefree_dfcon)

attr.sizefree_dfcon <- list(dim = c(136,3), dimnames = list(as.character(1:136), c("north", "south", "subtropicus") ))
attributes(predsizefree_dfcon.rda$posterior) <- attr.sizefree_dfcon

attr2.sizefree_dfcon <- list(levels = c("north", "south", "subtropicus"), class ="factor")
attributes(sizefree_dfcon.x.pop$Population) <- attr2.sizefree_dfcon
factor.sizefree_dfcon <- as.factor(sizefree_dfcon.x.pop$Population)

ucpm.sizefree_dfcon.rda <- ucpm(predsizefree_dfcon.rda$posterior, factor.sizefree_dfcon)
ucpm.sizefree_dfcon.rda

ucpm.sizefree_dfcon.lda <- ucpm(modelsizefree_dfcon$posterior, factor.sizefree_dfcon)
ucpm.sizefree_dfcon.lda






ggord(modelsizefree_dfvd00.plot, sizefree_dfvd00.x.pop$Population)
ggord(modelsizefree_dfd98.plot, sizefree_dfd98.x.pop$Population)
ggord(modelsizefree_dfb13.plot, sizefree_dfb13.x.pop$Population)
ggord(modelsizefree_dft16.plot, sizefree_dft16.x.pop$Population)
ggord(modelsizefree_gmm.plot, antechinusdatapop$Population)




#########3
########################3
#########################3

#PCA of raw GMM data (unscaled or partial Procrustes superimposition)

gpapprocall <- ProcGPA(A_reorder, scale = FALSE)

PCAgmmrawall <- gm.prcomp(gpapprocall$rotated)

summary(PCAgmmrawall)

PCgmmrawpop <- data.frame(PCAgmmrawall$x[-c(which(antechinusdata$Population=="?")),1:23])

PCgmmrawpop$Population <- antechinusdatapop$Population

ldagmmrawpop <- lda(Population~., data = PCgmmrawpop, prior = c(1,1,1)/3)

ldsgmmrawpop <- predict(ldagmmrawpop)


PCgmmrawunknown <- data.frame(PCAgmmrawall$x[c(which(antechinusdata$Population=="?")),1:23])


ldapredgmmraw <- predict(ldagmmrawpop, newdata = PCgmmrawunknown)

gmmrawpostdf <- as.data.frame(ldapredgmmraw$posterior)

gmmrawpostdf$max <- apply(X = gmmrawpostdf, MARGIN = 1, FUN = max)*100

gmmrawpostdf$maxround <- as.factor(round(gmmrawpostdf$max, digits = 2))





#COMPARING UCPM NOT INCLUDING UNKOWN IN GMM AND INCLUDING UNKNOWN FROM BEGINNING

ldagmmrawpopcv <- lda(Population~., data = PCgmmrawpop, prior = c(1,1,1)/3, CV=TRUE)

ucpmgmmrawpop <- ucpm(ldagmmrawpopcv$posterior, factor.antepop)

ucpmgmmrawpop
ucpmgmmraw
