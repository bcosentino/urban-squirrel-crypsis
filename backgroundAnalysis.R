library(glmmTMB)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####Load data#####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

load("backgroundData.RData")

#data frames:
#r = data from road habitat
#nr = data from non-road habitats

#variables in each data frame:
#env = habitat type 
#sceneid = identification code for unique scene within habitat
#season = leaf-on or leaf-off seasons
#b.full = proportion of full background matching melanic morph
#g.full = proportion of full background matching gray morph
#b.immediate = proportion of immediage background matching melanic morph
#g.immediate = proportion of immediate background matching gray morph
#env.scene.id = unique ID for combination of habitat and scene id
#perspective = ground or aerial viewpoint
#position = branch, climb, or ground postures

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####Variable adjustments#####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Add/subtract trace value to avoid 0/1 observations for beta dist
nr$b.full[nr$b.full == 0] <- 1e-5
nr$b.full[nr$b.full == 1] <- 1 - 1e-5

nr$b.immediate[nr$b.immediate == 0] <- 1e-5
nr$b.immediate[nr$b.immediate == 1] <- 1 - 1e-5

r$b.full[r$b.full == 0] <- 1e-5
r$b.full[r$b.full == 1] <- 1 - 1e-5

r$b.immediate[r$b.immediate == 0] <- 1e-5
r$b.immediate[r$b.immediate == 1] <- 1 - 1e-5

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####Beta regression models#####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#non-road environments, full background
m.nr.f <- glmmTMB(b.full ~ env * season + env * position + env * perspective + (1 | env.scene.id), 
                  data = nr,family = beta_family())
car::Anova(m.nr.f)

#non-road environments, immediate background
m.nr.i <- glmmTMB(b.immediate ~ env * season + env * position + env * perspective + (1 | env.scene.id), 
                  data = nr, family = beta_family())
car::Anova(m.nr.i)

#non-road environments, full background
m.r.f <- glmmTMB(b.full ~ season + (1 | env.scene.id), 
                  data = r,family = beta_family())
car::Anova(m.r.f)

#non-road environments, immediate background
m.r.i <- glmmTMB(b.immediate ~ season + (1 | env.scene.id), 
                  data = r, family = beta_family())
car::Anova(m.r.i)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Model predictions#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#predictions for non-road
p.nr.f <- expand.grid(env = unique(nr$env),
                      season = unique(nr$season),
                      position = unique(nr$position),
                      perspective = unique(nr$perspective))
o.nr.f <- predict(m.nr.f, newdata = p.nr.f,
                  type = "link", se.fit = TRUE, re.form = ~0)
p.nr.f <- cbind(p.nr.f, o.nr.f)
p.nr.f$fit.prob <- plogis(p.nr.f$fit)
p.nr.f$ll.prob <- plogis(p.nr.f$fit - 1.96*p.nr.f$se.fit)
p.nr.f$ul.prob <- plogis(p.nr.f$fit + 1.96*p.nr.f$se.fit)
p.nr.f$scale <- "full"

p.nr.i <- expand.grid(env = unique(nr$env),
                      season = unique(nr$season),
                      position = unique(nr$position),
                      perspective = unique(nr$perspective))
o.nr.i <- predict(m.nr.i, newdata = p.nr.i,
                  type = "link", se.fit = TRUE, re.form = ~0)
p.nr.i <- cbind(p.nr.i, o.nr.i)
p.nr.i$fit.prob <- plogis(p.nr.i$fit)
p.nr.i$ll.prob <- plogis(p.nr.i$fit - 1.96*p.nr.i$se.fit)
p.nr.i$ul.prob <- plogis(p.nr.i$fit + 1.96*p.nr.i$se.fit)
p.nr.i$scale <- "immediate"

p.nr <- rbind(p.nr.f, p.nr.i) 

#predictions for road
p.r.f <- expand.grid(season = unique(r$season))
o.r.f <- predict(m.r.f, newdata = p.r.f,
                  type = "link", se.fit = TRUE, re.form = ~0)
p.r.f <- cbind(p.r.f, o.r.f)
p.r.f$fit.prob <- plogis(p.r.f$fit)
p.r.f$ll.prob <- plogis(p.r.f$fit - 1.96*p.r.f$se.fit)
p.r.f$ul.prob <- plogis(p.r.f$fit + 1.96*p.r.f$se.fit)
p.r.f$scale <- "full"

p.r.i <- expand.grid(season = unique(r$season))
o.r.i <- predict(m.r.i, newdata = p.r.i,
                  type = "link", se.fit = TRUE, re.form = ~0)
p.r.i <- cbind(p.r.i, o.r.i)
p.r.i$fit.prob <- plogis(p.r.i$fit)
p.r.i$ll.prob <- plogis(p.r.i$fit - 1.96*p.r.i$se.fit)
p.r.i$ul.prob <- plogis(p.r.i$fit + 1.96*p.r.i$se.fit)
p.r.i$scale <- "immediate"

p.r <- rbind(p.r.f, p.r.i)
