# Memory experiment

dat <- rbind(data.frame(Memory = c(9,8,6,8,10,4,6,5,7,7), task = "Counting"),
             data.frame(Memory = c(7,9,6,6,6,11,6,3,8,7), task = "Rhyming"),
             data.frame(Memory = c(11,13,8,6,14,11,13,13,10,11), task = "Adjective"),
             data.frame(Memory = c(12,11,16,11,9,23,12,10,19,11), task = "Imagery"),
             data.frame(Memory = c(10,19,14,5,10,11,14,15,11,11), task = "Intentional")
             )

dat$task <- factor(dat$task, levels = c("Counting", "Rhyming", "Adjective",
                                        "Imagery", "Intentional"))

fit <- lm(data = dat, Memory ~ task)
anova(fit)

# Given the large F statistic and the p-value < 0.001, we reject the null
# hypothesis and conclude that the level of processing affects how much material
# is remembered.

# We can speak of a causal effect of the task since participants were 
# randomized to each "treatment" arm. 

# Note that the function aov() can also be used to perform the analysis of variance
fit2 <- aov(data = dat, Memory ~ task)
summary(fit2)

# We now have a confirmation that the task has a significant effect on the variability
# of the memory scores, but we don't know exactly which groups differ from one another.
# The function TukeyHSD, which returns the different contrasts between the task groups,
# is compatible with the aov object.
TukeyHSD(fit2)

# Note that the p-values are adjusted for multiple comparisons using the Tukey
# method.

# We see that the adjective, imagery and intentional tasks lead to significantly
# higher memory scores compared with the counting task.
# Aswell, the adjective, imagery and intentional tasks lead to more material
# remembered compared with the rhyming task.
