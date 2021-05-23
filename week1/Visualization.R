# Title     : TODO
# Objective : TODO
# Created by: alejanpi
# Created on: 5/18/2021

mpg <- ggplot2::mpg

hist(mpg$cty)

mean(mpg$cty)
sd(mpg$cty)

hist(mpg$cty,
     xlab   = "Miles Per Gallon (City)",
     main   = "Histogram of MPG (City)",
     breaks = 12,
     color  = "darkorange",
     border = "dodgerblue"
     )

barplot(table(mpg$drv),
        xlab = "Drivetrain (f = FWD, r = RWD, 4 = 4WD)",
        main = "Drivetrains",
        col  = "dodgerblue",
        border = "darkorange"
        )

boxplot(hwy ~ drv, data = mpg)

boxplot(hwy ~ drv, data = mpg,
        xlab = "Drivetrain (f = FWD, r = RWD, 4 = 4WD)",
        ylab = "Miles Per Gallon (Highway)",
        main = "MPG (Highway) vs Drivetrain",
        pch = 20,
        cex = 2,
        border  = "dodgerblue",
        col = "darkorange"
        )

plot(hwy ~ displ, data = mpg,
        xlab = "Engine Displacement (Liters)",
        ylab = "Miles Per Gallon (Highway)",
        main = "MPG (Highway) vs Engine Displacement",
        pch = 20,
        cex = 2,
        col = "dodgerblue"
)

## different plotting systems

library(lattice)
xyplot(hwy ~ displ, data = mpg)

library(ggplot2)
ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()