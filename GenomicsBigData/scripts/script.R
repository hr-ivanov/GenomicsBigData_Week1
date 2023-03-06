# weight_kg <- 55
# chislo <- weight_kg*1000
# weight_kg <- 100
# weight_g <- 1000*weight_kg
# weight_kg
# weight_g
# chislo

# mass <- 47.5
# mass
# age  <- 122
# age
# mass <- mass * 2.0
# mass
# age  <- age - 20
# age
# mass_index <- mass/age
# mass_index
class(weight_g)
str(weight_g)

v <- c(1, 2, 3, "a", "b", FALSE)
class(v)
x <- c(2:20)
class(x)

combined <- c(x,v)
combined

chromosomes <- c(1:22, "X", "Y")
chromosomes

x+1
x+x
x**2

chromosomes[c(23, 9)]

chromosomes2 <- chromosomes[c(23,20,21,1,5,6)]
chromosomes2

weight_g <- c(21, 34, 39, 54, 55)
weight_g > 50
weight_g[weight_g<30 | weight_g>50]
weight_g[weight_g >= 30 | weight_g == 21]

animals <- c("mouse", "rat", "dog", "cat")
animals %in% c("rat", "cat", "dog", "duck", "goat")

c("rat", "cat", "dog", "duck", "goat") %in% animals

"2">"120"

heights <- c(160, 175, 152, 165, NA, 172, 154, 178, 155, 150, 163, 175, 160, 161, NA, 182, 165, 162, 178, 192, 167)
median(heights, na.rm = TRUE)
heightsNoEmpty <- heights[!is.na(heights)]
length(heightsNoEmpty[heightsNoEmpty>=175])
