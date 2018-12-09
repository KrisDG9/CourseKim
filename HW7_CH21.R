#Reducing code duplication has three main benefits:

#1. It’s easier to see the intent of your code, because your eyes are drawn to what’s different, not what stays the same.

#2. It’s easier to respond to changes in requirements. As your needs change, you only need to make changes in one place, rather than remembering to change every place that you copied-and-pasted the code.

#3. You’re likely to have fewer bugs because each line of code is used in more places.

#One tool for reducing duplication is functions, which reduce duplication by extracting repeated patterns of code into independent pieces that can be reused.

#Another tool for reducing duplication is iteration, which helps you when you need to do the same thing to multiple inputs.

#So basically iteration is repeating the same operation on different columns, or on different datasets. 

#Two important iteration paradigms: imperative programming and functional programming. 

#Imperative: tools like for loops and while loops. Clear but loops are verbose and have code that is duplicated for every for loop.

#Functional: tools to extract out duplicated code, so each common for loop pattern gets its own function. Shorter, more efficient.

library(tidyverse) #There is some iteration stuff in base R but also in purrr.

#Imagine we have this simple tibble:

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

#If you want the median of earch column, you could use copy-paste:

median(df$a)

median(df$b)

median(df$c)

median(df$d)

#But then we are copy-pasting a lot of stuff. Instead, we could use a for loop:

output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}
output

#Every for loop has three components:

#1. The output: output <- vector("double", length(x)). 

  #Before you start the loop, you must always allocate sufficient space for the output. 

  #This is important for efficiency: if you grow the for loop at each iteration using c() (for example), your for loop will be very slow.

  #You can create an empty vector of given length using the vector() function, which has a type argument (e.g. integer) and a length argument.

#2. The sequence: i in seq_along(df). 

  #This determines what to loop over: each run of the for loop will assign i to a different value from seq_along(df). 

  #Seq_along() is a safe version of the familiar 1:length(l), with an important difference: if you have a zero-length vector, seq_along() does the right thing, but 1:length(x) produces an error.

#3. The body: output[[i]] <- median(df[[i]]). 

  #This is the code that does the work. 

  #It’s run repeatedly, each time with a different value for i. 

  #The first iteration will run output[[1]] <- median(df[[1]]), the second will run output[[2]] <- median(df[[2]]), and so on.

#EXERCISES.21.2.1

#Ex1Q: Write for loops to:

  #Compute the mean of every column in mtcars.

  output <- vector("double", ncol(mtcars))
  names(output) <- names(mtcars)
  for (i in names(mtcars)) {
    output[i] <- mean(mtcars[[i]])
  }
  output

  #Determine the type of each column in nycflights13::flights.

  data("flights", package = "nycflights13")
  output <- vector("list", ncol(flights))
  names(output) <- names(flights)
  for (i in names(flights)) {
    output[[i]] <- class(flights[[i]])
  }
  output

  #Compute the number of unique values in each column of iris.

  data("iris")
  iris_uniq <- vector("double", ncol(iris))
  names(iris_uniq) <- names(iris)
  for (i in names(iris)) {
    iris_uniq[i] <- length(unique(iris[[i]]))
  }
  iris_uniq
  
  #Generate 10 random normals for each of mu = -10, 0, 10, and 100.
  
  n <- 10
  mu <- c(-10, 0, 10, 100)
  normals <- vector("list", length(mu))
  for (i in seq_along(normals)) {
    normals[[i]] <- rnorm(n, mean = mu[i])
  }
  normals

#Ex1A: For the second I used a list instead of a vector (because the class can have multiple values). For the fourth we could also have used rnorm() becauses it recycles the mean argument.
  
#Ex2Q: Eliminate the for loop in each of the following examples by taking advantage of an existing function that works with vectors.
  
  #For loop 1:

  stringr::str_c(letters, collapse = "")

  #For loop 2:
    
  sd(x) #or
  sqrt(sum((x - mean(x)) ^ 2) / (length(x) - 1))
    
  #For loop 3:
    
  all.equal(cumsum(x),out)

#Ex3Q: Combine your function writing and for loop skills:
  
  #Write a for loop that prints() the lyrics to the children’s song “Alice the camel”.
  
  humps <- c("five", "four", "three", "two", "one", "no")
  for (i in humps) {
    cat(str_c("Alice the camel has ", rep(i, 3), " humps.",
              collapse = "\n"), "\n")
    if (i == "no") {
      cat("Now Alice is a horse.\n")
    } else {
      cat("So go, Alice, go.\n")
    }
    cat("\n")
  }
  
  #Convert the nursery rhyme “ten in the bed” to a function. Generalise it to any number of people in any sleeping structure.
  
  numbers <- c("ten", "nine", "eight", "seven", "six", "five",
               "four", "three", "two", "one")
  for (i in numbers) {
    cat(str_c("There were ", i, " in the bed\n"))
    cat("and the little one said\n")
    if (i == "one") {
      cat("I'm lonely...")
    } else {
      cat("Roll over, roll over\n")
      cat("So they all rolled over and one fell out.\n")
    }
    cat("\n")
  }

  #Convert the song “99 bottles of beer on the wall” to a function. Generalise to any number of any vessel containing any liquid on any surface.
  
  bottles <- function(i) {
    if (i > 2) {
      bottles <- str_c(i - 1, " bottles")
    } else if (i == 2) {
      bottles <- "1 bottle"
    } else {
      bottles <- "no more bottles"
    }
    bottles
  }
  
  beer_bottles <- function(n) {
     for (i in seq(n, 1)) {
      cat(str_c(bottles(i), " of beer on the wall, ", bottles(i), " of beer.\n"))
      cat(str_c("Take one down and pass it around, ", bottles(i - 1),
                " of beer on the wall.\n\n"))
    }
    cat("No more bottles of beer on the wall, no more bottles of beer.\n")
    cat(str_c("Go to the store and buy some more, ", bottles(n), " of beer on the wall.\n"))
  }
  beer_bottles(101)

#Ex4Q: It’s common to see for loops that don’t preallocate the output and instead increase the length of a vector at each step:
  
output <- vector("integer", 0)
for (i in seq_along(x)) {
  output <- c(output, lengths(x[[i]]))
}
output

#How does this affect performance? Design and execute an experiment.

library(microbenchmark)

add_to_vector <- function(n) {
  output <- vector("integer", 0)
  for (i in seq_len(n)) {
    output <- c(output, i)
  }
  output  
}
microbenchmark(add_to_vector(10000), times = 3)

add_to_vector_2 <- function(n) {
  output <- vector("integer", n)
  for (i in seq_len(n)) {
    output[[i]] <- i
  }
  output
}
microbenchmark(add_to_vector_2(10000), times = 3)

#Ex4A: The microbenchmark() function will run an R expression a number of times and time it. We see that the pre-allocated vector is much faster than the 'regular' function that appends to an integer vector.

#Four variations on the basic theme of the for loop:

#1. Modifying an existing object, instead of creating a new object.

#Example: we want to rescale every column in a data frame.

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

#To solve this with a for loop we again think about the three components: output (here same as input), sequence (we can think about a data frame as a list of columns, so we can iterate over each column with seq_along(df)), and body (apply rescale01()).

for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}

#You usually modify a list or data frame with this sort of loop, so use [[, not [ (you can also use [[ in all your loops just to make clear that you want to work with a single element).

#2. Looping over names or values, instead of indices: looping patterns.

#There are three basic ways to loop over a vector. 

#In addition to looping over the numeric indices with for (i in seq_along(xs)) and extracting the value with x[[i]], you can also loop over elements or names.

#Loop over the elements: for (x in xs). This is most useful if you only care about side-effects, like plotting or saving a file, because it’s difficult to save the output efficiently.

#Loop over the names: for (nm in names(xs)). This gives you name, which you can use to access the value with x[[nm]]. This is useful if you want to use the name in a plot title or a file name. If you’re creating named output, make sure to name the results vector:

results <- vector("list", length(x))
names(results) <- names(x)

#Iteration over the numeric indices is the most general form, because given the position you can extract both the name and the value:

for (i in seq_along(x)) {
  name <- names(x)[[i]]
  value <- x[[i]]
}

#3. Handling outputs of unknown length.

#Example: you want to simulate some random vectors of random lengths. 

#You might be tempted to solve this problem by progressively growing the vector:

means <- c(0, 1, 2)

output <- double()
for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}
str(output)

#But this is not very efficient because in each iteration, R has to copy all the data from the previous iterations (which takes up time).

#A better solution to save the results in a list, and then combine into a single vector after the loop is done:

out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)
str(unlist(out))

#We used unlist() to flatten a list of vectors into a single vector. A stricter option is to use purrr::flatten_dbl() — it will throw an error if the input isn’t a list of doubles.

#This pattern also occurs if you are generating a long string or if you are generating a big data frame.
  
#For long strings, instead of paste()ing together each iteration with the previous, save the output in a character vector and then combine that vector into a single string with paste(output, collapse = "").

#For big frames, instead of sequentially rbind()ing in each iteration, save the output in a list, then use dplyr::bind_rows(output) to combine the output into a single data frame.

#So if you encounter this, switch to a more complex result object, and then combine in one step at the end.

#4. Handling sequences of unknown length.

#Sometimes you don’t even know how long the input sequence should run for.

#Example is when doing simulations (you might want to loop until you get three heads in a row). 

#You can’t do that sort of iteration with the for loop, but you have to use a while loop. 

#A while loop is simpler than for loop because it only has two components, a condition and a body:

while (condition) {
  # body
}

#A while loop is also more general than a for loop, because you can rewrite any for loop as a while loop, but you can’t rewrite every while loop as a for loop:

for (i in seq_along(x)) {
  # body
}

# Equivalent to

i <- 1
while (i <= length(x)) {
  # body
  i <- i + 1 
}

#Here’s how we could use a while loop to find how many tries it takes to get three heads in a row:

flip <- function() sample(c("T", "H"), 1)

flips <- 0
nheads <- 0

while (nheads < 3) {
  if (flip() == "H") {
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}
flips

#EXERCISES.21.3.5

#Ex1Q: Imagine you have a directory full of CSV files that you want to read in. You have their paths in a vector, files <- dir("data/", pattern = "\\.csv$", full.names = TRUE), and now want to read each one with read_csv(). Write the for loop that will load them into a single data frame.

files <- dir("data/", pattern = "\\.csv$", full.names = TRUE)
df <- vector("list", length(files))
for (filename in seq_along(files)) {
  df[[i]] <- read_csv(files[[i]])
}

df <- bind_rows(df)

#Ex1A: Pre-allocate a list, read each file into a data frame, and assign it to an element in that list. The result is a list of data frames. Then use bind_rows() to combine the list of data frames into a single data frame.

#Ex2Q: What happens if you use for (nm in names(x)) and x has no names? What if only some of the elements are named? What if the names are not unique?

x <- 1:3
print(names(x))
for (nm in names(x)) {
  print(nm)
  print(x[[nm]])
} #When there are no names for the vector, it does not run the code in the loop (it runs zero iterations of the loop).

x <- c(a = 1, 2, c = 3)
names(x)
for (nm in names(x)) {
  print(nm)
  print(x[[nm]])
} #If there are only some names, you get an error if you try to access an element without a name. However, oddly, nm == "" when there is no name.

x <- c(a = 1, a = 2, c = 3)
names(x)
for (nm in names(x)) {
  print(nm)
  print(x[[nm]])
} #If there are duplicate names, then x[[nm]] will give the first element with that name. There is no way to access elements with duplicate names.

#Ex3Q: Write a function that prints the mean of each numeric column in a data frame, along with its name. In addition, with what function can you make sure that the numbers lined up nicely, even though the variable names had different lengths?

show_mean <- function(df, digits = 2) {
  # Get max length of all variable names in the dataset
  maxstr <- max(str_length(names(df)))
  for (nm in names(df)) {
    if (is.numeric(df[[nm]])) {
      cat(str_c(str_pad(str_c(nm, ":"), maxstr + 1L, side = "right"),
                format(mean(df[[nm]]), digits = digits, nsmall = digits),
                sep = " "),
          "\n")
    }
  }
}
show_mean(iris)

#Ex3A: One way is by using str_pad(), and str_length() to ensure that the space given to the variable names is the same.

#Ex4Q: What does this code do? How does it work?

trans <- list( 
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
  }
)
for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}

#Ex4A: This code mutates the disp and am columns: disp is multiplied by 0.0163871 and am is replaced by a factor variable. The code works by looping over a named list of functions. It calls the named function in the list on the column of mtcars with the same name, and replaces the values of that column.

#For loops vs. functionals

#For loops are not as important in R as they are in other languages because R is a functional programming language: it’s possible to wrap up for loops in a function, and call that function instead of using the for loop directly.

#Example of why this is important:

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

#Imagine you want to compute the mean of every column. You could do that with a for loop:

output <- vector("double", length(df))
for (i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}
output

#Let's say you realise that you’re going to want to compute the means of every column pretty frequently, so you extract it out into a function:

col_mean <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- mean(df[[i]])
  }
  output
}

#But then you think it’d also be helpful to be able to compute the median, and the standard deviation, so you copy and paste your col_mean() function and replace the mean() with median() and sd():

col_median <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- median(df[[i]])
  }
  output
}
col_sd <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- sd(df[[i]])
  }
  output
}

#But now you've copy-pasted your code (and that is bad: error-prone etc.).

#And there's a lot of duplication, which we can extract into an additional argument:

f <- function(x, i) abs(x - mean(x)) ^ i

#We can do exactly the same thing with col_mean(), col_median() and col_sd() by adding an argument that supplies the function to apply to each column:

col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}
col_summary(df, median)

col_summary(df, mean)

#The idea of passing a function to another function is extremely powerful idea, and it’s one of the behaviours that makes R a functional programming language.

#The purrr package provides functions that eliminate the need for many common for loops. The apply family of functions in base R (apply(), lapply(), tapply(), etc) solve a similar problem (but purrr is more consistent and thus easier to learn).

#The goal of using purrr functions instead of for loops is to allow you break common list manipulation challenges into independent pieces:
  #You first solve the problem for a single element of the list, and then purrr takes care of generalising your solution to every element in the list.
  #If you’re solving a complex problem, you break it down into bite-sized pieces that allow you to advance one small step towards a solution, and purrr gets you those small pieces that you can compose together with the pipe.

#EXERCISES.21.4.1

#Ex1Q: Read the documentation for apply(). In the 2d case, what two for loops does it generalise?

?apply

#Ex1A: It generalises looping over the rows or columns of a matrix or data-frame.

#Ex2Q: Adapt col_summary() so that it only applies to numeric columns You might want to start with an is_numeric() function that returns a logical vector that has a TRUE corresponding to each numeric column.

col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
} #original function

col_summary2 <- function(df, fun) {
  # test whether each colum is numeric
  numeric_cols <- vector("logical", length(df))
  for (i in seq_along(df)) {
    numeric_cols[[i]] <- is.numeric(df[[i]])
  }
  # indexes of numeric columns
  idxs <- seq_along(df)[numeric_cols]
  # number of numeric columns
  n <- sum(numeric_cols)
  out <- vector("double", n)
  for (i in idxs) {
    out[i] <- fun(df[[i]])
  }
  out
} #adapted function

#The purr package provides a family of functions to help you loop over a vector, do something to each element, and save the results.

#There is one function for each type of output: map() makes a list, map_lgl() makes a logical vector, map_int() makes an integer vector, map_dbl() makes a double vector, and map_chr() makes a character vector.

#Each function takes a vector as input, applies a function to each piece, and then returns a new vector that’s the same length (and has the same names) as the input. 

#Map functions are a step up a tower of abstraction.

#In the past an advantage of map functions was that they were quicker, but nowadays it's chief benefit is not speed, but clarity: they make your code easier to write and to read.

#We can use map-functions to perform the same computations as the last for loop. Those summary functions returned doubles, so we need to use map_dbl():

map_dbl(df, mean)

map_dbl(df, median)

map_dbl(df, sd)

#Compared to using a for loop, focus is on the operation being performed (i.e. mean(), median(), sd()), not the bookkeeping required to loop over every element and store the output. This is even more apparent if you use the pipe:

df %>% map_dbl(mean)

df %>% map_dbl(median)

df %>% map_dbl(sd)

#Differences between map_*() and col_summary():

  #All purrr functions are implemented in C. This makes them a little faster at the expense of readability.

  #The second argument, .f, the function to apply, can be a formula, a character vector, or an integer vector. 

  #map_*() uses … ([dot dot dot]) to pass along additional arguments to .f each time it’s called:

  map_dbl(df, mean, trim = 0.5)

  #The map functions also preserve names:
  
  z <- list(x = 1:3, y = 4:5)
  
  map_int(z, length)  

#Map function shortcuts: there are a few shortcuts that you can use with .f.
  
#Example: we want to fit a linear model to each group in a dataset.
  
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df)) #Here we split up the mtcars dataset in to three pieces (one for each value of cylinder) and fit the same linear model to each piece.
  
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .)) #Purrr provides a shortcut for creating an anonymous function in R: a one-sided formula. (We used . to refer to the current list element, like how i refers to the current index in a for loop.)

models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared) #now we use the shorthand for anonymous functions to extract summary statistics.

models %>% 
  map(summary) %>% 
  map_dbl("r.squared") #We can do that even shorter because extracting names is a common operation for which purrr provides a very short shortcut.

x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% map_dbl(2) #Finally, we can also use an integer to select elements by position.

#Base R apply functions show some similarities with the purrr functions:

  #Lapply() is basically identical to map(), except that map() is consistent with all the other functions in purrr, and you can use the shortcuts for .f.

  #Sapply() is a wrapper around lapply() that automatically simplifies the output. This is useful for interactive work but is problematic in a function because you never know what sort of output you’ll get.

  x1 <- list(
    c(0.27, 0.37, 0.57, 0.91, 0.20),
    c(0.90, 0.94, 0.66, 0.63, 0.06), 
    c(0.21, 0.18, 0.69, 0.38, 0.77)
  )
  x2 <- list(
    c(0.50, 0.72, 0.99, 0.38, 0.78), 
    c(0.93, 0.21, 0.65, 0.13, 0.27), 
    c(0.39, 0.01, 0.38, 0.87, 0.34)
  )
  
  threshold <- function(x, cutoff = 0.8) x[x > cutoff]
  x1 %>% sapply(threshold) %>% str()
  x2 %>% sapply(threshold) %>% str()

  #Vapply is a safe alternative to sapply() because you supply an additional argument that defines the type. The only problem with vapply() is that it’s more typing: vapply(df, is.numeric, logical(1)) is equivalent to map_lgl(df, is.numeric). One advantage of vapply() over purrr’s map functions is that it can also produce matrices (map functions only produce vectors).
  
#EXERCISES.21.5.3
  
#Ex1Q: Write code that uses one of the map functions to:
  
  #Compute the mean of every column in mtcars.
  
  map_dbl(mtcars, mean)

  #Determine the type of each column in nycflights13::flights.
  
  map_chr(nycflights13::flights, typeof)

  #Compute the number of unique values in each column of iris.
  
  map_int(iris, ~ length(unique(.)))
  #or
  map_dbl(iris, ~ length(unique(.)))   

  #Generate 10 random normals for each of mu = -10, 0, 10, and 100.
  
  map(c(-10, 0, 10, 100), ~ rnorm(n = 10, mean = .))

#Ex2Q: How can you create a single vector that for each column in a data frame indicates whether or not it’s a factor?
  
map_lgl(iris, is.factor)

#Ex2A: Just use the is.factor function and combine that with a map function.

#Ex3Q: What happens when you use the map functions on vectors that aren’t lists? What does map(1:5, runif) do? Why?

map(1:5, runif)

#Ex3A: Map functions work with any vectors, not just lists. As with lists, the map functions apply the function to each element of the vector.

#Ex4Q: What does map(-2:2, rnorm, n = 5) do? Why? What does map_dbl(-2:2, rnorm, n = 5) do? Why?

map(-2:2, rnorm, n = 5)

map_dbl(-2:2, rnorm, n = 5)

flatten_dbl(map(-2:2, rnorm, n = 5))

#Ex4A: First takes sample size 5 from 5 normal distributions that have a different mean but the same sd. It returns a list with each element a numeric vectors of length 5. The second errors; this is because map_dbl expects the function it applies to each element to return a numeric vector of length one. (If we want a numeric vector, we could use map() followed by flatten_dbl().)

#Ex5Q: Rewrite map(x, function(df) lm(mpg ~ wt, data = df)) to eliminate the anonymous function.

map(list(mtcars), ~ lm(mpg ~ wt, data = .))

#Failures

#When you use the map functions to repeat many operations, the chances are much higher that one of those operations will fail. 

#When 1 fails, you’ll get an error message, and no output. 

#This is annoying: why does one failure prevent you from accessing all the other successes? 

#We can deal with this situation with the safely() function, which takes a function (a verb) and returns a modified version. 

#In this case, the modified function will never throw an error.

#Instead, it always returns a list with two elements:
  #Result is the original result. If there was an error, this will be NULL.
  #Error is an error object. If the operation was successful, this will be NULL.

#Example:

safe_log <- safely(log)

str(safe_log(10))

str(safe_log("a"))

#When the function succeeds, the result element contains the result and the error element is NULL. When the function fails, the result element is NULL and the error element contains an error object.

#Safely() is designed to work with map:

x <- list(1, 10, "a")

y <- x %>% map(safely(log))

str(y)

#And with purrr we can make it even easier because we can get 2 lists: one of all the errors and one of all the output:

y <- y %>% transpose()

str(y)

#When encountering an error, you'll typically either look at the values of x where y is an error, or work with the values of y that are ok:

is_ok <- y$error %>% map_lgl(is_null)

x[!is_ok]

y$result[is_ok] %>% flatten_dbl()

#Additional useful purrr-functions are possibly() and quietly():

  #Like safely(), possibly() always succeeds. It’s simpler than safely(), because you give it a default value to return when there is an error.

  x <- list(1, 10, "a")
  
  x %>% map_dbl(possibly(log, NA_real_))

  #Quietly() performs a similar role to safely(), but instead of capturing errors, it captures printed output, messages, and warnings: 
  
  x <- list(1, -1)
  
  x %>% map(quietly(log)) %>% str()

#Mapping over multiple arguments
  
#Instead of mapping along a single input, you often have multiple related inputs that you need iterate along in parallel.
  
#That’s the job of the map2() and pmap() functions. 
  
#Example: you want to simulate some random normals with different means. You know how to do that with map():
  
mu <- list(5, 10, -3)
mu %>% 
  map(rnorm, n = 5) %>% 
  str()

#If you then want to vary the sd, you could do that by iterating over the indices and index into vectors of means and sds:

sigma <- list(1, 5, 10)
seq_along(mu) %>% 
  map(~rnorm(5, mu[[.]], sigma[[.]])) %>% 
  str()

#But that obfuscates the intent of the code. Instead we could use map2() which iterates over two vectors in parallel:

map2(mu, sigma, rnorm, n = 5) %>% str()

#This map2 generates a series of function calls.

#Arguments that vary for each call come before the function; arguments that are the same for every call come after.

#Like map(), map2() is just a wrapper around a for loop:

map2 <- function(x, y, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], y[[i]], ...)
  }
  out
}

#Instead of map3(), map4(), etc., purrr provides pmap(), which takes a list of arguments. You might use that if you wanted to vary the mean, sd, and number of samples:

n <- list(1, 3, 5)
args1 <- list(n, mu, sigma)
args1 %>%
  pmap(rnorm) %>% 
  str()

#If you don’t name the elements of list, pmap() will use positional matching when calling the function. That makes the code harder to read, so it’s better to name the arguments:

args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>% 
  pmap(rnorm) %>% 
  str()

#This makes longer but safer calls.

#And since the arguments are all the same length, it makes sense to store them in a data frame:

params <- tribble(
  ~mean, ~sd, ~n,
  5,     1,  1,
  10,     5,  3,
  -3,    10,  5
)
params %>% 
  pmap(rnorm)

#Invoking different functions

#This is a final step up in complexity: in addition to varying the arguments to the function you might also want to vary the function itself:

f <- c("runif", "rnorm", "rpois")
param <- list(
  list(min = -1, max = 1), 
  list(sd = 5), 
  list(lambda = 10)
)

#We can handle this using invoke_map:

invoke_map(f, param, n = 5) %>% str()

#It's first argument is a list of functions or character vector of function names; the second argument is a list of lists giving the arguments that vary for each function. The subsequent arguments are passed on to every function.

#And again, you can use tribble() to make creating these matching pairs a little easier:
  
sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)

sim %>% 
  mutate(sim = invoke_map(f, params, n = 10))

#Walk

#Walk is an alternative to map that you use when you want to call a function for its side effects, rather than for its return value. 

#You typically do this because you want to render output to the screen or save files to disk.

#Example: 

x <- list(1, "a", 3)

x %>% 
  walk(print)

#Walk functions like walk(), walk2() and pwalk() all invisibly return .x, the first argument. This makes them suitable for use in the middle of pipelines.

#Purrr provides a number of other functions that abstract over other types of for loops:

#1. Predicate functions

  #A number of functions work with predicate functions that return either a single TRUE or FALSE.

  #Keep() and discard() keep elements of the input where the predicate is TRUE or FALSE respectively:

  iris %>% 
    keep(is.factor) %>% 
    str()

  iris %>% 
    discard(is.factor) %>% 
    str()

  #Some() and every() determine if the predicate is true for any or for all of the elements:
  
  x <- list(1:5, letters, list(10))
  
  x %>% 
    some(is_character)

  x %>% 
    every(is_vector)

  #Detect() finds the first element where the predicate is true; detect_index() returns its position:
  
  x <- sample(10)
  
  x

  x %>% 
    detect(~ . > 5)  

  x %>% 
    detect_index(~ . > 5)  

  #Head_while() and tail_while() take elements from the start or end of a vector while a predicate is true: 
  
  x %>% 
    head_while(~ . > 5)

  x %>% 
    tail_while(~ . > 5)

#Reduce and accumulate
  
#Sometimes you have a complex list that you want to reduce to a simple list by repeatedly applying a function that reduces a pair to a singleton. 
  
#This is useful if you want to apply a two-table dplyr verb to multiple tables. 
  
#Example 1: you might have a list of data frames, and you want to reduce to a single data frame by joining the elements together:

dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)
  
dfs %>% reduce(full_join)

#Example 2: you have a list of vectors, and want to find the intersection:

vs <- list(
  c(1, 3, 5, 6, 10),
  c(1, 2, 3, 7, 8, 10),
  c(1, 2, 3, 4, 8, 9, 10)
)

vs %>% reduce(intersect)  

#The reduce function takes a “binary” function (i.e. a function with two primary inputs), and applies it repeatedly to a list until there is only a single element left.

#Accumulate is similar but it keeps all the interim results. You could use it to implement a cumulative sum:

x <- sample(10)

x %>% accumulate(`+`)

#EXERCISES.21.9.3

#Ex1Q: Implement your own version of every() using a for loop. Compare it with purrr::every(). What does purrr’s version do that your version doesn’t?

every2 <- function(.x, .p, ...) {
  for (i in .x) {
    if (!.p(i, ...)) {
      # If any is FALSE we know not all of then were TRUE
      return(FALSE)
    }
  }
  # if nothing was FALSE, then it is TRUE
  TRUE  
}

every2(1:3, function(x) {x > 1})

every2(1:3, function(x) {x > 0})

#Ex1A: The purrr::every() function does fancy things with .p, like taking a logical vector instead of a function, or being able to test part of a string if the elements of .x are lists.

#Ex2Q: Create an enhanced col_summary() that applies a summary function to every numeric column in a data frame.

col_sum2 <- function(df, f, ...) {
  map(keep(df, is.numeric), f, ...)
}
col_sum2(iris, mean)

#Ex3Q: A possible base R equivalent of col_summary() is:

col_sum3 <- function(df, f) {
  is_num <- sapply(df, is.numeric)
  df_num <- df[, is_num]
  
  sapply(df_num, f)
}

#But it has a number of bugs as illustrated with the following inputs:
  
df <- tibble(
  x = 1:3, 
  y = 3:1,
  z = c("a", "b", "c")
)
# OK
col_sum3(df, mean)
# Has problems: don't always return numeric vector
col_sum3(df[1:2], mean)
col_sum3(df[1], mean)
col_sum3(df[0], mean)

#What causes the bugs?

#Ex3A: The problem is that sapply does not always return numeric vectors. If no columns are selected, instead of returning an empty numeric vector, it returns an empty list. This causes an error since we can’t use a list with [.