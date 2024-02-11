# 2. Add two numbers: https://leetcode.com/problems/add-two-numbers/
add_two_nums <- function (l1, l2) {
  
  # reverse order and then turn into integer
  l1_new <- as.integer(paste(rev(l1), collapse = ""))
  l2_new <- as.integer(paste(rev(l2), collapse = ""))
  
  # calculate the result and then turn it into a string
  l3 = l1_new + l2_new
  l3_new <- paste(l3)
  
  # convert string into an array and then change the chars to ints
  l3_array <- strsplit(l3_new, "")[[1]]
  l3_int_array <- as.integer(unlist(l3_array))
  
  # lastly, reverse it and there's the answer
  l3_final <- rev(l3_int_array)
  l3_final
  
}
#add_two_nums(c(3,8,1), c(3, 6, 2))
#add_two_nums(c(2,4,3), c(5,6,4))
#add_two_nums(c(9,9,9,9,9,9,9), c(9,9,9,9))





# 3. Longest Substring Without Repeating Characters: 
# https://leetcode.com/problems/longest-substring-without-repeating-characters/description/
longest_substring_no_repeat <- function(s) {
  
  # longest substring
  longest_subtring = ''
  
  # go through all possible combinations
  for (i in 1:nchar(s)) {
    for (j in i:nchar(s)) {
      
      # get substring
      s_substring = substr(s, i, j)
      
      # strip s_substring of any duplicate letters
      unique_chars <- unique(strsplit(s_substring, "")[[1]])
      unique_string <- paste(unique_chars, collapse = "")
      
      # check if two strings are the same
      # and if the substring's len is >= len of longest_subtring
      if (s_substring == unique_string &
          nchar(s_substring) >= nchar(longest_subtring)) {
        
        # if yes to both, save this substring
        longest_subtring = s_substring;
      }
      
    }
  }
  
  # show the result
  nchar(longest_subtring)
  
}
#longest_substring_no_repeat('abcabcbb')





# 4. Median of Two Sorted Arrays
# https://leetcode.com/problems/median-of-two-sorted-arrays/
median_two_arrays <- function(a1, a2) {
  
  # first combine the two arrays
  a3 <- sort(append(a1, a2))
  
  # now find the median based on whether
  # there are even or odd number of elements
  if (length(a3)%%2 == 0) {
    ele_index_1 = length(a3)/2
    ele_index_2 = ele_index_1+1
    median <- (a3[ele_index_1] + a3[ele_index_2])/2
  }
  else if (length(a3)%%2 == 1) {
    ele_index = length(a3) - (length(a3)-1)/2
    median <- a3[ele_index]
  }
  
  # show median
  median
  
}
#median_two_arrays(c(1,3), c(2,4,5,6))





# 5. Longest Palindromic Substring: https://leetcode.com/problems/longest-palindromic-substring/
longest_palindromic_string <- function(s) {
  
  # longest palindromic substring
  lng_pa_sub = ''
  
  # go through all possible combinations
  for (i in 1:nchar(s)-1) {
    for (j in (i+1):nchar(s)) {
      
      # get substring and its reverse
      s_substring = substr(s, i, j)
      s_substring_rev = paste(rev(strsplit(s_substring, "")[[1]]), collapse="")
      
      # if they match and length is >= current lng_pa_sub
      if (s_substring == s_substring_rev &
          nchar(s_substring >= nchar(lng_pa_sub))) {
        lng_pa_sub = s_substring
      }
      
    }
  }
  
  # display final result
  lng_pa_sub
  
}
#longest_palindromic_string('babad')





# 6. Zigzag Conversion: https://leetcode.com/problems/zigzag-conversion/
zigzag_conversion <- function(s, numRows) {
  
  # combo list
  combo = list()
  
  # place vector
  p = c(1, 1)
  
  # stage
  stage = 1
  
  # iterating the place vector
  for (i in 1:nchar(s)) {
    
    # append p into combo
    new_list <- list(p[1], p[2], substr(s, i, i))
    combo <- append(combo, list(new_list))
    
    # if stage is 1, move down
    if (stage == 1) {
      p <- p + c(1, 0)
      if (p[1] == numRows) { # when we reach bottom
        stage <- 2
      }
    }
    
    # if stage is 2, move right and up
    else if (stage == 2) {
      p <- p + c(-1, 1)
      if (p[1] == 1) { # when we reach the top
        stage <- 1
      }
    }
    
  }
  
  # order by the first ele of each sublist (the row)
  ordered_list <- combo[order(sapply(combo, function(x) x[[1]]))]
  
  # go thru ordered_list and extract the final_string
  final_string <- ''
  for (i in 1:length(ordered_list)) {
    final_string <- paste0(final_string, ordered_list[[i]][[3]])
  }
  final_string
  
}
#zigzag_conversion('PAYPALISHIRING', 3)





# 7. Reverse Integer: https://leetcode.com/problems/reverse-integer/
reverse_integer <- function(x) {
  x <- as.integer(paste(rev(strsplit(as.character(x), "")[[1]]), collapse = ""))
  x
}
#reverse_integer(558371)





# 9. Palindrome Number: https://leetcode.com/problems/palindrome-number/
palin_num <- function(x) {
  y <- as.integer(paste(rev(strsplit(as.character(x), "")[[1]]), collapse = ""))
  x == y
}
#palin_num(5885)





# 11. Container With Most Water: https://leetcode.com/problems/container-with-most-water/
rect_most_water <- function(height) {
  
  # add the x coord to each height point
  coords <- list()
  for (i in 1:length(height)) {
    sub_list <- c(i, height[i])
    coords <- append(coords, list(sub_list))
  }
  
  # save all combos
  largest_area <- 0
  for (i in 1:(length(coords)-1)) {
    for (j in (i+1):length(coords)) {
      width <- j-i
      height_min <- min(c(coords[[j]][[2]], coords[[i]][[2]]))
      area <- width*height_min
      if (area >= largest_area) {
        largest_area <- area
        solution <- list(area, width, height_min, i, j, "\n")
      }
    }
  }
  
  # show the result
  cat(largest_area, "\n", paste(solution))
  
}
#rect_most_water(c(1,8,6,2,5,4,8,3,7))





# 13. Roman to Integer: https://leetcode.com/problems/roman-to-integer/
roman_to_int <- function(s) {
  
  # library to account occurrences of substring
  library(stringr)
  
  # create an array with values to replace
  replace_array = list(
                        list('IV', 4),
                        list('IV', 9),
                        list('XL', 40),
                        list('XC', 90),
                        list('CD', 400),
                        list('CM', 900),
                        list('I', 1),
                        list('V', 5),
                        list('X', 10),
                        list('L', 50),
                        list('C', 100),
                        list('D', 500),
                        list('M', 1000)
  )
  
  # create array to sum values
  arr_to_sum = c()
  
  # see if each of the values is present
  for (i in replace_array) {
    
    # roman numeral to check
    rome_num <- i[[1]]
    
    # Check if present
    is_rome_num_pres <- grepl(rome_num, s)
    
    # Count the number of times the substring appears in the string
    count <- str_count(s, rome_num)
    
    # If pres...
    if (is_rome_num_pres) {
      
      # Replace roman numeral with nothing
      s <- gsub(rome_num, "", s)
      
      # Append the number value
      for (j in 1:count) {
        arr_to_sum <- append(arr_to_sum, i[[2]])
      }
      
    }
    
  }
  
  # Sum array and show output
  output <- sum(arr_to_sum)
  output
  
}
#roman_to_int('MCMXCIV')





# 14. Longest Common Prefix: https://leetcode.com/problems/longest-common-prefix/
longest_common_prefix <- function(strs) {
  
  # convert each string to a list
  # find the longest string too
  strs_array <- list()
  max_word_length <- 0
  for (i in strs) {
    sub_list <- strsplit(i, "")
    if (nchar(i) > max_word_length) {
      max_word_length <- nchar(i)
    }
    strs_array <- append(strs_array, sub_list)
  }
  
  # Longest Common Prefix
  lcp <- c()
  
  # iterate through i letters
  for (i in 1:max_word_length) {
    
    # get the ith letter of each of the j words
    ith_letter <- c()
    for (j in 1:length(strs_array)) {
      ith_letter <- append(ith_letter, strs_array[[j]][[i]])
    }
    
    # now let's check if all eles are the same
    counter <- 0
    for (k in 1:length(ith_letter)) {
      if (ith_letter[[1]] == ith_letter[[k]]) {
        counter <- counter + 1
      }
    }
    
    # store letter if they all match
    if (counter == length(ith_letter)) {
      lcp <- append(lcp, ith_letter[[1]])
    }
    
    # if they don't match, break the for loop
    else {
     break 
    }
    
  }
  
  # return the result
  paste(lcp, collapse="")
  
}
longest_common_prefix(c("flower","flow","flight"))





# 15. 3Sum: https://leetcode.com/problems/3sum/
three_sum <- function(nums, target) {
  
  # store the results
  triplets <- list()
  
  # all possible combos
  for (i in nums) {
    for (j in nums) {
      for (k in nums) {
        
        #calculate sum
        sum = i + j + k
        
        #check if sum matches target
        if (sum == target) {
          
          # create combo as vector
          combo <- sort(c(i, j, k))
          
          # only append if it's not already in triplets
          if (!any(sapply(triplets, identical, combo))) {
            triplets <- append(triplets, list(combo))
          }
          
        }
        
      }
    }
  }
  
  # show the result
  for (i in triplets) {
    list_as_string <- paste(i, collapse = " ")
    print(list_as_string)
  }
  
}
#three_sum(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 8)





# 16. 3Sum Closest: https://leetcode.com/problems/3sum-closest/
three_sum_closest <- function(nums, target) {
  
  # create all combos
  combos <- list()
  for (i in 1:(length(nums)-2)) {
    for (j in (i+1):(length(nums)-1)) {
      for (k in (j+1):length(nums)) {
        
        # create a sublist of the nums
        sublist <- c(nums[[i]], nums[[j]], nums[[k]])
        
        # find their sum
        sum <- sum(sublist)
        
        # find the diff as an abs value
        abs_targ_diff <- abs(sum-target)
        
        # create the final sublist
        final_sublist <- list(abs_targ_diff, sum, sublist)
        
        # append this result
        combos <- append(combos, list(final_sublist))
        
      }
    }
  }
  
  # now arrange the sublists by the first element,
  # the absolute value of the diff btw target and sum
  combos <- combos[order(sapply(combos,'[[',1))]
  
  # the solution is the sum (second element)
  # of the first sublist
  #print(combos)
  combos[[1]][[2]]
  
}
#three_sum_closest(c(-1,2,1,-4), 1)
