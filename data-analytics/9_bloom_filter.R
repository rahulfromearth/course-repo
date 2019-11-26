library(digest) # https://cran.r-project.org/web/packages/digest/digest.pdf
library(bit)    # https://cran.r-project.org/web/packages/bit/bit.pdf

# https://llimllib.github.io/bloomfilter-tutorial/
# https://github.com/rstudio/shiny-examples/blob/master/087-crandash/bloomfilter.R

# https://www.geeksforgeeks.org/bloom-filters-introduction-and-python-implementation/
# Translation of above python script

BloomFilter <- setRefClass("BloomFilter",
                           fields = list(
                             .fp_prob = "numeric",
                             .size = "integer",
                             .hash_count = "integer",
                             .bit_array = "ANY"
                           ),
                           methods = list(
                             initialize = function(items_count, fp_prob) {
                               .fp_prob    <<- fp_prob
                               .size       <<- get_size(items_count, fp_prob)
                               .hash_count <<- get_hash_count(.size, items_count)
                               .bit_array  <<- bit(.size)
                             },
                             get_size = function(n, p) {
                               m = -(n * log(p)) / (log(2)^2)
                               return (as.integer(m))
                             },
                             get_hash_count = function(m, n) {
                               k = (m/n) * log(2)
                               return (as.integer(k)) 
                             },
                             add = function(item) {
                               for (i in 1:.hash_count) {
                                 hash_digest = get_hash(item, i)
                                 .bit_array[hash_digest] <<- TRUE
                               }
                             },
                             check = function(item) {
                               for (i in 1:.hash_count) {
                                 hash_digest = get_hash(item, i)
                                 if (.bit_array[hash_digest] == FALSE) {
                                   return (FALSE)
                                 }
                               }
                               return (TRUE)
                             },
                             get_hash = function(item, seed) {
                               hex_str = digest(object = item,
                                                algo = "murmur32",
                                                serialize = F,
                                                seed = seed)
                               hex = paste('0x', hex_str, sep = "")
                               # strtoi overflows with integers larger than 2^31 (NA)
                               as.numeric(hex) %% .size 
                             }
                           )
)

n = 20
p = 0.05

bloomf = BloomFilter$new(n, p)

sprintf("Size of bit array:%s", bloomf$.size)
sprintf("False positive Probability:%s", bloomf$.fp_prob)
sprintf("Number of hash functions:%s", bloomf$.hash_count)

word_present = c('abound','abounds','abundance','abundant','accessable',
				'bloom','blossom','bolster','bonny','bonus','bonuses',
				'coherent','cohesive','colorful','comely','comfort',
				'gems','generosity','generous','generously','genial')

word_absent = c('bluff','cheater','hate','war','humanity',
			'racism','hurt','nuke','gloomy','facebook',
			'geeksforgeeks','twitter')

for (item in word_present) {
	bloomf$add(item)
}

set.seed(42)

word_present = sample(word_present)
word_absent  = sample(word_absent)

test_words = sample( c(word_present[1:10], word_absent) )

# sprintf doesn't print in non-interactive mode
output <- function(...) { cat(sprintf(...)) }

for ( word in test_words ) {
	if ( bloomf$check(word) ) {
		if (word %in% word_absent) {
			output("'%s' is a false positive!\n", word)
		}
		else {
		  output("'%s' is probably present!\n", word)
		}
	}
	else {
	  output("'%s' is definitely not present!\n", word)
	}
}
