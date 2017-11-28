# Find all the potential trees
groups <- table(Image_label)

# Subset to the reasonable ones
groups <- groups[groups < 100 & groups > 50]

# Do the work
groups.we.want <- unique(names(groups))
for(i in seq(1, length(groups.we.want))){
    # Subset our Image_label to the group we want
    # Grab the rows of this group
    #... using row(Image_label)
    # Grab the cols of this group
    #... using col(Image_label)
    # Calculate their `mean`s
    # Store somehow
    #...you're done!
}
