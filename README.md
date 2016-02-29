# yelp_friends
Predict recommendation of connection

The Python code includes three part:

1. "create_dict_user_business.py"
	This script basically combines the 'business' and 'review' dataset to obtain the 'city' information and 'reviewed business' information associated with "stars" and "review times" for each user.

2. "find_features_per_link.py"
	This script mainly find all the features for each pair of (user i, user j)

3. "graphical_representation.py"
	This script builds the matrix to represent the user, finds its connected components, extract the first and second order neighbors. 
	The main function is included in this script. 
	In the end, the features for each pair (user i, user j) are outputed in the designated file.

The R code performs logistic regression and neural network.

The writeup is in the "Yelp Friends.pdf" file.
