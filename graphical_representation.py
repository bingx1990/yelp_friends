"""
    This script reformats the user data; builds the sparse matrix representation restricted in a subsample;
    find the connected components and extract the features for each pair of (user i, user j)
"""
#! /usr/bin/env python
from __future__ import print_function
from functools import partial
import sys
import simplejson as json
import numpy as np
import scipy.sparse as sps
from datetime import date
import find_features_per_link as ffpl
import multiprocessing
import create_dict_user_business as cdub

global today
today = date.today()

" get the number of past days since joining yelp "
def past_day(yelping_since):
    year = int(yelping_since.split('-')[0])
    month = int(yelping_since.split('-')[1])
    return(365*(today.year-year)+30*(today.month-month))

" create dataset, build a dictionary for users and get a p% subsample "
def generate_data(fpath,p):
    users = {}
    users_friends = []
    user_id = 0
    other_related_features = []
    with open(fpath,"r") as fin:
        for lines in fin:
            line = json.loads(lines)
            yelping_since = line["yelping_since"]
            line_friends = line["friends"]
            if past_day(yelping_since) > 365 and len(line_friends)>5:
            # we sreen users who have joined yelp more than one year and have more than 5 friends
                if np.random.binomial(1,p):
                    line_user_id = line["user_id"]
                    keys = ["elite","name","fans"]
                    line_user_other_related = [line[x] for x in keys ]
                    # check whether user is a unique key
                    if line_user_id in users:
                        print ('Repeated user !',file=sys.stderr)
                        return None
                    else:
                        # use integers to encode the user_id
                        users[line_user_id] = user_id
                        users_friends.append([user_id,line_friends])
                        other_related_features.append(line_user_other_related)
                        user_id += 1
    print ('Reading the data is finished.',file=sys.stderr)
    return ([users,users_friends,other_related_features])

" check that all of the friends are listed, encode id by integer "
" and build symmetric sparse matrix to represent the connections "
def build_sparse_matrix(result):
    users = result[0]
    users_friends = result[1]
    unlisted_friend_count = 0
    indices = []
    indptr = [0]
    length = 0
    for i,line in enumerate(users_friends):
        # if there is no friend
        if not line[1]:
            indptr.append(length)
            continue
        else:
            friends = []
            for one_friend in line[1]:
                if one_friend not in users:
                    unlisted_friend_count += 1
                    continue
                else:
                    length += 1
                    friends.append(users[one_friend])
            indices = indices + friends
            indptr.append(length)
        print('Finish user {}'.format(i),file=sys.stderr)
    print ('There are {} friends unlisted in total !'.format(unlisted_friend_count),file=sys.stderr)
    data = np.ones(indptr[-1],dtype=int)
    print ('Start building the sparse matrix ...',file=sys.stderr)
    user_sparse_matrix = sps.csr_matrix((data,indices,indptr),dtype=int,shape=(len(users),len(users)))
    print ('Finish building the sparse matrix !',file=sys.stderr)
    return(user_sparse_matrix)

" define a function to make the sparse matrix symmetric "
def symmetric_sparse_matrix(spm):
    spm_lil = spm.tolil()
    n = spm.shape[0]
    indptr = spm.indptr
    indices = spm.indices
    count = 0
    for i in range(n):
        for j in indices[indptr[i]:indptr[i+1]]:
            if spm_lil[j,i] == 0:
                spm_lil[j,i] = 1
                count += 1
                print(i,j)
    print("Add {} more edges.".format(count),file=sys.stderr)
    return(spm_lil.tocsr())

" define a function to check if there exists a one-way friend connection "                
def check_direction(spm):
    indices = spm.indices
    indptr = spm.indptr
    count = 0
    n = spm.shape[1]
    for i in range(n):
        for j in indices[indptr[i]:indptr[i+1]]:
            if i not in indices[indptr[j]:indptr[j+1]]:
                count += 1
    print ('There are {} one-way links'.format(count),file=sys.stderr)
    
if __name__ == '__main__':
    f_user = "./dataset-examples-master/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_user.json"
    f_busi = "./dataset-examples-master/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json"
    f_review = "./dataset-examples-master/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json"
    fout = "features_per_link.txt"
    # p is the percentage of the subsample, here is 5% 
    result = generate_data(f_user,0.05)
    
    users_dict = result[0]  # get a dictionary of users
    raw_users_busi = cdub.create_dict_user_busi(f_busi,f_review,users_dict) 
    users_busi_dict = cdub.find_city(raw_users_busi[0]) # get a dictionary of users and their business
    other_users_features = result[2]
    
    spm = build_sparse_matrix(result)  # build sparse matrix
    print ("The shape of the user matrix is {}".format(spm.shape),file=sys.stderr)

    # check the existence of one-way edge and make it symmetric (I've already checked that all the edges are two-way)
    # so we make all the one-way edge become two-way
    check_direction(spm)
    spm_sym = symmetric_sparse_matrix(spm)
    check_direction(spm_sym)
    ### second path : A*A
    square = spm_sym*spm_sym
    ### get the diagonal of 2-path matrix
    squareself = sps.csr_matrix.copy(square).tolil()
    squareself.setdiag(np.ones(squareself.shape[0]))
    diagsquare = square - squareself
    ### third path : A^3-A*diag(A^2)-diag(A^2)*A+A
    cubic = square*spm_sym-spm_sym*diagsquare-diagsquare*spm_sym+spm_sym

    # find connected components
    connected_components = sps.csgraph.connected_components(spm_sym,directed=False,return_labels=True)
    print("The number of distinct groups is {}".format(connected_components[0]),file=sys.stderr)

    # find features per link
    N = spm_sym.shape[0]
    ffpl.get_features(range(N),spm_sym,square,cubic,other_users_features,users_busi_dict,fout)

"""
    #### The following is a parallel code, but I don't use it because of the inconvenience for outputing result ####
    k = multiprocessing.cpu_count()
    print ('{} processes are in parallel!'.format(k),file=sys.stderr)
    epochs = np.array_split(range(N),k)
    p = multiprocessing.Pool(k)
    partial_get_features = partial(ffpl.get_features,spm_sym=spm_sym,square=square,cubic=cubic,
        other_users_features=other_users_features,users_busi_dict=users_busi_dict)
    features = p.map(partial_get_features,[x for x in epochs])
    # print out the result
    print("Start outputing the results...",file=sys.stderr)
    with open("features_per_link.txt","wb") as fw:
        for line in features:
            for item in line:
                for i,element in enumerate(item):
                    if i < len(item)-1:
                        print (element, end=',',file=fw)
                    else:
                        print (element, file=fw)
    print("Finish printing out the results.",file=sys.stderr)
    sys.exit()
                
"""


