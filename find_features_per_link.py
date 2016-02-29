"""
    This scripts find features per link, i.e. per two users;
    features include two degrees, number of 2-path neighbors,
    number of 3-path neighbors, total elite years, whether
    their names are different, total number of fans, whether
    in the same city, total number of common business
"""
#! /usr/bin/env python
from __future__ import print_function
import sys
import scipy.sparse as sps
import numpy as np
from datetime import date
import simplejson as json
import multiprocessing

" get features of other_users_features for user i and j "
" other_users_features has the following format: (elite, name, fans) "
" we use the total number of elite years, an indicator function for their name, "
" and the total number of their fans "
def get_users_features(other_users_features,i,j):
    features_i = other_users_features[i]
    features_j = other_users_features[j]
    elites = len(features_i[0]+features_j[0])
    if features_i[1] == features_j[1]:
        name = [1]
    else:
        name = [0]
    fans = features_i[2] + features_j[2]
    features = [elites]+name+[fans]
    return(features)

" get features of users_busi_dict for user i and j "
" users_busi_dict has the following format: {'i': city, {'busi':avg_star,count}} "
" we get indicator function for I_city, total count of common business for which the difference "
" of average_stars are less than 4 "
def get_users_busi_features(users_busi_dict,i,j):
    if i not in users_busi_dict or j not in users_busi_dict:
        return([0,0])
    else:
        values_i = users_busi_dict[i]
        values_j = users_busi_dict[j]
        # indicator of the city
        if values_i[0] == values_j[0]:
            city = [1]
        else:
            city = [0]
        # total count of shared business and absolute value of stars
        business_i = values_i[1]
        business_j = values_j[1]
        count = 0
        star = 0
        for key_i in business_i.keys():
            if key_i in business_j:
                star_i = business_i[key_i][0]
                star_j = business_j[key_i][0]
                if abs(star_i-star_j)<=4:
                    count += business_i[key_i][1] + business_j[key_i][1]
        return(city+[count])
            
" find features per two nodes, get the following format for pair of (user i, user j) "
" degree_i, degree_j, #2path, #3path, elite, name, fans, I_city, "
" count_of_common_business having the difference of average stars less than 3 "
" output the result in the designated file "
def get_features(rows,spm_sym,square,cubic,other_users_features,users_busi_dict,fout):
    n = spm_sym.shape[1]
    with open(fout,"w") as fw:
        for i in rows:
            for j in range(n):
                if j>i:
                    degree_i = spm_sym.indptr[i+1]-spm_sym.indptr[i]
                    degree_j = spm_sym.indptr[j+1]-spm_sym.indptr[j]
                    square_ij = square[i,j]
                    cubic_ij = cubic[i,j]
                    other_users = get_users_features(other_users_features,i,j)
                    other_users_busi = get_users_busi_features(users_busi_dict,i,j)
                    features=[spm_sym[i,j],i,j,degree_i,degree_j,square_ij,cubic_ij]+other_users+other_users_busi
                    # print out the result
                    for index,element in enumerate(features):
                        if index < len(features)-1:
                            print(element,end=",",file=fw)
                        else:
                            print(element,file=fw)
            print('Finish getting features for row {} !'.format(i),file=sys.stderr)














    
