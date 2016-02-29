"""
    build a dictionary with the following structure "
    {'user_id': [city, {'business_1_id': (ave_star, count)},
                       {'business_2_id': (ave_star, count)}]}
    eg:
    {'0': ["Bellevue",{"Apple": 3.54, 1}]}
"""

from __future__ import print_function
import sys
import simplejson as json

def create_dict_user_busi(fpath1,fpath2,users_dict):
    # fpath1 denotes the business dataset, fpath2 denotes the review one
    users_busi_dict = {}
    busi_dict = {}
    print("Start reading the business data...",file=sys.stderr)
    with open(fpath1,"r") as fr1:
        for lines in fr1:
            line = json.loads(lines)
            city = line["city"]
            bid = line["business_id"]
            busi_dict[bid] = city
    print("Finish reading the business data...",file=sys.stderr)
    print("Start reading the review data...",file=sys.stderr)
    with open(fpath2,"r") as fr2:
        for lines in fr2:
            line = json.loads(lines)
            bid = line["business_id"]
            uid = line["user_id"]
            star = line["stars"]
            if uid not in users_dict:
            # if uid is not in the total user list, then continue
                continue
            else:
                uid_int = users_dict[uid]
                if uid in users_busi_dict:
                # if uid has already existed in users_busi_dict, then add this to its values
                    values = users_busi_dict[uid]
                    if bid in values:
                    # if bid has already existed, then re-calculate the star and count
                        old_star = values[bid][0]
                        count = values[bid][1] + 1
                        city = values[bid][2]
                        new_star = (old_star*(count-1) + star)/count
                        values[bid] = [new_star,count,city]
                        users_busi_dict[uid_int] = values
                    else:
                    # if not, add a new one
                        city = busi_dict[bid]
                        values[bid] = [star,1,city]
                else:
                # create a new one
                    city = busi_dict[bid]
                    users_busi_dict[uid_int] = {bid:[star,1,city]}
    print("Finish reading the review data...",file=sys.stderr)
    return ([users_busi_dict,busi_dict])
        
" function to find the most possible city for each user by choosing the city of the business they visit most"
def find_city(users_busi_dict):
    for key in users_busi_dict.keys():
        values = users_busi_dict[key]
        count = 0
        city = None
        new_values = {}
        for busi in values.keys():
                new_values[busi] = values[busi][:2]
                new_count = values[busi][1]
                if new_count > count:
                        city = values[busi][2]
                        count = new_count
        users_busi_dict[key] = [city,new_values]
    return(users_busi_dict)
