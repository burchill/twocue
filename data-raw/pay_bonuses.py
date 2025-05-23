#!/usr/bin/env python3

# Copyright (c) 2012-2018 Andrew Watts and the University of Rochester BCS Department
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

"""
Pays out bonuses to Amazon Mechanical Turk workers. For Exp4, it should be $6.50
"""

import argparse

from csv import DictReader, DictWriter
from functools import reduce
from operator import add

import boto3

from botocore.exceptions import ClientError

__author__ = 'Andrew Watts <awatts2@ur.rochester.edu>'

parser = argparse.ArgumentParser(description='Grant bonuses for HITs on Amazon Mechanical Turk')
parser.add_argument('-f', required=True,
                    help='(required) The name of the CSV file you are using')
parser.add_argument('-a', required=True,
                    help='(required) The amount of money per bonus. $7.00 mostly')
parser.add_argument('-o', required=True,
                    help='(required) The name of the CSV file you\'re using to keep track of workers who already got paid')
parser.add_argument('-p', '--profile',
                    help='Run commands using specific aws credentials rather the default. To set-up alternative credentials see http://boto3.readthedocs.org/en/latest/guide/configuration.html#shared-credentials-file')
args = parser.parse_args()

# Only region w/ MTurk endpoint currently is us-east-1
region = 'us-east-1'
endpoint = f'https://mturk-requester.{region}.amazonaws.com'
# If you want to use profiles, you have to create a Session with one before connecting a client
session = boto3.Session(profile_name=args.profile)

mtc = session.client('mturk', endpoint_url=endpoint, region_name=region)

try:
    available_balance = float(mtc.get_account_balance().get('AvailableBalance', 0.0))
    print(f'Available balance: ${available_balance}')
except ClientError as e:
    print(e)

# Makes the list of bonuses
bonus_list = []
with open(args.f, 'r') as csvinfile:
    bonuses = DictReader(csvinfile)
    for row in bonuses:
        bonus_list.append(row)

bonus_sum = reduce(add, [float(args.a) for x in bonus_list])
print(bonus_sum)

# Saves a backup of the old file      
old_rows = []
with open(args.o, 'r') as csvofile:
    old_dict = DictReader(csvofile)
    for row in old_dict:
        old_rows.append(row)
with open(args.o+".backup", 'w') as backup_file:
    writer = DictWriter(backup_file, fieldnames=['worker','assignment'])
    writer.writeheader()
    writer.writerows(bonus_list)
    
double_worker_list =[]
had_error = False
if bonus_sum > available_balance:
    print(f'Insufficient funds (${available_balance:.2f}) to pay bonuses (${bonus_sum:.2f})! Add ${bonus_sum - available_balance :.2f} to your account before proceeding')
    had_error=True
else:
    for row in bonus_list:
        worker_id = row['worker']
        assignment_id = row['assignment']
        if worker_id not in double_worker_list:
            double_worker_list += [worker_id]
            try:
                mtc.send_bonus(
                    WorkerId=worker_id,
                    BonusAmount=args.a,
                    AssignmentId=assignment_id,
                    Reason=f'For qualifying and finishing Experiment 2'
                )
                print("Gave bonus to "+worker_id+" for "+assignment_id)
            except ClientError as e:
                print("Error for "+worker_id+" with "+assignment_id)
                had_error = True
                print(e)  

if had_error:
    print("Due to messup, the old CSV file was not modified!")
else:
    with open(args.o, 'w') as csvofile:
        writer = DictWriter(csvofile, fieldnames=['worker','assignment'])
        writer.writeheader()
        writer.writerows(old_rows)
        writer.writerows(bonus_list)
    print("wrote to "+args.o+"!")
            
print("Done!")
print("REMEMBER: DELETE UNPAID BONUS FILE IF EVERYTHING WORKED")
try:
    available_balance = float(mtc.get_account_balance().get('AvailableBalance', 0.0))
    print(f'Available balance: ${available_balance}')
except ClientError as e:
    print(e)

