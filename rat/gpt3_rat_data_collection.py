# Monte-Carlo experiment with GPT-3 on the AUT paperclip and book tasks
# aim is to optimize parameters and instructions for optimal AUT performance by GPT-3
# this script is to gather the data from gpt-3
# then raters score each response with snapshot scoring method
# we'll analyze the data to decide which instructions and the paramater settings are best 

# import libraries
import numpy as np
import csv
from datetime import datetime
import os
import openai

# filename to write data to
csv_file = 'data/220422_gpt3_rat.csv' 

# load API key from environment variable 
openai.api_key = os.getenv("OPENAI_API_KEY")

# fixed parameters to supply each run
max_tokens = 32
n = 1 
engine = "text-davinci-002"

# GPT-3 variables and parameters to optimize
temperature = .3
presence_penalty = 0
frequency_penalty = 0

# instructions
instr_common = ''
instr_item = ['', 
              '',
              '',
              '',
              '',
              '',
              '',
              '',
              '',
              '',
              '',
              '',
              '',
              ]

# function to call gpt3 api with instr and parameter settings
def gpt3_response(eng, instr, max_tok, temp, pres_pnty, freq_pnty, num):
    response = openai.Completion.create(
        engine=eng, 
        prompt=instr, 
        max_tokens=max_tok,
        temperature=pres_pnty,
        presence_penalty=freq_pnty,
        frequency_penalty=freq_pnty,
        n=num)
    return(response)

# open csv file to write data to
f = open(csv_file, 'w')
# create csv writer
writer = csv.writer(f)
# write header to csv file
header = ['id', 'gpt3_id', 'gpt3_created', 'gpt3_model', 'engine', 'timestamp', 'temperature', 'presence_penalty', 'frequency_penalty', 'instr_text', 'rat_item', 'gpt3_response']
writer.writerow(header)

# loop through rat items
rowid = 0
for rat_item in rat_items:
  instr = instr_common + instr_item.format(rat_item) 
  #response = gpt3_response(engine, instr, max_tokens, temp, pres_pnlt, freq_pnlt, n)
  rowid = rowid + 1
  gpt3_response_text = 'response' #response['choices'][0]['text']
  gpt3_id = rowid #response['id']
  gpt3_created = datetime.now() #response['created']
  gpt3_model = engine #response['model']
  row = [rowid, gpt3_id, gpt3_created, gpt3_model, engine, datetime.now(), temp, pres_pnlt, freq_pnlt, instr, rat_item, gpt3_response_text]
  writer.writerow(row)


# close csv writer and file
f.close()
