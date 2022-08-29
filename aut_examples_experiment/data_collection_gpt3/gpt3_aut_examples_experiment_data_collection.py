# data collection with GPT-3 to examine effect of examples on performance

# import libraries
import numpy as np
import csv
from datetime import datetime
import os
import openai

# filename to write data to
csv_file = '../data_gpt3/220829_gpt3_aut_examples_experiment.csv' 

# load API key from environment variable 
openai.api_key = os.getenv("OPENAI_API_KEY")

# fixed parameters to supply each run
max_tokens = 200
n = 1 
engine = "text-davinci-002"

# GPT-3 variables and parameters to optimize
#temperature = np.arange(.4, 1, .1)
#presence_penalty = np.arange(1, 2.1, .5)
#frequency_penalty = np.arange(1, 2.1, .5)
# testing script settings purposes
temperature = np.arange(.6, .71, .1)
presence_penalty = 1
frequency_penalty = 1

# AUT variables
aut_object = ['book', 'fork', 'tin can']
instr_common = 'Think of as many creative uses as you can for a {}. Certainly, there are common, uncreative ways to use a {}. However, for this task, only list creative ideas.'

# AUT instructions per condition TODO update {}
# condition 1 no examples
instr_1 = '' 
# condition 2 Negative uncreative (low bar) examples: uncreative_examples 
instr_2 = 'Examples of some uncreative ideas are: {}. Now, try to come up with your own creative ideas.'
# condition 3 Negative common (high bar) examples: common_examples
instr_3 = 'Examples of some common, uncreative ideas are: {}. Now, try to come up with your own creative ideas.'
# condition 4 Positive common (low bar) examples: common_examples
instr_4 = 'Examples of some common, creative ideas are: {}. Now, try to come up with your own creative ideas.'
# condition 5 Positive creative (high bar) examples: creative_examples
instr_5 = 'Examples of some creative ideas are: {}. Now, try to come up with your own creative ideas.'

instruction = {'instr_1': instr_1, 'instr_2': instr_2, 'instr_3': instr_3, 'instr_4', instr_4, 'instr_5', instr_5}

# AUT examples per object
uncreative_examples = {'book': 'read, learn something, as a notebook', 'fork': 'eat, stir, as a knife', 'tin can': 'store food,  drink from, as a container'}
common_examples = {'book': 'bookend, doorstop, raise computer screen', 'fork': 'poke holes in something, brush hair, as a weapon', 'tin can': 'pen holder, tin can telephone, piggybank'}
creative_examples = {'book': 'roof tile, table tennis racket, dominos', 'fork': 'tent peg, bend it into a candle holder, stick in wall as coat hanger', 'tin can': 'bridge for ants, showerhead, lampshade'} 
examples = {'instr_1': '', 'instr_2': uncreative_examples, 'instr_3': common_examples, 'instr_4': common_examples, 'instr_5': creative_examples}

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
header = ['id', 'gpt3_id', 'gpt3_created', 'gpt3_model', 'engine', 'timestamp', 'temperature', 'presence_penalty', 'frequency_penalty', 'instr_nr', 'instr_text', 'aut_object', 'gpt3_response']
writer.writerow(header)

# loop through all variables / parameters of the experiment
rowid = 0
for temp in temperature:
    for pres_pnlt in presence_penalty:
        for freq_pnlt in frequency_penalty:
            for aut_obj in aut_object:
                for instr_key, instr_val in instruction.items(): # TODO add conditions here
                    instr = instr_common.format(aut_obj) + instr_val.format(examples[instr_key[aut_obj]]) 
                    response = gpt3_response(engine, instr, max_tokens, temp, pres_pnlt, freq_pnlt, n)
                    rowid = rowid + 1
                    gpt3_response_text = response['choices'][0]['text']
                    gpt3_id = response['id']
                    gpt3_created = response['created']
                    gpt3_model = response['model']
                    row = [rowid, gpt3_id, gpt3_created, gpt3_model, engine, datetime.now(), temp, pres_pnlt, freq_pnlt, instr_key, instr, aut_obj, gpt3_response_text]
                    writer.writerow(row)


# close csv writer and file
f.close()
