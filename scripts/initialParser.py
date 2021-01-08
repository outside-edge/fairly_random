import json
import re
import csv
import os
from tqdm import tqdm


def structure(filename, data_writer):
    with open(filename, "r") as content:
        text_form = json.load(content)
        for i in range(len(text_form)):
            current = text_form[i]
            rain_info = re.findall("Match State(?:.(?!<))*Rain.*?<", current['postText'], re.IGNORECASE)
            if len(rain_info) == 0:
                continue
            match_state = rain_info[len(rain_info) - 1]
            formatted_version = match_state[:len(match_state) - 1]
            if formatted_version.lower().endswith('rain stopped'):
                continue
            day = re.findall(r'\d+', formatted_version)[0]
            row = [filename, day, str(current['innings']), str(current['over']), formatted_version]
            data_writer.writerow(row)


def main():
    outfile = open('rain_data.csv', 'w')
    data_writer = csv.writer(outfile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
    data_writer.writerow(['filename', 'day', 'innings', 'overs', 'match_state'])
    directory = "comms"
    for filename in tqdm(os.listdir(directory)):
        if filename.endswith(".json"):
            structure(os.path.join(directory, filename), data_writer)


if __name__ == '__main__':
    main()



