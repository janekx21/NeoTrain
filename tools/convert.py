import csv
import random

set = [*"asdfjklöeruiqwopghtzvncmyb"]
must = [*"yb"]

result = []
with open('german.dic', newline='') as csvfile:
    spamreader = csv.reader(csvfile, delimiter=' ', quotechar='|')
    for row in spamreader:
        for char in [*row[0]]:
            if char not in set:
                break
        else:
            for item in must:
                if item not in [*row[0]]:
                    break
            else:
                result.append(row[0])


random.shuffle(result)
print(" ".join(result[:20]))
