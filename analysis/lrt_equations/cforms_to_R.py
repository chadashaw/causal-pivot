#!/usr/bin/env python3

import fileinput

# sed - i 's/Omega/omega/g' $input_file
# sed - i 's/Alpha/alpha/g' $input_file
# sed - i 's/Beta/beta/g' $input_file
# sed - i 's/Gamma/gamma/g' $input_file
# sed - i 's/Eta/eta/g' $input_file
# sed - i 's/Power/raise.power/g' $input_file
# sed - i 's/E/exp(1)/g' $input_file

terms = {
    "ω": "omega",
    "α": "alpha",
    "β": "beta",
    "γ": "gamma",
    "η": "eta",
    "ζ": "zeta",
    "κ": "kappa",
    "X": "X",
    "A": "A",
}

functions = {
    "Power": "raise.power",
    "E": "exp(1)"
}

functionDefsR = []

for line in fileinput.input():
    fname, fdef = line.split('=')
    functionDefR = fname.strip() + " <- function("
    termsFound = []
    for term, termReplacement in terms.items():
        if fdef.find(term) < 0:
            continue
        termsFound.append(termReplacement)
        fdef = fdef.replace(term, termReplacement)
    for function, functionReplacement in functions.items():
        fdef = fdef.replace(function, functionReplacement)

    functionDefR += ', '.join(termsFound) + ')'
    functionDefR += ' {\n\t' + fdef.strip() + '\n}'
    functionDefsR.append(functionDefR)

print('\n\n'.join(functionDefsR))
