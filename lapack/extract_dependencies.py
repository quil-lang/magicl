""" 
Usage: python extract_depencies.py <fortran-files>...

Extracts Fortran 'EXTERNAL' functions / subroutines from the specified files.
This only reports on those routines seen in this script's execution, e.g. this
is suitable for extracting all inter-LAPACK dependencies while ignoring BLAS
dependencies (by calling this with only LAPACK sources). 
"""

import re, sys, os
from pathlib import Path

ex = re.compile(r"\s+EXTERNAL\s+(\w+)")
more = re.compile(r"\s+\$\s+(\w+)")


def external_callees(line: str) -> list:
    """Get all external callees from an EXTERNAL statement (on this line)."""
    match = ex.match(line)
    if match:
        tokens = [s.replace(',','').lower() for s in line.split()[1:]]
        return tokens
    else:
        return []


def continuing_callees(line: str) -> list:
    """Get all external callees from lines following an EXTERNAL statement."""
    match = more.match(line)
    if match:
        tokens = [s.replace(',','').lower() for s in line.split()[1:]]
        return tokens
    else:
        return []

callees = {}

for path in sys.argv[1:]:
    if not (os.path.isfile(path) and path.endswith('.f')):
        continue
    
    with open(path, 'r') as f:
        current = Path(path).stem.lower()
        callees[current] = extern = set({})

        line = f.readline()
        while line:
            results = external_callees(line)
            if results:
                line = f.readline()
                while line and continuing_callees(line):
                    results += continuing_callees(line)
                    line = f.readline()
                for r in results:
                    extern.add(r)
            else:
                line = f.readline()

for name in callees:
    print(f'({name}',end='')
    for callee in callees[name]:
        # exclude non lapack functions / internal fns
        if callee in callees:
            print(' ', callee, sep='', end='')
    print(')')

    
