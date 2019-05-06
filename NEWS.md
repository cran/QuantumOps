# QuantumOps 2.5.2
## Major changes
New functions for Steane encoding and error correction

New function to perform the swap test to encode the inner product of two quantum states into an ancilla qubit

measure and reduceMeasure now can index from left to right, instead of right to left. This is to be consistent with indexing in other functions. A new optional boolean argument, l2r, will enable this

New functions to convert between decimal and binary format which is useful for some algorithms

New function singleSWAP implements same functionality as function single, but for 2-qubit SWAP gate, allowing for swaps between two qubits in a multi-qubit state

New function compareQuantumState which creates an oracle to flip a qubit if two quantum states are the same

intket expanded to allow for specification of multiple integer encoded basis states and arbitrary relative amplitudes for each

## Bug fixes
Major crash in cntrld function fixed

Purely imaginary coefficients now always show in dirac function

