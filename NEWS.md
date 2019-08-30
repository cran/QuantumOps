# QuantumOps 2.5.3
## Major changes
QuantumClassifier is a new function which is very similar to QuantumMNIST256Classifier. This new version allows for the specification of the number of qubits used, the depth of the circuit, and the ranges of the controlled gates for each block. 

controlled function is now more general, allowing for the control and target qubits to be anywhere in the input ket. Functionality is exactly the same as cntrld function, except inputs are specified in a vector rather than a list, which fits R methodology

testGate is a new function that takes an input gate and tries different inputs and displays the output. Useful for testing user defined gates.



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

