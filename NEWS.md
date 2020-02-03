# QuantumOps 3.0.1
##Major changes
Phase Damping has been added as a noise model

## Bug fixes
Incorrect application of the QFT in Shors algorithm has been fixed. It should only be applied to the first register of qubits, and had been applied to both registers. 


# QuantumOps 3.0.0
## Major changes
Functionality has been added for simulating operations by cycle, where cycle means the a single operation applied to a register of qubits. The general form is a list. For example, a 5-cycle operation on 2 qubits will be a list of length 5, where each entry is a 4x4 matrix representing the operation for that cycle. 

Density matrix representation has been added, which allows for more accurate simulation. convert\_ket2DM is a function that converts kets to density matrix format. opDM is a function that performs operations with density matrices. 

Noise models, including stochastic Pauli noise, Coherent noise, and Amplitude Damping, have been added which can be applied to density matrices.

Automated Randomized Compiling has been added, which can serve as a noise mitigation technique.

QuantumOps can now use the gridsynth gate decomposition algorithm. The decomposition itself has NOT been implemented, but required the binary file which has been provided at https://www.mathstat.dal.ca/~selinger/newsynth/. QuantumOps can be used to generate input and collect output from the binary. Additionally, QuantumOps uses quantum circuits from "A Meet-in-the-Middle Algorithm for Fast Synthesis of Depth-Optimal Quantum Circuits" (Amy 2013) to generate controlled 2-qubit versions of the decomposed gates.

## Minor changes
QAOA, QuantumClassifier, and QFT have been augmented with the ability to return the circuit by cycle. Meaning that, instead of a single matrix, they can return a list of matrices which represent the operation at each cycle (time step). This enables use with RandomizeCompile and the different error models

## Bug fixes
QuantumClassifier has parameter updates in the wrong direction! They have now been flipped to be correct

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

