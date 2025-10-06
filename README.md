# eBPF Software fault isolation

## Testing

The testing can be run by running `cabal test` in the root directory.

## Running the program

It can run on assembly files by running `cabal run ebpf-sfi -- <path to input
file> <path to output>`. This runs the ebpf sfi algorithm on the input file and
creates the safe program at path to output file.
