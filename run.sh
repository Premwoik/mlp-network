#!/bin/bash
echo "Building..."
stack build
echo "Done."
echo "Running..."
stack exec mlp-exe
echo "Done."