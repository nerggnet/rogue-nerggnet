#!/usr/bin/env bash

# This is the output file where we'll store all the source code.
OUTFILE="compiled_source.md"

# Clear or create the file (if it doesn't exist).
> "$OUTFILE"

# Find all .hs files in 'app' and 'src' directories.
FILES=$(find app src -type f -name "*.hs")

echo "Compiling Haskell files into '$OUTFILE'..."

# Loop through each file, append its content to the output file
# with triple backticks around the file content.
for file in $FILES; do
  echo "Processing $file ..."
  echo '```haskell' >> "$OUTFILE"
  cat "$file" >> "$OUTFILE"
  echo '```' >> "$OUTFILE"
  echo >> "$OUTFILE"   # Extra blank line for readability
done

echo "Done! All .hs files have been written to '$OUTFILE'."
