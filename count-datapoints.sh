#!/bin/sh

echo "# of lines in [$2, $3]: "
sed -n '/'"$2"'/','/'"$3"'/p' $1 | wc -l