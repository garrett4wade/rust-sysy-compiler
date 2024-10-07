#!/bin/bash
current_dir=$(basename "$PWD")
if [$current_dir != "rust-sysy-compiler"]; then
    echo "Please run this script in the root directory of the project."
    exit 1
fi

docker run -it --rm -v $PWD:/root/compiler maxxing/compiler-dev \
    autotest -riscv -s lv1 /root/compiler