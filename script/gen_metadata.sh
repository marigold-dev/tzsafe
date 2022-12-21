#!/bin/bash

echo '
{
    "name": "multisig",
    "description": "Marigold Multisig Contract",
    "version": "'${VERSION}'",
    "license": {
        "name": "MIT"
    },
    "authors": [
        "Marigold <contract@marigold.dev>"
    ],
    "homepage": "https://marigold.dev",
    "source": {
        "tools": "cameligo",
        "location": "https://github.com/marigold-dev/multisig/"
    },
    "interfaces": [
        "TZIP-016"
    ]
}'
